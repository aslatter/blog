{-# LANGUAGE OverloadedStrings #-}

{-| This module pulls in all of the
templates defined in other modules and
packs them into the template state.

It also contains helpers for working
with the templates.

-}

module Blog.Templates
    ( templateReloader
    , render
    , renderBlaze
    , renderWithText
    , initTemplates
    , appNotFound
    ) where

import Blog.Core
import Blog.Markdown

import Blaze.ByteString.Builder (toLazyByteString)
import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.Text (Text)
import Happstack.Server
    ( Response, toResponse, setResponseCode, toResponseBS
    , internalServerError )
import qualified Happstack.Server.Heist as H
import Text.Blaze (Html)
import Text.Blaze.Renderer.XmlHtml (renderHtmlNodes)
import Text.Templating.Heist
import Text.Templating.Heist.TemplateDirectory
import Text.XmlHtml (Node(..))

-- | Given the name of a template, serve it up.
render :: ByteString -> App Response
render name = do
  ts <- appTemplate
  renderInternal ts name


renderInternal :: TemplateState App -> ByteString -> App Response
renderInternal ts name = do
  result <- renderTemplate ts name
  case result of
    Nothing -> internalServerError $ toResponse ("Sorry, there was a server error!" :: String)
    Just (builder, mimeType) ->
        return $ toResponseBS mimeType $ toLazyByteString builder

-- | Call a template in a modified environment.
renderWith :: (TemplateState App -> TemplateState App) -> ByteString -> App Response
renderWith f name = do
  ts <- appTemplate
  renderInternal (f ts) name

-- | Call a template with the given arguments.
renderWithText :: [(Text, Text)] -> ByteString -> App Response
renderWithText splices =
    let splices' = map fn splices
        fn (key, val) = (key, textSplice val)
    in renderWith (bindSplices splices')

-- | Call a template with the given markup spliced into the
-- "content" tag.
renderBlaze :: [(Text, Text)] -> ByteString -> Html -> App Response
renderBlaze splices template content =
    let contentSplice = return $ renderHtmlNodes content
        splices' = ("content", contentSplice) : map fn splices
        fn (key, val) = (key, textSplice val)
    in renderWith (bindSplices splices') template

-- | Ping this handler to refresh the templates from disk.
templateReloader :: App Response
templateReloader =
    appTemplateDirectory >>= H.templateReloader

-- | Build the initial template state information, given
-- the path to the templates folder.
initTemplates :: MonadIO m => FilePath -> IO (TemplateDirectory m)
initTemplates templateDir =
    let ts = bindSplices appSplices emptyTemplateState
    in newTemplateDirectory' templateDir ts

-- | Default splices we make available.
appSplices :: Monad m => [(Text, Splice m)]
appSplices =
    [ ("header", headerSplice)
    , (markdownCssTagName, markdownCss)
    , missing "pageTitle"
    , missing "content"
    ]

missing :: Monad m => String -> (Text, Splice m)
missing name =
    (fromString name, fail $ "Missing splice '" ++ name ++ "'!") 

-- | By default, the "header" tag shows the page title.
headerSplice :: Monad m => Splice m
headerSplice = return [Element "pageTitle" [] []]

-- | Serve up a 404 response.
-- Note: this will succeed even if templates aren't
-- configured properly.
appNotFound :: App Response
appNotFound = do
  setResponseCode 404
  exists <- hasTemplate "/code/404" <$> appTemplate
  if exists
    then render "/code/404"
    else return $ toResponse ("The page you requested was not found." :: String)

