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
    , initTemplates
    , appNotFound
    ) where

import Blog.Core

import Control.Applicative ((<$>))
import Blaze.ByteString.Builder (toLazyByteString)
import Data.ByteString (ByteString)
import Happstack.Server
import qualified Happstack.Server.Heist as H
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

renderWith :: (TemplateState App -> TemplateState App) -> ByteString -> App Response
renderWith f name = do
  ts <- appTemplate
  renderInternal (f ts) name

-- | Ping this handler to refresh the templates from disk.
templateReloader :: App Response
templateReloader =
    appTemplateDirectory >>= H.templateReloader

-- | Build the initial template state information, given
-- the path to the templates folder.
initTemplates :: FilePath -> IO (TemplateDirectory App)
initTemplates templateDir = do
  let ts = bindSplices splices
           $ emptyTemplateState templateDir
  newTemplateDirectory' templateDir ts

splices =
    [ ("header", headerSplice)
    ]

-- | By default, the "header" tag shows the page title.
headerSplice :: Monad m => Splice m
headerSplice = return $ [Element "pageTitle" [] []]

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

