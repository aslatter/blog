{-# LANGUAGE OverloadedStrings #-}

module Blog.Posts
    ( postHandler
    ) where

import Blog.Core
import Blog.Templates

import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mempty, Monoid)
import Data.Time (Day, TimeOfDay)
import Text.Blaze.Html5 (Html, (!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Digestive ((++>), (<++), check, Validator, validate)
import Text.Digestive.Blaze.Html5
import Text.Digestive.Forms.Happstack
import Happstack.Server

data PostContent =
    MkPostContent
    { postTitle :: String
    , postDay   :: Day
    , postTime  :: TimeOfDay
    , postBody  :: String
    }

type AppForm a = HappstackForm App Html BlazeFormHtml a

postForm :: AppForm PostContent
postForm =
    MkPostContent
      <$> label "Title: " ++> titleForm Nothing <++ errors
      <*> label "Date:  " ++> inputTextRead "Invalid date" Nothing <++ errors
      <*> label "Time: "  ++> inputTextRead "Invalid time" Nothing <++ errors 
      <*> inputTextArea Nothing Nothing Nothing <++ errors

titleForm :: Maybe String -> AppForm String
titleForm title =
    flip validate (required "A title is required") $
    inputText title

required :: (Monoid a, Eq a, Monad m) =>
            errMsg
         ->  Validator m errMsg a
required e = check e (/= mempty)

postHandler :: PostSite -> App Response
postHandler New = do
    decodeBody $ defaultBodyPolicy "tmp" (1024*1024) (20*1024) (2*1024)
    r <- eitherHappstackForm postForm "new-post" :: App (Either BlazeFormHtml PostContent)
    case r of
      Left form -> do
          let (renderedForm,_) = renderFormHtml form
          renderBlaze
            [ ("pageTitle", "New Post")
            ]
            "_layout"
            $ H.form ! A.method (toValue ("POST"::String)) $
               do
                 renderedForm
                 H.input ! A.type_ "submit"
      Right _ ->
          renderWithText
            [ ("pageTitle", "Thanks!")
            , ("content", "Your post has been submitted!")
            ]
            "_layout"
          
postHandler _ = error "Nothing to see here"