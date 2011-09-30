{-# LANGUAGE OverloadedStrings #-}

module Blog.Posts
    ( postHandler
    ) where

import Blog.Core
import Blog.Templates

import Control.Applicative ((<$>), (<*>))
import Data.Time (Day)
import Text.Blaze.Html5 (Html, (!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Digestive ((++>), (<++))
import Text.Digestive.Blaze.Html5
import Text.Digestive.Forms.Happstack
import Happstack.Server

data PostContent =
    MkPostContent
    { postTitle :: String
    , postTime :: Day
    , postBody :: String
    }

{-
inputDate :: (Monad m, Functor m, FormInput i f)
             => Formlet m i e BlazeFormHtml Day
inputDate err =
    Forms.inputRead err $ \id' inp ->
    createFormHtml $ \cfg ->
        applyClasses' [htmlInputClasses] cfg $
            H.input ! A.type_ "date"
                    ! A.name (toValue $ show id') 
                    ! A.id (toValue $ show id')
                    ! A.value (toValue $ fromMaybe "" inp)
-}

postForm :: HappstackForm App Html BlazeFormHtml PostContent
postForm =
    MkPostContent
      <$> label "Title: " ++> inputText Nothing <++ errors
      <*> label "Date:  " ++> inputTextRead "Invalid date" Nothing <++ errors
      <*> inputTextArea Nothing Nothing Nothing <++ errors

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