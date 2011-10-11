{-# LANGUAGE OverloadedStrings #-}

module Blog.Posts
    ( postHandler
    ) where

import Blog.Core
import Blog.Forms
import qualified Blog.Posts.Core as P
import Blog.Templates

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid (mempty, Monoid)
import Data.Acid (update)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.Time
    (Day, TimeOfDay, TimeZone, LocalTime(..), ZonedTime(..),
     getCurrentTimeZone)
import qualified Database.BlobStorage as Store
import Text.Blaze.Html5 (Html, (!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Digestive ((++>), (<++), check, Validator, validate)
import Text.Digestive.Blaze.Html5
import Text.Digestive.Forms.Happstack
import Happstack.Server

-- Primitve operations

insertPost :: PostInsert -> App PostId
insertPost pc = do
  p <- appPosts
  liftIO $ update p $ P.InsertPost pc

toLazyBS = LBS.fromChunks . return

storePostBody :: Text -> App BlobId
storePostBody body = do
  b <- appBlobStore
  liftIO $ Store.add b $ toLazyBS $ encodeUtf8 body

-- Forms & form data

data PostContent =
    MkPostContent
    { postTitle :: String
    , postDay   :: Day
    , postTime  :: TimeOfDay
    , postBody  :: String
    }

createPost :: UserId -> TimeZone -> PostContent -> App PostInsert
createPost user tz pc = do
  blob <- storePostBody body
  return $ MkInsert zonedTime title user blob
 where
   title = T.pack $ postTitle pc
   body  = T.pack $ postBody pc
   localTime = LocalTime (postDay pc) (postTime pc)
   zonedTime = ZonedTime localTime tz


postForm :: AppForm PostContent
postForm =
    MkPostContent
      <$> label "Title: " ++> titleForm Nothing <++ errors
      <*> label "Date:  " ++> inputTextRead "Invalid date" Nothing <++ errors
      <*> label "Time: "  ++> inputTextRead "Invalid time" Nothing <++ errors 
      <*> inputTextArea Nothing Nothing Nothing <++ errors

titleForm :: Maybe String -> AppForm String
titleForm title =
    mkRequired "A title is required" $
    inputText title

-- The handler

postHandler :: PostSite -> App Response
postHandler New = do
    user <- requireLoggedIn
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
      Right postContent -> do
           tz <- liftIO getCurrentTimeZone
           newPost <- createPost user tz postContent
           _ <- insertPost newPost
           renderWithText
            [ ("pageTitle", "Thanks!")
            , ("content", "Your post has been submitted!")
            ]
            "_layout"
          
postHandler _ = error "Nothing to see here"