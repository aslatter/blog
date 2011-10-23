{-# LANGUAGE OverloadedStrings #-}

module Blog.Posts
    ( postHandler
    , paginatePosts
    , getPostBody
    ) where

import Blog.Core
import Blog.Forms
import qualified Blog.Posts.Core as P
import Blog.Templates

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Acid (update', query')
import Data.Monoid (mconcat)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T
import Data.Time
    (Day, TimeOfDay, TimeZone, LocalTime(..), ZonedTime(..),
     getCurrentTimeZone)
import qualified Database.BlobStorage as Store
import Text.Blaze.Html5 ((!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Digestive ((++>), (<++))
import Text.Digestive.Blaze.Html5
import Text.Digestive.Forms.Happstack
import Happstack.Server (Response, decodeBody, defaultBodyPolicy, finishWith)

-- Primitve operations

insertPost :: PostInsert -> App PostId
insertPost pc = do
  p <- appPosts
  update' p $ P.InsertPost pc

storePostBody :: Text -> App BlobId
storePostBody body = do
  b <- appBlobStore
  liftIO $ Store.add b $ toLazyBS $ encodeUtf8 body
 where toLazyBS = LBS.fromChunks . return

getPostBody :: P.Post -> App Text
getPostBody post = do
  b <- appBlobStore
  bytes <- liftIO $ Store.fetch b (P.post_body post)
  return $ decodeUtf8 $ fromLazyBs bytes
 where fromLazyBs = mconcat . LBS.toChunks

paginatePosts :: Int -> Int -> App [P.Post]
paginatePosts start rows = do
  p <- appPosts
  query' p $ P.PaginatePosts start rows

-- Forms & form data

data PostContent =
    MkPostContent
    { pc_title :: Text
    , pc_day   :: Day
    , pc_time  :: TimeOfDay
    , pc_body  :: Text
    }

createPost :: UserId -> TimeZone -> PostContent -> App PostInsert
createPost user tz pc = do
  blob <- storePostBody body
  return $ MkInsert zonedTime title user blob
 where
   title = pc_title pc
   body  = pc_body pc
   localTime = LocalTime (pc_day pc) (pc_time pc)
   zonedTime = ZonedTime localTime tz


postForm :: AppForm PostContent
postForm =
    MkPostContent
      <$> label "Title: " ++> titleForm Nothing <++ errors
      <*> label "Date:  " ++> inputTextRead "Invalid date" Nothing <++ errors
      <*> label "Time: "  ++> inputTextRead "Invalid time" Nothing <++ errors 
      <*> inputTextArea Nothing Nothing Nothing <++ errors

titleForm :: Maybe Text -> AppForm Text
titleForm title =
    mkRequired "A title is required" $
    inputText title

-- The handler

postHandler :: PostSite -> App Response
postHandler New = do
    user <- requireLoggedIn
    decodeBody $ defaultBodyPolicy "tmp" (1024*1024) (20*1024) (2*1024)

    postContent <- handleForm "New Post" postForm

    tz <- liftIO getCurrentTimeZone
    newPost <- createPost user tz postContent
    _ <- insertPost newPost
    renderWithText
      [ ("pageTitle", "Thanks!")
      , ("content", "Your post has been submitted!")
      ]
      "_layout"
  
postHandler _ = error "Nothing to see here"
