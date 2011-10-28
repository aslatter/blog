{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Blog.Posts
    ( postHandler
    , paginatePosts
    , getPostBody
    ) where

import Blog.Core
import Blog.Forms
import qualified Blog.Posts.Core as P
import Blog.Templates

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Acid (update', query')
import Data.Monoid (mconcat)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time
    (Day, TimeOfDay, TimeZone, LocalTime(..), ZonedTime(..),
     getCurrentTimeZone, getZonedTime)
import qualified Database.BlobStorage as Store
import Text.Digestive ((++>), (<++))
import Text.Digestive.Blaze.Html5
import Happstack.Server (Response, decodeBody, defaultBodyPolicy)

-- Primitve operations

insertPost :: PostInsert -> App PostId
insertPost pc = do
  p <- appPosts
  update' p $ P.InsertPost pc

postById :: PostId -> App (Maybe Post)
postById pid = do
  p <- appPosts
  query' p $ P.PostById pid

updatePost :: PostInsert -> PostId -> App Bool
updatePost post pid = do
  p <- appPosts
  update' p $ P.UpdatePost post pid

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


postForm :: PostContent -> AppForm PostContent
postForm c =
    MkPostContent
      <$> label "Title: " ++> titleForm (Just $ pc_title c) <++ errors
      <*> label "Date:  " ++> inputTextRead "Invalid date" (Just $ pc_day c) <++ errors
      <*> label "Time: "  ++> inputTextRead "Invalid time" (Just $ pc_time c) <++ errors 
      <*> inputTextArea (Just 25) (Just 80) (Just $ pc_body c) <++ errors

titleForm :: Maybe Text -> AppForm Text
titleForm title =
    mkRequired "A title is required" $
    inputText title

newPostForm :: App (AppForm PostContent)
newPostForm = do
  zt <- liftIO getZonedTime
  let t = zonedTimeToLocalTime zt
  return $
    postForm $
    MkPostContent
        ""
        (localDay t)
        (localTimeOfDay t)
        ""

editPostForm :: Post -> App (AppForm PostContent)
editPostForm post = do
  body <- getPostBody post
  let t = zonedTimeToLocalTime $ post_time post
  return $
    postForm $
    MkPostContent
        (post_title post)
        (localDay t)
        (localTimeOfDay t)
        body

-- The handler

postBodyDecode :: App ()
postBodyDecode =
    decodeBody $ defaultBodyPolicy "tmp" (1024*1024) (20*1024) (2*1024)

postHandler :: PostSite -> App Response
postHandler New = do
    user <- requireLoggedIn'
    postBodyDecode

    postContent <- newPostForm >>= handleForm "New Post"

    tz <- liftIO getCurrentTimeZone
    newPost <- createPost user tz postContent
    _ <- insertPost newPost
    renderWithText
      [ ("pageTitle", "Thanks!")
      , ("content", "Your post has been submitted!")
      ]
      "_layout"

postHandler (Edit postId) = do
  postM <- postById postId
  case postM of
    Nothing -> empty
    Just post
        -> do
      user <- requireLoggedIn'
      -- check for posting user?
      postBodyDecode

      postContent <- editPostForm post >>= handleForm "Edit Post"
      newPost <- createPost (post_author post) (postTz post) postContent
      ret <- updatePost newPost postId

      if not ret
      then undefined -- TODO
      else do

      renderWithText
        [ ("pageTitle", "Thanks!")
        , ("content", "Your edit has been submitted!")
        ]
        "_layout"

postHandler _ = error "Nothing to see here"
