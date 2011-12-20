{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

module Blog.Posts
    ( postHandler
    , paginatePosts
    , getPostBody
    , postHtml
    ) where

import Blog.Core
import Blog.Forms
import Blog.Markdown
import qualified Blog.Posts.Core as P
import Blog.Templates

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Acid (update', query')
import Text.Blaze.Html5 ((!), toValue, Html, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mconcat, mappend)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time
    (Day, TimeOfDay, TimeZone, LocalTime(..), ZonedTime(..),
     getCurrentTimeZone, getZonedTime)
import qualified Database.BlobStorage as Store
import Text.Digestive ((++>), (<++))
import Text.Digestive.Blaze.Html5
import Happstack.Server
    (Response, decodeBody, defaultBodyPolicy, seeOther, toResponse,
     askRq, readInputsBody )
import Web.Routes (showURL)

-- Primitve operations

insertPost :: PostInsert -> App Post
insertPost pc = do
  p <- appPosts
  pid <- update' p $ P.InsertPost pc
  post <- postById pid
  case post of
    Nothing -> error "fatal error in insertPost"
    Just post' -> return post'

postById :: PostId -> App (Maybe Post)
postById pid = do
  p <- appPosts
  query' p $ P.PostById pid

postByPath :: Day -> Text -> App (Maybe Post)
postByPath day title = do
  p <- appPosts
  query' p $ P.PostByPath day title

updatePost :: PostInsert -> PostId -> App (Maybe Post)
updatePost post pid = do
  p <- appPosts
  res <- update' p $ P.UpdatePost post pid
  case res of
    False -> return Nothing
    True  -> postById pid

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
    , pc_body  :: Text -- ^ newlines are expected to be \r\n encoded
    }

createPost :: UserId -> TimeZone -> PostContent -> App PostInsert
createPost user tz pc = do
  blob <- storePostBody body
  return $ MkInsert zonedTime title user blob
 where
   title = pc_title pc
   body  = decodeNewlines $ pc_body pc
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
        (encodeNewlines body)

encodeNewlines :: Text -> Text
encodeNewlines =
    T.intercalate "\r\n" . T.splitOn "\n"

decodeNewlines :: Text -> Text
decodeNewlines =
    T.intercalate "\n" . T.splitOn "\r\n"

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
    post' <- insertPost newPost

    permURL <- postUrl post'
    seeOther permURL $ toResponse ()

postHandler (Edit postId) = do
  postM <- postById postId
  case postM of
    Nothing -> empty
    Just post
        -> do
      _ <- requireLoggedIn'
      -- check for posting user?
      postBodyDecode

      postContent <- editPostForm post >>= handleForm "Edit Post"
      newPost <- createPost (post_author post) (postTz post) postContent
      ret <- updatePost newPost postId

      case ret of
        Nothing -> undefined -- TODO
        Just post' -> do
         permURL <- postUrl post'
         seeOther permURL $ toResponse ()

postHandler (View (PathDay day) shortTitle) = do
  postM <- postByPath day shortTitle
  case postM of
    Nothing -> empty
    Just post ->
        postHtml post >>=
        renderBlaze [("pageTitle",post_title post),("header","")] "_layout"

postUrl :: Post -> App String
postUrl post = showURL $ Post $ View (PathDay $ postDay post) (post_short_name post)

postHtml :: Post -> App Html
postHtml post = do
  body <- getPostBody post
  editUrl <- showURL $ Post $ Edit $ post_id post
  permURL <- postUrl post
  return $
    H.article $ do
      H.h2 $
        H.a ! A.href (toValue permURL) $
          toHtml $ post_title post
      H.p $ do
        "Posted on: "
        toHtml $ show $ post_time post
      markdown body
      H.p $
        H.a ! A.href (toValue editUrl) $ "Edit"

