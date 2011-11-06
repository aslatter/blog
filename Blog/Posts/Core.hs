{-# LANGUAGE DoAndIfThenElse, BangPatterns, TemplateHaskell, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}
module Blog.Posts.Core where

import Blog.Users.Core (UserId)

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Class (get, put, modify)

import Database.BlobStorage (BlobId)

import Data.Acid
import Data.Char (isAlphaNum)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid (mappend, mempty)
import Data.SafeCopy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Typeable (Typeable)
import Data.Word
import Web.Routes (PathInfo)

{-

* A Post is what we store in the database
* A PostInsert is what a user edits

-}

data PostInsert =
 MkInsert {
  insert_time  :: ZonedTime,
  insert_title :: Text,
  insert_author  :: UserId,
  insert_body  :: BlobId
 }
 deriving Typeable

data Post =
 MkPost {
  post_id :: PostId,
  post_time :: ZonedTime,
  post_short_name :: Text,
  post_title :: Text,
  post_author :: UserId,
  post_body :: BlobId
 }
 deriving Typeable

-- | This class defines a least common denominator for
-- the types that represent posts
class PostLike post where
    toPostInsert :: post -> PostInsert

instance PostLike PostInsert where
    toPostInsert = id

instance PostLike Post where
    toPostInsert p =
        MkInsert
        { insert_time = post_time p
        , insert_title = post_title p
        , insert_author = post_author p
        , insert_body = post_body p
        }

postUtcTime :: PostLike post => post -> UTCTime
postUtcTime = zonedTimeToUTC . insert_time . toPostInsert

postDay :: PostLike post => post -> Day
postDay = localDay . zonedTimeToLocalTime . insert_time . toPostInsert

postTz :: PostLike post => post -> TimeZone
postTz = zonedTimeZone . insert_time . toPostInsert

newtype PostId = MkPostId Word32
  deriving (Eq, Ord, Show, Read, Hashable,
              SafeCopy, Typeable, Data, PathInfo)

data Posts =
 MkPosts {
  posts_by_time :: M.Map UTCTime (HashSet PostId),
  posts_by_day  :: M.Map Day (HashSet PostId),
  posts_by_id   :: HashMap PostId Post,
  posts_next_id :: !Word32
 }
 deriving Typeable

emptyPosts :: Posts
emptyPosts =
    MkPosts
    { posts_by_time = mempty
    , posts_by_day = mempty
    , posts_by_id = mempty
    , posts_next_id = 0
    }

-- | Generate an id for a new post
freshId :: Update Posts PostId
freshId = do
  posts <- get
  let next = MkPostId $ posts_next_id posts
  put $ posts { posts_next_id = posts_next_id posts + 1 }
  return next

titleExistsForDay :: Text -> Day -> Query Posts Bool
titleExistsForDay title day = do
  idsForDay <- M.findWithDefault HS.empty day <$> asks posts_by_day
  titlesForDay <-  map post_title . catMaybes <$> mapM postById (HS.toList idsForDay)
  return $ HS.member title $ HS.fromList titlesForDay

titleStyleText :: Text -> Text
titleStyleText str =
    let exploded = T.split (not . isAlphaNum) str
        flted    = filter (not . T.null) exploded
    in T.intercalate (T.singleton '_') flted

mkTitle :: Text -> Text
mkTitle str =
    let titleStyled = titleStyleText str
    in if (T.null titleStyled)
       then T.pack "untitled"
       else T.take 200 titleStyled

createPostTitle :: PostInsert -> Query Posts Text
createPostTitle post = do
  let d = postDay post
      t = mkTitle (insert_title post)
  go d t (1::Int)

 where
   addSuffix str n 
       | n == 0 = str
       | otherwise
           = str `mappend` T.pack (show n)
   go day title !ix = do
     let numberedTitle = addSuffix title ix
     exists <- titleExistsForDay numberedTitle day
     if exists then go day title (ix+1)
     else return title
  

-- always succeeds
insertPost :: PostInsert -> Update Posts PostId
insertPost post = do
  pid <- freshId
  unsafeInsertPostById post pid
  return pid

-- Assumes that a post with the given id does not exist
-- in the posts collection
unsafeInsertPostById :: PostInsert -> PostId -> Update Posts ()
unsafeInsertPostById postIns postIdent = do
  let day = postDay postIns
      utcTime = postUtcTime postIns
  shortTitle <- runQuery $ createPostTitle postIns
  let post =
          MkPost
          { post_id = postIdent
          , post_time = insert_time postIns
          , post_author = insert_author postIns
          , post_title = insert_title postIns
          , post_body = insert_body postIns
          , post_short_name = shortTitle
          }
  modify $ \posts ->
      posts { posts_by_day = updateSet postIdent day (posts_by_day posts)
            , posts_by_time = updateSet postIdent utcTime (posts_by_time posts)
            , posts_by_id = HM.insert postIdent post (posts_by_id posts)
            }
 where
   updateSet :: (Ord key, Eq val, Hashable val) =>
                val -> key ->
                M.Map key (HS.HashSet val) -> M.Map key (HS.HashSet val)
   updateSet v =
       M.alter $ \hsM ->
           case hsM of
             Nothing -> Just $ HS.singleton v
             Just hs -> Just $ HS.insert v hs


-- | always succeeds, unless the post id is invalid
updatePost :: PostInsert -> PostId -> Update Posts Bool
updatePost post pid = do
  result <- deletePost pid
  when result $
       unsafeInsertPostById post pid
  return result

-- | Returns false if the post was not found
deletePost :: PostId -> Update Posts Bool
deletePost pid = do
  mPost <- runQuery $ postById pid
  case mPost of
    Nothing -> return False
    Just post ->
        let day = postDay post
            utcTime = postUtcTime post
            removeFromMap :: Ord a => a -> (M.Map a (HS.HashSet PostId)) -> (M.Map a (HS.HashSet PostId))
            removeFromMap key =
                M.update
                     (\s -> let s' = HS.delete pid s in if HS.null s' then Nothing else Just s')
                     key
        in do
          modify $ \posts ->
              posts
               { posts_by_day = removeFromMap day (posts_by_day posts)
               , posts_by_time = removeFromMap utcTime (posts_by_time posts)
               , posts_by_id = HM.delete pid (posts_by_id posts)
               }
          return True


postById :: PostId -> Query Posts (Maybe Post)
postById postId = asks $ \posts -> HM.lookup postId (posts_by_id posts)

paginatePosts :: Int -- ^ Zero indexed post to start with
              -> Int -- ^ Number of posts to return 
              -> Query Posts [Post]
paginatePosts start rows =
    asks $ \posts ->
    let sets = map snd $ M.toDescList (posts_by_time posts)
        idList = concatMap HS.toList sets
        postList = mapMaybe (flip HM.lookup (posts_by_id posts)) idList
    in take rows $ drop start postList

postByPath :: Day -> Text -> Query Posts (Maybe Post)
postByPath day shortTitle =
    asks $ \posts -> do
      idSet <- M.lookup day (posts_by_day posts)
      let postList = mapMaybe (flip HM.lookup (posts_by_id posts)) $ HS.toList idSet
      find (\p -> post_short_name p == shortTitle) postList

deriveSafeCopy 1 'base ''Post
deriveSafeCopy 1 'base ''PostInsert
deriveSafeCopy 1 'base ''Posts

makeAcidic ''Posts
           [ 'insertPost
           , 'updatePost
           , 'deletePost
           , 'postById
           , 'postByPath
           , 'paginatePosts
           ]
