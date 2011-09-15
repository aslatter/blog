{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|

This module defines the main 'Application' type,
as well as the associated monad for manipulating it.

-}
module Blog.Types where

import Blog.Users.Types
import Blog.Posts.Types
import Blog.Sitemap (Sitemap)

import Control.Monad.Reader
import Control.Monad.State

import Database.BlobStorage

import Data.Acid

import Text.Templating.Heist
    (TemplateState)

-- shared for all requests
data Application
    = MkApplication
      { app_blobstore :: BlobStorage
      , app_users :: AcidState Users
      , app_posts :: AcidState Posts
      , app_template :: TemplateState AppMonad
      }

-- created within a single request
data AppDynamic
    = MkAppDynamic
      { app_user :: Maybe UserId
      }

newtype AppMonadT m a = A (ReaderT Application (StateT AppDynamic m) a)
  deriving (Functor, Monad, MonadIO)
type AppMonad = AppMonadT IO

class MonadIO m => App m where
    appBlobStore :: m BlobStorage
    appUsers :: m (AcidState Users)
    appPosts :: m (AcidState Posts)
    appTemplate :: m (TemplateState AppMonad)
    appDynamicGet :: m AppDynamic
    appDynamicSet :: AppDynamic -> m ()

instance MonadIO m => App (AppMonadT m) where
    appBlobStore = A $ asks app_blobstore
    appUsers     = A $ asks app_users
    appPosts     = A $ asks app_posts
    appTemplate  = A $ asks app_template
    appDynamicGet = A $ get
    appDynamicSet = A . put

