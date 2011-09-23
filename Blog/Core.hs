{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|

This module defines the main 'Application' type,
as well as the associated monad for manipulating it.

-}
module Blog.Core
    ( module Blog.Users.Core
    , module Blog.Posts.Core
    , module Blog.Sitemap
    , AppState(..)
    , AppDynamic(..)
    , App
    , runApp
    , emptyAppDynamic
    , appBlobStore
    , appUsers
    , appPosts
    ) where

import Blog.Users.Core (Users(..), UserId(..), emptyUsers)
import Blog.Posts.Core (Posts(..), emptyPosts)
import Blog.Sitemap
    ( Sitemap(..)
    , PostSite(..)
    , UserSite(..)
    , mkSitePI'
    )

import Control.Monad.Reader
import Control.Monad.State.Strict

import Database.BlobStorage

import Data.Acid
import Data.Monoid (mempty)

import Happstack.Server (ServerPartT, mapServerPartT)

import Text.Templating.Heist
    (TemplateState)

import Web.Routes (RouteT, mapRouteT)

-- shared for all requests
data AppState
    = MkAppState
      { app_blobstore :: BlobStorage
      , app_users :: AcidState Users
      , app_posts :: AcidState Posts
      --, app_template :: TemplateState App
      }

-- created within a single request
data AppDynamic
    = MkAppDynamic
      { app_user :: Maybe UserId
      }

emptyAppDynamic :: AppDynamic
emptyAppDynamic = MkAppDynamic Nothing

-- Woo!
type App = RouteT Sitemap (ServerPartT (ReaderT AppState (StateT AppDynamic IO)))

runApp :: AppState -> App a -> RouteT Sitemap (ServerPartT IO) a
runApp appState m = mapRouteT mapFn m
 where
   mapFn =
       mapServerPartT $ flip evalStateT emptyAppDynamic . flip runReaderT appState

appBlobStore :: App BlobStorage
appBlobStore = asks app_blobstore

appUsers :: App (AcidState Users)
appUsers = asks app_users

appPosts :: App (AcidState Posts)
appPosts = asks app_posts

{-
appTemplate :: App (TemplateState App)
appTemplate = asks app_template
-}
