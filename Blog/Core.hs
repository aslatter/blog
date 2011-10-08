{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
{-|

This module defines the main 'Application' type,
as well as the associated monad for manipulating it.

-}
module Blog.Core
    ( module Xport
    , AppState(..)
    , App
    , runApp
    , execAppAction
    , appBlobStore
    , appUsers
    , appPosts
    , appTemplate
    , appTemplateDirectory
    ) where

import Blog.Users.Core as Xport (Users(..), UserId(..), emptyUsers)
import Blog.Posts.Core as Xport (Posts(..), emptyPosts, PostInsert(..), PostId)
import Blog.Sitemap as Xport
    ( Sitemap(..)
    , PostSite(..)
    , UserSite(..)
    , mkSitePI'
    )

import Control.Applicative (Alternative, Applicative)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Database.BlobStorage
import Database.BlobStorage as Xport (BlobId)
import Data.Acid
import Happstack.Server
    (ServerPartT, mapServerPartT,
     ServerMonad, FilterMonad, Response, WebMonad, HasRqData)
import Happstack.Server.TinyAuth as Xport
import Text.Templating.Heist
    (TemplateState)
import Text.Templating.Heist.TemplateDirectory
    (TemplateDirectory, getDirectoryTS)
import Web.Routes
import Web.Routes.Happstack()

-- shared for all requests
data AppState
    = MkAppState
      { app_blobstore :: BlobStorage
      , app_users :: AcidState Users
      , app_posts :: AcidState Posts
      , app_template :: TemplateDirectory App
      }

-- Woo!
newtype App a =
    App { unApp :: RouteT Sitemap (ServerPartT (ReaderT AppState IO)) a }
 deriving (ServerMonad, FilterMonad Response, WebMonad Response, HasRqData,
           Monad, Functor, Applicative, Alternative, MonadPlus, MonadIO,
           MonadReader AppState)
-- I dream of a future happstack where 'ServerMonad, FilterMonad Response,
-- WebMonad Response, HasRqData' are all summed up with 'ServerMonad'
instance ShowURL App where
    type URL App = Sitemap
    showURLParams url = App . showURLParams url


instance AuthMonad App where
    getAuthState = do
      loginUrl <- showURL $ User UserLogin
      return $ defaultAuthState {loginForm = loginUrl}

runApp :: AppState -> App a -> RouteT Sitemap (ServerPartT IO) a
runApp appState (App m) = mapRouteT mapFn m
 where
   mapFn =
       mapServerPartT $ flip runReaderT appState


-- | Used to resolve smart URL using functions
-- outside of a routing context.
execRouteT :: Site url x -> RouteT url m a -> m a
execRouteT site route =
    let fn url q1 =
            case formatPathSegments site url of
              (path,q2) -> encodePathInfo path (q1 ++ q2)
    in unRouteT route fn

-- | Whereas 'runApp' is meant to be used in the context of routing,
-- 'execAppAction' can be used outside of routing. The 'Site'
-- parameter is only used for URL resolution.
execAppAction :: Site Sitemap x -> AppState -> App a -> ServerPartT IO a
execAppAction site appState = execRouteT site . runApp appState

appBlobStore :: App BlobStorage
appBlobStore = asks app_blobstore

appUsers :: App (AcidState Users)
appUsers = asks app_users

appPosts :: App (AcidState Posts)
appPosts = asks app_posts

appTemplate :: App (TemplateState App)
appTemplate = asks app_template >>= getDirectoryTS

appTemplateDirectory :: App (TemplateDirectory App)
appTemplateDirectory = asks app_template

