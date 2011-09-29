{-# LANGUAGE OverloadedStrings #-}

import Blog.Core
import Blog.Posts
import Blog.Templates

import Control.Exception (bracket)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Acid
import qualified Database.BlobStorage as BS
import Happstack.Server
    hiding (body)
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import Web.Routes
import Web.Routes.Happstack

-- 'Sitemap' is a parsed URL path.
-- This function dispatches to our request handlers.
route :: Sitemap -> App Response
route url =
    case url of
      Home   -> render "home"
      Post url -> postHandler url
      User{} -> error "What the heck are users?!?"

main :: IO ()
main =
    withAppState $ \appState -> do

      let site = mkSite appState
      let appAction = execAppAction site appState

      simpleHTTP nullConf $
        msum
          [ implSite "/" "" site
          , appAction appNotFound
          ]

-- The app-state is shared by all incoming
-- requests/threads, and has references to
-- databases and configuration and so on that
-- the handlers use to service requests.
initAppState :: IO AppState
initAppState = do
  putStrLn "Starting up ..."

  posts <- openAcidState emptyPosts
  users <- openAcidState emptyUsers
  store <- BS.open "blobStore"
  templates <- initTemplates "templates"

  return $ MkAppState store users posts templates  

closeAppState :: AppState -> IO ()
closeAppState (MkAppState _ users posts _) = do
  putStrLn "\nShutting down ..."
  closeAcidState users
  closeAcidState posts

withAppState :: (AppState -> IO a) -> IO a
withAppState k =
    bracket initAppState closeAppState k

-- The 'Site' type is part of the web-routes package, and its
-- first parameter is my Sitemap type.
mkSite :: AppState -> Site Sitemap (ServerPartT IO Response)
mkSite appState
    = setDefault Home
      $ mkSitePI'
      $ runRouteT
      $ runApp appState . route
