{-# LANGUAGE OverloadedStrings #-}

import Blog.Core
import Blog.Posts
import Blog.Templates
import Blog.Users

import Control.Exception (bracket)
import Control.Monad.Reader
import Data.Acid
import Data.List ((!!))
import qualified Database.BlobStorage as BS
import Happstack.Server
    hiding (body)
import System.Environment (getArgs)
import Web.Routes
import Web.Routes.Happstack

-- 'Sitemap' is a parsed URL path.
-- This function dispatches to our request handlers.
route :: Sitemap -> App Response
route url =
    case url of
      Home         -> render "home"
      Post postUrl -> postHandler postUrl
      User userUrl -> userHandler userUrl

main :: IO ()
main = do
  args <- getArgs
  if (length args == 2)
   then addUser (args!!0) (args!!1) 
   else site

addUser :: String -> String -> IO ()
addUser name pword =
    withAppState $ \appState -> do
      
      addUserFromPlaintext name pword $ app_users appState
      return ()

site :: IO ()
site =
    withAppState $ \appState -> do

      let site = mkSite appState
      let appAction = execAppAction site appState

      simpleHTTP nullConf $
        msum
          [ implSite "/" "" site
          , serveDirectory DisableBrowsing [] "static"
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
withAppState =
    bracket initAppState closeAppState

-- The 'Site' type is part of the web-routes package, and its
-- first parameter is my Sitemap type.
mkSite :: AppState -> Site Sitemap (ServerPartT IO Response)
mkSite appState
    = setDefault Home
      $ mkSitePI'
      $ runRouteT
      $ runApp appState . route
