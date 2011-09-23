{-# LANGUAGE OverloadedStrings #-}

import Blog.Core

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
      Home ->
          do
            newPostURL <- showURL $ Post New
            loginURL   <- showURL $ User UserLogin

            ok $ toResponse $
               docTypeHtml $
               body $ do
                 h1 "links"
                 p $ a ! A.href (toValue newPostURL)  $ "New Post"
                 p $ a ! A.href (toValue loginURL)    $ "Login"
                          
      Post{} -> error "What the heck are posts?!?"
      User{} -> error "What the heck are users?!?"

main :: IO ()
main =
    withAppState $ \appState -> do

      let site = mkSite appState

      simpleHTTP nullConf $
          implSite "/" "" site

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

  return $ MkAppState store users posts  

closeAppState :: AppState -> IO ()
closeAppState (MkAppState _ users posts) = do
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

