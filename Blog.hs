{-# LANGUAGE OverloadedStrings #-}

import Blog.Core

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

-- gory details
main :: IO ()
main = do
  appState <- initAppState

  let site = mkSite appState

  simpleHTTP nullConf $
    implSite "/" "" site

initAppState :: IO AppState
initAppState = do
  posts <- openAcidState emptyPosts
  users <- openAcidState emptyUsers
  store <- BS.open "blobStore"

  return $ MkAppState store users posts  

-- gorier details
mkSite :: AppState -> Site Sitemap (ServerPartT IO Response)
mkSite appState
    = setDefault Home
      $ mkSitePI
      $ runRouteT
      $ runApp appState . route

