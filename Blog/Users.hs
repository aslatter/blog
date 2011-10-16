{-# LANGUAGE OverloadedStrings #-}
module Blog.Users
    ( userHandler
    , addUserFromPlaintext
    )where

import Blog.Core
import Blog.Forms
import Blog.Templates
import qualified Blog.Users.Core as Core

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, void)
import Control.Monad.Trans (liftIO)
import Crypto.BCrypt (HashingPolicy)
import qualified Crypto.BCrypt as BC
import Data.Acid
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Happstack.Server
import Text.Blaze.Html5 (Html, (!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Digestive
    ((++>), Transformer, transformEitherM, transform)
import Text.Digestive.Blaze.Html5
import Text.Digestive.Forms.Happstack

-- primitive operations

newUser :: Text -> ByteString -> App (Maybe UserId)
newUser login pwordhash = do
  s <- appUsers
  liftIO $ update s $ Core.NewUser login pwordhash

userById :: UserId -> App (Maybe User)
userById ident = do
  s <- appUsers
  liftIO $ query s $ Core.UserById ident

userIdByLogin :: Text -> App (Maybe UserId)
userIdByLogin login = do
  s <- appUsers
  liftIO $ query s $ Core.UserIdByLogin login

userByLogin :: Text -> App (Maybe (User, UserId))
userByLogin name = do
  userIdM <- userIdByLogin name
  case userIdM of
    Nothing -> return Nothing
    Just userId ->
        userById userId >>= \userM ->
        case userM of
          Nothing -> return Nothing
          Just user -> return $ Just (user, userId)

setLoginHash :: UserId -> ByteString -> App Bool
setLoginHash user hash = do
  s <- appUsers
  liftIO $ update s $ Core.SetLoginHash user hash

-- more complex operations

hashingPolicy :: HashingPolicy
hashingPolicy = BC.slowerBcryptHashingPolicy

hashPassword :: ByteString -> IO ByteString
hashPassword pw = do
  hashM <- BC.hashPasswordUsingPolicy hashingPolicy pw
  case hashM of
    Nothing{} -> error "Configuration error"
    Just hash -> return hash

-- On auth success, if not using the current hashing policy,
-- re-hash and update users db with new hash.
coreAuthUser :: Text -> ByteString -> App (Maybe UserId)
coreAuthUser name pword = do
  userM <- userByLogin name
  case userM of
    Nothing -> do
        return Nothing
    Just (user, userId)
        -> do
      let hashed = user_pwordhash user
          success = BC.validatePassword hashed pword
          byPolicy = BC.hashUsesPolicy hashingPolicy hashed
      when (success && not byPolicy) $ do
        -- re-hash with current policy
        reHashed <- liftIO $ hashPassword pword
        void $ setLoginHash userId reHashed
      return $
        if success then Just userId else Nothing

addUserFromPlaintext :: String -- ^ Username
                     -> String -- ^ Password
                     -> AcidState Users
                     -> IO (Maybe UserId)
addUserFromPlaintext username password s =
    let name = T.pack username
        plain = T.encodeUtf8 . T.pack $ password
    in do
      hashed <- hashPassword plain
      update s $ Core.NewUser name hashed

-- Forms & higher-level operations

data LoginData
    = LoginData
      { login_username :: String
      , login_password :: String
      }

loginForm :: AppForm UserId
loginForm =
    flip transform authenticate $
    (errors ++>) $
    LoginData <$> label "User: "     ++> inputText Nothing
              <*> label "Password: " ++> inputPassword

-- we perform a login by transforming the form into one
-- over user ids

authenticate :: Transformer App Html LoginData UserId
authenticate =
    transformEitherM $ \login -> do
      let userName = T.pack $ login_username login
          pword = T.encodeUtf8 . T.pack $ login_password login
      authResult <- coreAuthUser userName pword
      case authResult of
        Just userId -> return . Right $ userId
        Nothing -> return . Left $ "Incorrect username or password!"

-- handler

userHandler :: UserSite -> App Response
userHandler UserLogin = do
  decodeBody $ defaultBodyPolicy "tmp" (1024*1024) (20*1024) (2*1024)
  r <- eitherHappstackForm loginForm "login"
  case r of
    Left form -> do
        let (renderedForm,_) = renderFormHtml form
        renderBlaze
          [ ("pageTitle", "Login")
          ]
          "_layout"
          $ H.form ! A.method (toValue ("POST"::String)) $
            do
              renderedForm
              H.input ! A.type_ "submit"
    Right userId
        -> do
           setLoggedIn userId
           forwardAfterLogin "/"

userHandler _ = error "What?!?!"