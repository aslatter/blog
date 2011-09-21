{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}
module Blog.Users.Types where

import Blog.Instances()

import Control.Monad.State.Class (get, put, modify)
import Control.Monad.Reader.Class (asks)
import Data.Acid
import Data.Data (Data)
import Data.ByteString
import Data.Hashable
import qualified Data.HashMap.Strict as H
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (mempty)
import Data.SafeCopy
import Data.Text
import Data.Typeable (Typeable)
import Data.Word

-- fundamental operations on
-- the users db and a user,
-- including the acidic stuff

data User
 = MkUser
   { user_login   :: Text
   , user_name    :: Text
   , user_enabled :: Bool
   , user_admin   :: Bool
   , user_pwordhash :: ByteString
   }
  deriving (Typeable)

newtype UserId = MkUserId Word32
  deriving (Eq, Ord, Hashable, SafeCopy, Data, Typeable)

data Users
 = MkUsers
   { users_by_id :: H.HashMap UserId User
   , users_by_name :: Map Text UserId
   , users_next_id :: !Word32
   }
  deriving (Typeable)

emptyUsers =
    MkUsers
    { users_by_id = mempty
    , users_by_name = mempty
    , users_next_id = 0
    }

freshId :: Update Users UserId
freshId = do
  users <- get
  let oldIdent = users_next_id users
  put $ users { users_next_id = oldIdent + 1}
  return $ MkUserId oldIdent

-- | Returns 'Nothing'  if the given user name is already in
-- use.
newUser :: Text -> ByteString -> Update Users (Maybe UserId)
newUser login pwordhash
    = do
  existing <- runQuery $ userIdByLogin login
  case existing of
    Just{} -> return Nothing
    Nothing{}
        -> do
      let user = MkUser login mempty True False pwordhash
      ident <- freshId
      modify $ \u ->
          let by_id = H.insert ident user $ users_by_id u
              by_name = M.insert login ident $ users_by_name u
          in u { users_by_id = by_id
               , users_by_name = by_name
               }
      return $ Just ident

userById :: UserId -> Query Users (Maybe User)
userById ident = asks $ H.lookup ident . users_by_id

userIdByLogin :: Text -> Query Users (Maybe UserId)
userIdByLogin login = asks $ M.lookup login . users_by_name


deriveSafeCopy 1 'base ''User
deriveSafeCopy 1 'base ''Users

makeAcidic ''Users
           [ 'newUser
           , 'userById
           , 'userIdByLogin
           ]
