
module Blog.Users where

import Blog.Types
import Blog.Users.Types

-- list acidic actions into
-- application monad

newUser :: App m => Text -> ByteString -> m (Maybe UserId)
newUser login pwordhash = do
  s <- appUsers
  update' s $ NewUser login pwordhash

userById :: App m => UserId -> m (Maybe User)
userById ident = do
  s <- appUsers
  query' s $ UserById ident

userIdByLogin :: App m => Text -> m (Maybe UserId)
userIdByLogin login = do
  s <- appUsers
  query' s $ UserIdByLogin login

