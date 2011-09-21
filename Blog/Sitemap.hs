{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-|

This module defines the Sitemap type,
which is used for routing, as well as its
mapping to and from path segments.

-}
module Blog.Sitemap where

import Blog.Instances()
import Blog.Posts.Core (PostId(..))

import Control.Applicative

import Data.Data (Data)
import Data.Text (Text)
import Data.Time
    ( Day
    , FormatTime, ParseTime
    , readsTime
    , showGregorian
    )
import Data.Typeable (Typeable)

import System.Locale (defaultTimeLocale, iso8601DateFormat)

import Text.Parsec

import Web.Routes (PathInfo(..), URLParser, pToken)
import Web.Routes.TH (derivePathInfo)

data Sitemap
    = Home
    | Post PostSite
    | User UserSite
 deriving (Eq, Ord, Read, Show, Data, Typeable)

data PostSite
    = View PathDay Text
    | Edit PostId
    | Delete PostId
    | New
 deriving (Eq, Ord, Read, Show, Data, Typeable)

data UserSite
    = UserLogin
    | UserLogout
    | UserNew
 deriving (Eq, Ord, Read, Show, Data, Typeable)

-- for avoiding orphan instances
newtype PathDay = PathDay Day
 deriving (Eq, Ord, Read, Show, Data, Typeable, ParseTime, FormatTime)

parseReadS :: msg -> ReadS a -> URLParser a
parseReadS msg r =
    pToken msg $ \str ->
        case r str of
          [(x,"")] -> Just x
          _ -> Nothing

instance PathInfo PathDay where
    toPathSegments (PathDay day) =
        [showGregorian day]
    fromPathSegments =
        PathDay <$>
         parseReadS "valid date"
           (readsTime defaultTimeLocale (iso8601DateFormat Nothing))

deriving instance PathInfo PostId

derivePathInfo ''UserSite
derivePathInfo ''PostSite
derivePathInfo ''Sitemap
