{-# OPTIONS_GHC -fno-warn-orphans #-}
module Blog.Instances() where

import Database.BlobStorage (BlobId)

import qualified Data.Binary as B
import Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import Data.SafeCopy
import qualified Data.Serialize as C
import qualified Data.Text as T
import Data.Word

import qualified Web.Routes as WR

instance (SafeCopy k, SafeCopy v, Eq k, Hashable k) => SafeCopy (H.HashMap k v) where
    getCopy = contain $ fmap H.fromList safeGet
    putCopy = contain . safePut . H.toList

instance (SafeCopy k, Eq k, Hashable k) => SafeCopy (HS.HashSet k) where
    getCopy = contain $ fmap HS.fromList safeGet
    putCopy = contain . safePut . HS.toList

instance SafeCopy BlobId where
    getCopy = getBinaryCopy
    putCopy = putBinaryCopy

getBinaryCopy :: (B.Binary a) => Contained (C.Get a)
getBinaryCopy =
    contain $ do
      bytes <- C.get
      return $ B.decode bytes

putBinaryCopy :: (B.Binary a) => a -> Contained C.Put
putBinaryCopy = contain . C.put . B.encode

instance WR.PathInfo Word32 where
    toPathSegments = WR.toPathSegments . (fromIntegral :: Word32 -> Int)
    fromPathSegments = (fromIntegral :: Int -> Word32) `fmap` WR.fromPathSegments

