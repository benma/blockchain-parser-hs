module ParserUtil ( runGet
                  , runGetMaybe
                  , parseManyLazily
                  , parseManyLazyByteStringLazily
                  , parseKeepRaw
                  , match
                  , match_
                  , eof
                  ) where

import qualified Control.Monad as M
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Serialize.Get as Get
import Control.Applicative ((<$>), (*>), (<*>), pure)

runGet :: Get.Get a -> BS.ByteString -> a
runGet p s = either error id $ Get.runGet p s 
{-# INLINE runGet #-}

runGetMaybe :: Get.Get a -> BS.ByteString -> Maybe a
runGetMaybe p s = either (const Nothing) Just $ Get.runGet p s 
{-# INLINE runGetMaybe #-}

parseManyLazily :: Get.Get a -> BS.ByteString -> [a]
parseManyLazily p s  | BS.null s = []
                     | otherwise = case Get.runGetState p s 0 of
  Right (r, rest) -> r : parseManyLazily p rest
  Left _ -> []
{-# INLINE parseManyLazily #-}

parseManyLazyByteStringLazily :: Get.Get a -> BSL.ByteString -> [a]
parseManyLazyByteStringLazily p = concatMap (parseManyLazily p) . BSL.toChunks

match :: Eq a => Get.Get a -> a -> Get.Get a
match p test = do
  result <- p
  if result == test
    then return result
    else fail ""
{-# INLINE match #-}

match_ :: Eq a => Get.Get a -> a -> Get.Get ()
match_ p test = match p test *> pure ()
{-# INLINE match_ #-}

parseKeepRaw :: Get.Get a -> Get.Get (BS.ByteString, a)
parseKeepRaw g = do
  (len, r) <- Get.lookAhead $ do
                (res,after) <- Get.lookAhead $ (,) <$> g <*> Get.remaining
                total <- Get.remaining
                return (total-after, res)
  bs <- Get.getBytes len
  return (bs, r)
{-# INLINE parseKeepRaw #-}

eof :: Get.Get ()
eof = do
  empty <- Get.isEmpty
  M.unless empty $ fail "expected eof"
{-# INLINE eof #-}
