module BitcoinAddress ( addressToHash160
                      ) where

import qualified Crypto.Hash.SHA256 as SHA256
import Control.Monad (guard)
import Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as Map
import Data.Bits (shiftR)
import Data.Maybe (fromJust)
import Data.List (unfoldr)
import Data.Word (Word8)
import Data.Char (ord)

checkSum :: BS.ByteString -> BS.ByteString
checkSum = BS.take 4 . SHA256.hash . SHA256.hash

b58String :: String
b58String = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

b58Data :: BS.ByteString
b58Data = BS.pack $ map (fromIntegral . ord) b58String

b58Data' :: Map.HashMap Word8 Int
b58Data' = Map.fromList $ zip (BS.unpack b58Data) [0..57]

b58 :: Word8 -> Word8
b58 i = BS.index b58Data (fromIntegral i)

b58' :: Word8 -> Maybe Word8
b58' w = fromIntegral <$> Map.lookup w b58Data'

integerToBS :: Integer -> BS.ByteString
integerToBS 0 = BS.pack [0]
integerToBS i
    | i > 0 = BS.pack $ reverse $ unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
    where f 0 = Nothing
          f x = Just $ (fromInteger x :: Word8, x `shiftR` 8)
          
decodeBase58 :: BS.ByteString -> Maybe BS.ByteString
decodeBase58 bs = r >>= return . BS.append prefix
  where (z,b) = BS.span (== (b58 0)) bs
        prefix = BS.map (fromJust . b58') z -- preserve leading 1's
        r | BS.null b = Just BS.empty
          | otherwise = integerToBS <$> foldl f (Just 0) (BS.unpack b)
        f i w = do
            n <- fromIntegral <$> b58' w
            p <- i
            return $ p*58 + n

decodeBase58Check :: BS.ByteString -> Maybe BS.ByteString
decodeBase58Check bs = do
  rs <- decodeBase58 bs
  let (res,chk) = BS.splitAt (BS.length rs - 4) rs
  guard $ chk == checkSum res
  return res

addressToHash160 :: BS.ByteString -> Maybe BS.ByteString
addressToHash160 s = BS.drop 1 <$> decodeBase58Check s
