{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module BlockchainParser ( blocksFromRawStream
                        , longestChainFromRawStream
                        , getLongestChain
                        , getBlockData
                        , Block(..)
                        , BlockData(..)
                        , Transaction(..)
                        , Input(..)
                        , Output(..)
                        , Header(..)
                        , DecodedInputScript(..)
                        , DecodedOutputScript(..)
                        , decodeInputScript
                        , decodeOutputScript
                        , outputScriptToPubKeyHash
                        , Hash160
                        ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Serialize.Get as Get
import qualified Control.Monad as M
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))
import Data.Word (Word32, Word64)
import qualified Data.HashMap.Strict as Map
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.RIPEMD160 as RIPEMD160
import qualified Data.Foldable as F
import qualified ParserUtil as P


type Hash = BS.ByteString
type Hash160 = BS.ByteString
type PublicKey = BS.ByteString
type Signature = BS.ByteString

data Input = Input { _prevTxHash :: !Hash
                   , _prevOutputIndex :: !Int
                   , _inputScriptLength :: !Int
                   , _inputScript :: !BS.ByteString
                   , _sequenceNumber :: !Word32
                   } deriving Show

data Output = Output { _value :: !Word64
                     , _outputScriptLength :: !Int
                     , _outputScript :: !BS.ByteString
                     } deriving Show

data DecodedOutputScript = OutputPayToPubKey !BS.ByteString
                         | OutputPayToPubKeyHash !Hash160
                         deriving Show

data DecodedInputScript = InputSig !BS.ByteString
                        | InputSigPubKey !Signature !PublicKey
                        deriving Show

data Header = Header { _blockVersion :: !Word32
                     , _merkleRootHash :: !Hash
                     , _timestamp :: !Word32
                     , _bits :: !Word32
                     , _nonce:: !Word32
                     } deriving Show

data Transaction = Transaction { _txHash :: !BS.ByteString
                               , _txVersion :: !Word32
                               , _inCount :: !Int
                               , _inputs :: [(Int, Input)]
                               , _outCount :: !Int
                               , _outputs :: [(Int, Output)]
                               , _lockTime :: !Word32
                               } deriving Show

data BlockData = BlockData { _header :: Header
                           , _txCount :: !Int
                           , _transactions :: [Transaction]
                           } deriving Show

data Block = Block { _blockHash :: !BS.ByteString
                   , _prevHash :: !BS.ByteString
                   , _prevBlock' :: Maybe Block
                   , _depth :: !Int
                   , _rawBlock :: !BS.ByteString
                   }


hash160 :: BS.ByteString -> Hash160
hash160 = RIPEMD160.hash . SHA256.hash

decodeOutputScript :: BS.ByteString -> Maybe DecodedOutputScript
decodeOutputScript outputScript = flip P.runGetMaybe outputScript $ (payToPubKey <|> payToPubKeyHash)
  where
    payToPubKey = P.match_ Get.getWord8 0x41 -- OP: push 0x41 bytes
                  *> Get.lookAhead (P.match_ Get.getWord8 0x04)
                  *> (OutputPayToPubKey <$> Get.getBytes 65)
                  <* P.match_ Get.getWord8 0xac -- OP_CHECKSIG
                  <* P.eof
    payToPubKeyHash = P.match_ Get.getWord8 0x76 -- OP_DUP
                      *> P.match_ Get.getWord8 0xa9 -- OP_HASH160         
                      *> P.match_ Get.getWord8 0x14 -- OP: push 0x14 bytes
                      *> (OutputPayToPubKeyHash <$> Get.getBytes 20)
                      <* P.match_ Get.getWord8 0x88 -- OP_EQUALVERIFY
                      <* P.match_ Get.getWord8 0xac -- OP_CHECKSIG
                      <* P.eof

outputScriptToPubKeyHash :: BS.ByteString -> Maybe Hash160
outputScriptToPubKeyHash outputScript = case decodeOutputScript outputScript of
  Just (OutputPayToPubKeyHash hash) -> Just hash
  Just (OutputPayToPubKey pk) -> Just (hash160 pk)
  Nothing -> Nothing

decodeInputScript :: BS.ByteString -> Maybe DecodedInputScript
decodeInputScript inputScript = flip P.runGetMaybe inputScript $ (sigAndPubKey <|> onlySig)
  where sigAndPubKey = do c1 <- Get.getWord8
                          sig <- Get.getBytes (fromIntegral c1)
                          c2 <- Get.getWord8
                          pubKey <- Get.getBytes (fromIntegral c2)
                          P.eof
                          return $ InputSigPubKey sig pubKey
        onlySig = do c1 <- Get.getWord8
                     sig <- Get.getBytes (fromIntegral c1)
                     P.eof
                     return $ InputSig sig

parseBlock :: Get.Get Block
parseBlock = do
  P.match_ Get.getWord32le 0xd9b4bef9
  blockSize <- Get.getWord32le
  rawHeader <- BS.copy <$> (Get.lookAhead $ Get.getBytes 80)
  let blockHash = SHA256.hash . SHA256.hash $! rawHeader
  let prevHash = P.runGet (Get.uncheckedSkip 4 *> Get.getBytes 32) $! rawHeader
  blob <- Get.getBytes (fromIntegral blockSize)
  return $! Block blockHash prevHash Nothing 0 blob
{-# INLINE parseBlock #-}
  
getBlockData :: Block -> BlockData
getBlockData block = flip P.runGet (_rawBlock block) $ do
  header <- parseHeader
  txCount <- parseVariableLengthInteger
  rest <- Get.remaining >>= Get.getBytes
  let txs = take txCount $ P.parseManyLazily parseTransaction rest
  return $ BlockData header txCount txs
{-# INLINE getBlockData #-}

parseHeader :: Get.Get Header
parseHeader = Header
              <$> (P.match Get.getWord32le 0x01 <|> P.match Get.getWord32le 0x02) -- block version
              <* Get.getBytes 32 -- prevHash
              <*> Get.getBytes 32 -- merkleRootHash
              <*> Get.getWord32le -- timestamp
              <*> Get.getWord32le -- bits
              <*> Get.getWord32le -- nonce

parseTransaction :: Get.Get Transaction
parseTransaction = do
  -- transaction version, should be 1, but is 2/garbage for a total of 5 transactions
  (raw, transaction) <- P.parseKeepRaw $ do
    txVersion <- Get.getWord32le
    inCount <- parseVariableLengthInteger
    inputs <- zip [0..] <$> M.replicateM inCount parseInput
    outCount <- parseVariableLengthInteger
    outputs <- zip [0..] <$> M.replicateM outCount parseOutput
    lockTime <- Get.getWord32le
    return $ Transaction "" txVersion inCount inputs outCount outputs lockTime
  return $ transaction { _txHash = SHA256.hash . SHA256.hash $ raw }

parseInput :: Get.Get Input
parseInput = do
  prevHash <- Get.getBytes 32
  prevOutputIndex <- Get.getWord32le
  scriptLength <- parseVariableLengthInteger
  script <- Get.getBytes scriptLength
  sequenceNumber <- Get.getWord32le -- sequence number
  return $ Input prevHash (fromIntegral prevOutputIndex) scriptLength script sequenceNumber

parseOutput :: Get.Get Output
parseOutput = do
  value <- Get.getWord64le
  scriptLength <- parseVariableLengthInteger
  script <- Get.getBytes scriptLength
  return $ Output value scriptLength script

parseVariableLengthInteger :: Get.Get Int
parseVariableLengthInteger = Get.getWord8 >>= go
  where go w | w < 0xfd = return $ fromIntegral w
             | w == 0xfd = fromIntegral <$> Get.getWord16le
             | w == 0xfe = fromIntegral <$> Get.getWord32le
             | w == 0xff = fromIntegral <$> Get.getWord64le
             | otherwise = fail ""

-------------------

-- Attention: blocks assumed to be in order, i.e. if block B references block A by its hash, 
-- A must come before B.
findLongestChainHead :: [Block] -> Block
findLongestChainHead blocks = let (_, Just maxi) = F.foldl go (Map.empty, Nothing) blocks in maxi
  where go (tbl, !maxi) block = let !prev = Map.lookup (_prevHash block) tbl
                                    newBlock = block { _prevBlock' = prev
                                                     , _depth = maybe 0 (succ . _depth) prev
                                                     }
                                    newTbl = Map.insert (_blockHash block) newBlock tbl
                                in (newTbl, Just $! maybe newBlock (maxBy _depth newBlock) maxi)
        maxBy f a b = if f a > f b then a else b

-- given a block, fold it by following the previous block references
foldrBlocks :: (Block -> a -> a) -> a -> Block -> a
foldrBlocks f !start block = case (_prevBlock' block) of
  Nothing -> f block start
  Just prev -> foldrBlocks f (f block start) prev
{-# INLINE foldrBlocks #-}

blocksFromRawStream :: BSL.ByteString -> [Block]
blocksFromRawStream = P.parseManyLazyByteStringLazily parseBlock
{-# INLINABLE blocksFromRawStream #-}

longestChainFromRawStream :: BSL.ByteString -> ([Block], Block)
longestChainFromRawStream = getLongestChain . blocksFromRawStream
{-# INLINABLE longestChainFromRawStream #-}

-- attention: blocks assumed to be in order, i.e. if block B references block A by its hash, 
-- A must come before B.
getLongestChain :: [Block] -> ([Block], Block)
getLongestChain blocks = let newestBlock = findLongestChainHead blocks
                         in (chainBlocks newestBlock, newestBlock)
  where
    chainBlocks = foldrBlocks (:) []
{-# INLINABLE getLongestChain #-}

