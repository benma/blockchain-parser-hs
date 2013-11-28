{-# LANGUAGE DeriveGeneric #-}

module Examples ( BlocksHint(..)
                , getBalances
                ) where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import qualified Control.Monad.ST as ST
import qualified Data.STRef as ST
import qualified Data.Foldable as F
import qualified Control.Monad as M
import qualified Data.HashMap.Strict as Map
import Control.Applicative ((<$>))
import BlockchainParser


data BlocksHint = BlocksHint { _relevantBlocks :: [Int], _seenUpTo :: Int }
                deriving Generic

instance Serialize BlocksHint -- Generic 

getBalances :: [Block] -> Maybe BlocksHint -> [Hash160] -> ([Integer], BlocksHint)
getBalances blocks blocksHint addresses = ST.runST $ do
  oMap <- ST.newSTRef Map.empty
  maxDepth <- ST.newSTRef 0
  F.forM_ blocks $ \block -> do
    ST.modifySTRef' maxDepth (max (_depth block))

    let cond = case blocksHint of
          Nothing -> True
          Just (BlocksHint relevantBlocks seenUpTo) -> _depth block > seenUpTo || _depth block `elem` relevantBlocks
    
    M.when cond $ do
      F.forM_ (_transactions $ getBlockData block) $ \tx -> do
        F.forM_ (_inputs tx) $ \(_, input) -> ST.modifySTRef' oMap (Map.delete (_prevTxHash input, _prevOutputIndex input))
        F.forM_ (_outputs tx) $ \(idx, output) -> case outputScriptToPubKeyHash $ _outputScript output of
          Nothing -> return ()
          Just recipientHash -> F.forM_ (filter (==recipientHash) addresses) $ \address -> do
            ST.modifySTRef' oMap (Map.insert (_txHash tx, idx) (_depth block, (address, _value output)))
  
  values <- Map.elems <$> ST.readSTRef oMap
  maxDepth' <- ST.readSTRef maxDepth
  let byAddress address = F.foldl' (+) 0 . map (fromIntegral . snd . snd) . filter ((==address) . fst . snd) $ values
  let blockIdxs = map fst values
  return $ (map byAddress addresses, BlocksHint blockIdxs maxDepth')
