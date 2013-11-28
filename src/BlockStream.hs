{-# LANGUAGE OverloadedStrings #-}

module BlockStream ( rawBlockStream
                   ) where

import qualified System.Directory as Dir
import qualified Filesystem.Path.CurrentOS as Path
import Filesystem.Path.CurrentOS ((</>))
import Control.Applicative ((<$>))

import Data.List (sort, isPrefixOf)
import qualified System.IO as IO

import qualified Data.ByteString.Lazy as BSL

import System.IO.Posix.MMap (unsafeMMapFile)

blocksDirectory :: IO IO.FilePath
blocksDirectory = do
  home <- Path.decodeString <$> Dir.getHomeDirectory
  return $ Path.encodeString $ home </> Path.decode ".bitcoin" </> Path.decode "blocks"

blkFiles :: IO [IO.FilePath]
blkFiles = do
  dir <- blocksDirectory
  files <- sort . filter (isPrefixOf "blk") <$> (Dir.getDirectoryContents dir)
  return $ map (\p -> Path.encodeString $ Path.decodeString dir </> Path.decodeString p ) files

rawBlockStream :: IO BSL.ByteString
rawBlockStream = BSL.fromChunks <$> (blkFiles >>= mapM unsafeMMapFile)
