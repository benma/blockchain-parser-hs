import Data.Maybe (fromJust)
import Data.Serialize (encode, decode)
import qualified Options.Applicative as O
import qualified Data.Decimal as Dec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Time.LocalTime as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Foldable as F

import Data.Monoid ((<>))
import Control.Applicative ((<$>), (<*>), pure)
import BitcoinAddress (addressToHash160)
import BlockStream (rawBlockStream)
import BlockchainParser
import Examples

type StoreType = ([BS.ByteString], Maybe BlocksHint)

load :: FilePath -> IO StoreType
load filename = either error id <$> decode <$> BS.readFile filename

save :: FilePath -> StoreType -> IO ()
save filename x = BS.writeFile filename $ encode x


data Options = Options { _optFilename :: String }
             deriving Show

data Commands = CommandOnce
              | CommandCreate Options
              | CommandView Options
              deriving Show

main :: IO ()
main = do
  opts <- O.execParser optsParserInfo
  (addresses, blocksHint) <- case opts of
    CommandView (Options filename) -> do
      putStrLn "Load from file"
      load filename
    _ -> do
      putStrLn "Getting addresses from stdin"
      addresses <- filter (not . BS.null) <$> BSC.lines <$> BSC.getContents
      return (addresses, Nothing)   

  (blocks, latestBlock) <- longestChainFromRawStream <$> rawBlockStream

  putStrLn "Last block:"
  putStrLn $ "Depth: " ++ show (_depth latestBlock)
  time <- prettyTime $ _timestamp $ _header $ getBlockData latestBlock
  putStrLn $ "Time: " ++ time

  let addressHashes = map (fromJust . addressToHash160) addresses
      conv v = show $ Dec.Decimal 8 v
      (myBalances, newBlocksHint) = getBalances blocks blocksHint addressHashes
  F.forM_ (zip addresses myBalances) $ \(address, balance) -> putStrLn (BSC.unpack address ++ ": " ++ conv balance)
  putStrLn $ "Total: " ++ (conv $ sum myBalances)

  let save' filename = save filename (addresses, Just newBlocksHint)
  case opts of
    CommandCreate (Options filename) -> save' filename
    CommandView (Options filename) -> save' filename
    CommandOnce -> return ()

  where
    prettyTime timestamp = do
        tz <- Time.getCurrentTimeZone
        return $ show $ Time.utcToLocalTime tz $ Time.posixSecondsToUTCTime $ realToFrac timestamp
    optsParserInfo = O.info (O.helper <*> optsParser) (O.progDesc "Example usage of the blockchain parser")
    optsParser = O.subparser (O.command "once" optsOnceInfo
                              <> O.command "create" optsCreateInfo
                              <> O.command "view" optsViewInfo)
      where
        optsOnceInfo = O.info (pure CommandOnce) (O.progDesc "View balance with addresses taken from stdin.")
        optsCreateInfo = O.info (CommandCreate <$> optsCreateParser) (O.progDesc "Create watch only wallet with addresses taken from stdin.")
        optsViewInfo = O.info (CommandView <$> optsViewParser) (O.progDesc "View balance of watch only wallet.")
        optsCreateParser = Options <$> O.argument O.str (O.metavar "FILE")
        optsViewParser = optsCreateParser
