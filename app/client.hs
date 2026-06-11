module Main where

import System.Environment (getArgs)
import DC.Opts (command, cliArg, Option (..))
import Network.Socket
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (sendAll, recv)
import Control.Applicative (optional)
import System.IO (hReady, stdin, hIsTerminalDevice)
import Control.Monad (join)
import DC.Parse (Parser(runParser))
import DC.Json (jsonObject, writeJsonValue, JsonValue (..), FromJson (fromJson))
import System.Random (getStdGen)
import Text.Read (readMaybe)
import DC.Dice (processExpression)
import qualified Data.Map as M
import Debug.Trace (trace)
import DC.Entity(Entity) 



main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  sock <- socket AF_UNIX Stream defaultProtocol
  isTerm <- hIsTerminalDevice stdin
  gen <- getStdGen
  input <- if isTerm
    then do
      connect sock (SockAddrUnix "/tmp/dc.sock")
      sendAll sock $ C.pack "{ \"action\": \"GET\" }"
      r <- recv sock 4096
      return (C.unpack r)
    else getContents

  let state = JsonObject . fst <$> runParser jsonObject input
  let opts = map fst <$> mapM (runParser cliArg) args
  


