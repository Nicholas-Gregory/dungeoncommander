module Main where

import System.Environment (getArgs)
import DC.Opts (command, cliArg, Option (SubCommand, Arg))
import Network.Socket
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (sendAll, recv)
import Control.Applicative (optional)
import System.IO (hReady, stdin)
import Control.Monad (join)
import DC.Parse (Parser(runParser))
import DC.Json (jsonObject, writeJsonValue, JsonValue (JsonObject))
import System.Random (getStdGen)
import DC.Dice (processExpression)
import qualified Data.Map as M

processCommand :: [Option] -> JsonValue -> Maybe JsonValue
processCommand [Arg "select", Arg key] (JsonObject o) = M.lookup key o


main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  sock <- socket AF_UNIX Stream defaultProtocol
  ready <- hReady stdin
  gen <- getStdGen
  input <- if not ready 
    then do
      connect sock (SockAddrUnix "/tmp/dc.sock")
      sendAll sock $ C.pack "{ \"action\": \"GET\" }"
      r <- recv sock 4096
      return (C.unpack r)
    else getContents

  let state = JsonObject . fst <$> runParser jsonObject input
  let opts = map fst <$> mapM (runParser cliArg) args
  
  let r = (do
        o <- opts
        s <- state

        processCommand o s)
  
  case r of
    Just v -> putStrLn $ writeJsonValue v
    Nothing -> putStrLn "Parsing error, or couldn't find requested entity."

                  
