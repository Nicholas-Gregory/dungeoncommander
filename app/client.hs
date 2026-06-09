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
import DC.Json (jsonObject, writeJsonValue, JsonValue (..))
import System.Random (getStdGen)
import Text.Read (readMaybe)
import DC.Dice (processExpression)
import qualified Data.Map as M
import Debug.Trace (trace)

filterEquals :: JsonValue -> String -> JsonValue -> JsonValue
filterEquals (JsonObject json) k v = JsonObject $ M.filter (\(JsonObject x) -> case M.lookup k x of 
  Just v' -> v == v'
  Nothing -> False) json


processCommand :: [Option] -> JsonValue -> Maybe JsonValue
processCommand [Arg "select", Arg key] (JsonObject o) = M.lookup key o
processCommand (Arg "filter":xs) (JsonObject o) = case xs of
  [Arg k, Arg v] -> Just $ filterEquals (JsonObject o) k (JsonString v)
  [Flag "equals", Arg k, Arg v] -> Just $ filterEquals (JsonObject o) k (JsonString v)
  [Switch 'e', Arg k, Arg v] -> Just $ filterEquals (JsonObject o) k (JsonString v)
processCommand [Arg "set", Arg k, Arg v] (JsonObject o) =
  let newVal = case (readMaybe v :: Maybe Int) of
        Just n -> JsonNumber n
        Nothing -> case v of
          "true" -> JsonBool True
          "false" -> JsonBool False
          _ -> JsonString v
  in Just $ JsonObject $ M.adjust (const newVal) k o


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
  
  let r = (do
        o <- opts
        s <- state

        processCommand o s)
  
  case r of
    Just v -> putStrLn $ writeJsonValue v
    Nothing -> putStrLn "Parsing error, or couldn't find requested entity."

                  
