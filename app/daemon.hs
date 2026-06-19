module Main where

import Network.Socket
import Control.Monad (forever)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import System.IO (readFile, withFile, IOMode (ReadWriteMode), hGetContents, hPutStrLn)
import Control.Concurrent (forkFinally)
import qualified Data.Map as M
import DC.Json (JsonValue (JsonString, JsonObject), JsonObjectMap, jsonObject, getField, writeJsonValue)
import DC.Parse (Parser(runParser))
import System.Directory (doesFileExist)
import Control.Applicative (Alternative(empty))

performAction :: Socket -> String -> JsonValue -> IO ()
performAction conn "get" (JsonString "all") = do
  contents <- C.readFile "db.json"
  sendAll conn contents
performAction _ "setScene" v = do
  contents <- C.readFile "db.json"

  let parseResult = (case runParser jsonObject $ C.unpack contents of
        Left e -> Left e
        Right (oldMap, _) -> if M.member "currentScene" oldMap
          then Right $ M.adjust (const v) "currentScene" oldMap
          else Right $ M.insert "currentScene" v oldMap)
  
  case parseResult of
    Right newMap -> writeFile "db.json" $ writeJsonValue (JsonObject newMap)
    Left e -> print e

performAction _ _ _ = putStrLn "unrecognized action"

handleClient :: Socket -> IO ()
handleClient conn = do
  msg <- recv conn 1024

  case runParser jsonObject (C.unpack msg) of
    Right (r, "") -> case performAction conn <$> getField "action" r <*> getField "payload" r of
      Right result -> result
      Left e -> print e
    Right (_, s) -> putStrLn $ "server daemon did not parse entire client message. leftover: " <> s
    Left e -> print e

main :: IO ()
main = withSocketsDo $ do
  sock <- socket AF_UNIX Stream 0

  bind sock (SockAddrUnix "/tmp/dc.sock")
  listen sock 1
  
  forever $ do
    (conn, _) <- accept sock
    forkFinally (handleClient conn) (\_ -> close conn)