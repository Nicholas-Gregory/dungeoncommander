module Main where

import Network.Socket
import Control.Monad (forever)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import System.IO (readFile)
import Control.Concurrent (forkFinally)
import qualified Data.Map as M
import DC.Json (JsonValue (JsonString, JsonObject), JsonObjectMap, jsonObject, getField)
import DC.Parse (Parser(runParser))
import System.Directory (doesFileExist)

performAction :: Socket -> String -> JsonValue -> IO ()
performAction conn "get" (JsonString "all") = do
  contents <- C.readFile "db.json"
  sendAll conn contents

performAction _ _ _ = putStrLn "unrecognized action"

handleClient :: Socket -> IO ()
handleClient conn = do
  msg <- recv conn 1024

  case runParser jsonObject (C.unpack msg) of
    Just (r, "") -> case performAction conn <$> getField "action" r <*> getField "payload" r of
      Just result -> result
      Nothing -> putStrLn "malformed request"
    Just (_, s) -> putStrLn $ "server daemon did not parse entire message. leftover: " <> s
    Nothing -> putStrLn "server daemon encountered parsing error"

main :: IO ()
main = withSocketsDo $ do
  sock <- socket AF_UNIX Stream 0

  bind sock (SockAddrUnix "/tmp/dc.sock")
  listen sock 1
  
  forever $ do
    (conn, _) <- accept sock
    forkFinally (handleClient conn) (\_ -> close conn)