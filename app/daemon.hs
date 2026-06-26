module Main where

import Network.Socket
import Control.Monad (forever)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import System.IO (readFile, withFile, IOMode (ReadWriteMode), hGetContents, hPutStrLn, stderr)
import Control.Concurrent (forkFinally)
import qualified Data.Map as M
import DC.Json (JsonValue (JsonString, JsonObject), JsonObjectMap, jsonObject, getField, writeJsonValue, ToJson (toJson))
import DC.Parse (Parser(runParser))
import System.Directory (doesFileExist)
import Control.Applicative (Alternative(empty))
import DC.Types
import Data.IORef (newIORef)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except (runExceptT)

-- performAction :: Socket -> C.ByteString -> String -> JsonValue -> IO ()
-- performAction conn _ "get" (JsonString "all") = do
--   contents <- C.readFile "db.json"
--   sendAll conn contents
-- performAction conn msg "save" entities = do
--   let newMap = M.insert "entities" entities initialJson
--   C.writeFile "db.json" $ C.pack $ writeJsonValue $ JsonObject newMap
--   -- print "here"
--   sendAll conn $ C.pack "SUCCESS"
-- performAction conn msg "focus" entityIds = do
--   oldRaw <- C.readFile "db.json"
--   let oldJson = runParser jsonObject
-- performAction _ _ _ _ = hPutStrLn stderr "unkown action"


-- handleClient :: Socket -> IO ()
-- handleClient conn = do
--   msg <- recv conn 4096
--   print msg
--   case runParser jsonObject (C.unpack msg) of
--     Right (r, "") -> case performAction conn msg <$> getField "action" r <*> getField "payload" r of
--       Right result -> result
--       Left e -> print e
--     Right (_, s) -> putStrLn $ "server daemon did not parse entire client message. leftover: " <> s
--     Left e -> print e

handleClient :: Socket -> DaemonM ()
handleClient conn = do
  return ()

main :: IO ()
main = withSocketsDo $ do
  sock <- socket AF_UNIX Stream 0
  let initState = DState {
    focusedEntities = [],
    recentEdits = []
  }
  stateRef <- newIORef initState
  let env = DEnv {
    dSocketPath = "/tmp/dc.sock",
    dDbPath = "db.json",
    dState = stateRef,
    dConn = sock
  }
  bind sock (SockAddrUnix "/tmp/dc.sock")
  listen (dConn env) 1

  forever $ do
    (conn, _) <- accept sock
    forkFinally (runExceptT (runReaderT (handleClient conn) env)) (\r -> close conn)