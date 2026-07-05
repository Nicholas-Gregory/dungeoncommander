{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket
import Control.Monad (forever)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import System.IO (readFile, withFile, IOMode (ReadWriteMode), hGetContents, hPutStrLn, stderr)
import Control.Concurrent (forkFinally)
import qualified Data.Map as M
import DC.Parse (Parser(runParser))
import System.Directory (doesFileExist)
import Control.Applicative (Alternative(empty))
import DC.Types
import Data.IORef (newIORef)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import DC.DActions (receiveClient, readDbFile, sendDb, saveEntities, focusEntities, sendFocusedEntities)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import DC.Error (throwBaseError, ErrorDetail (ParseError, JsonValidationError))
import Data.Maybe (mapMaybe)
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Aeson.KeyMap as KM

performAction :: String -> JSON.Value -> DaemonM ()
performAction "get" "all" = sendDb
performAction "get" "focus" = sendFocusedEntities
performAction "save" entities = do
  conn <- asks dConn
  saveEntities entities

  liftIO $ sendAll conn $ C.pack "SUCCESS"
performAction "focus" (JSON.Array entityIds) = case traverse (\case
  JSON.String s -> Just s
  _ -> Nothing) entityIds of
    Just a -> do
      conn <- asks dConn
      focusEntities $ map T.unpack $ V.toList a

      liftIO $ sendAll conn $ C.pack "SUCCESS"
    Nothing -> throwBaseError $ JsonValidationError "Daemon received something other than list of strings for focus command"
performAction _ _ = throwBaseError $ JsonValidationError "Daemon received request from client with unknown 'action' and/or 'payload' fields"

handleClient :: DaemonM ()
handleClient = do
  cJson <- receiveClient

  case (KM.lookup "action" cJson, KM.lookup "payload" cJson) of
    (Just (JSON.String action), Just payload) -> performAction (T.unpack action) payload
    _ -> throwBaseError $ ParseError "Daemon received request from client without 'action' and/or 'payload' fields"

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
    let clientEnv = env { dConn = conn }

    forkFinally (runExceptT (runReaderT handleClient clientEnv)) (\r -> do
      case r of
        Left e -> print e
        Right (Left e) -> print e
        Right (Right ()) -> return ()
      close conn)