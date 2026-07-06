{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module DC.DActions (
  receiveClient,
  readDbFile,
  readDb,
  writeDb,
  sendDb,
  saveEntities,
  focusEntities,
  sendFocusedEntities
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Network.Socket
import DC.Types
import DC.Parse
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (asks)
import DC.Error (throwBaseError, ErrorDetail (ParseError, JsonValidationError))
import System.Directory (doesFileExist)
import Data.Maybe (catMaybes, mapMaybe)
import Debug.Trace (trace)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Vector as V

initialJson :: KM.KeyMap JSON.Value
initialJson = KM.fromList 
  [ ("entities", JSON.toJSON (KM.empty :: KM.KeyMap JSON.Value))
  , ("focus", JSON.toJSON ([] :: [String]))]

receiveClient :: DaemonM (KM.KeyMap JSON.Value)
receiveClient = do
  conn <- asks dConn
  msg <- liftIO $ recv conn 4096

  case JSON.eitherDecode (BS.fromStrict msg) :: Either String JSON.Value of
    Right (JSON.Object r) -> return r
    _ -> throwBaseError $ ParseError "Daemon encountered parsing error in message received from client (top level is not an object)"

readDbFile :: DaemonM String
readDbFile = do
  path <- asks dDbPath
  exists <- liftIO $ doesFileExist path

  if exists
    then do
      contents <- liftIO $ C.readFile path

      return $ C.unpack contents
    else do
      writeDb initialJson

      return $ C.unpack $ BS.toStrict $ JSON.encode initialJson

readDb :: DaemonM (KM.KeyMap JSON.Value)
readDb = do
  file <- readDbFile

  case JSON.eitherDecode $ BS.fromStrict $ C.pack file :: Either String JSON.Value of
    Right (JSON.Object r) -> return r
    _ -> throwBaseError $ ParseError "Daemon encountered parsing error in raw database file (top level is not an object)"

writeDb :: KM.KeyMap JSON.Value -> DaemonM ()
writeDb json = do
  let jsonStr = JSON.encode json

  liftIO $ C.writeFile "db.json" $ BS.toStrict jsonStr

sendDb :: DaemonM ()
sendDb = do
  conn <- asks dConn
  contents <- readDbFile

  liftIO $ sendAll conn $ C.pack contents 

saveEntities :: JSON.Value -> DaemonM ()
saveEntities newEntities = do
  oldMap <- readDb
  let newMap = KM.insert "entities" newEntities oldMap

  writeDb newMap

focusEntities :: [String] -> DaemonM ()
focusEntities fEntities = do
  db <- readDb

  case KM.lookup "focus" db of
    Just (JSON.Array f) -> do
      let newF = fEntities <> mapMaybe (\case 
            JSON.String s -> Just $ T.unpack s
            _ -> Nothing) (V.toList f)

      writeDb $ KM.insert "focus" (JSON.toJSON newF) db
    _ -> throwBaseError $ JsonValidationError "Daemon could not find focused entities"

sendFocusedEntities :: DaemonM ()
sendFocusedEntities = do
  db <- readDb
  conn <- asks dConn

  case KM.lookup "focus" db of
    Just f -> liftIO $ sendAll conn $ BS.toStrict $ JSON.encode f
    _ -> throwBaseError $ JsonValidationError "Daemon could not find focused entities"