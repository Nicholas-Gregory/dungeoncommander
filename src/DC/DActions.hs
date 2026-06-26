{-# LANGUAGE LambdaCase #-}

module DC.DActions (
  receiveClient,
  readDbFile,
  readDb,
  writeDb,
  sendDb,
  saveEntities,
  focusEntities
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Network.Socket
import DC.Types
import DC.Json
import DC.Parse
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (asks)
import DC.Error (throwBaseError, ErrorDetail (ParseError))
import System.Directory (doesFileExist)
import Data.Maybe (catMaybes, mapMaybe)

initialJson :: M.Map String JsonValue
initialJson = M.fromList 
  [ ("entities", toJson (M.empty :: M.Map String JsonValue))
  , ("focus", toJson ([] :: [String]))]

receiveClient :: DaemonM JsonObjectMap
receiveClient = do
  conn <- asks dConn
  msg <- liftIO $ recv conn 4096

  case runParser jsonObject (C.unpack msg) of
    Right (r, "") -> return r
    _ -> throwBaseError $ ParseError "Daemon encountered parsing error in message received from client"

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

      return $ writeJsonValue $ JsonObject initialJson

readDb :: DaemonM JsonObjectMap
readDb = do
  file <- readDbFile

  case runParser jsonObject file of
    Right (r, "") -> return r
    _ -> throwBaseError $ ParseError "Daemon encountered parsing error in raw database file"

writeDb :: JsonObjectMap -> DaemonM ()
writeDb json = do
  let jsonStr = writeJsonValue $ JsonObject json

  liftIO $ C.writeFile "db.json" $ C.pack jsonStr

sendDb :: DaemonM ()
sendDb = do
  conn <- asks dConn
  contents <- readDbFile

  liftIO $ sendAll conn $ C.pack contents 

saveEntities :: JsonValue -> DaemonM ()
saveEntities newEntities = do
  oldMap <- readDb
  let newMap = M.insert "entities" newEntities oldMap

  writeDb newMap

focusEntities :: [String] -> DaemonM ()
focusEntities fEntities = do
  db <- readDb

  case M.lookup "focus" db of
    Just (JsonArray f) -> do
      let newF = fEntities <> mapMaybe (\case 
            JsonString s -> Just s
            _ -> Nothing) f

      writeDb $ M.insert "focus" (toJson newF) db
    _ -> undefined

