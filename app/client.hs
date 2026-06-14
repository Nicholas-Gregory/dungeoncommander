{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import DC.Opts (command, cliArg, Option (..))
import Network.Socket
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (sendAll, recv)
import Control.Applicative (optional)
import System.IO (hReady, stdin, hIsTerminalDevice, hPutStrLn, stderr)
import Control.Monad (join)
import DC.Parse (Parser(runParser))
import DC.Json (jsonObject, writeJsonValue, JsonValue (..), FromJson (fromJson), getField, JsonObjectMap)
import System.Random (getStdGen, mkStdGen)
import Text.Read (readMaybe)
import DC.Dice (processExpression)
import qualified Data.Map as M
import Debug.Trace (trace)
import DC.Entity(Entity (Scene)) 
import Data.Map (mapMaybe)
import DC.Game (AppM, GameState (..), Env (..))
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Data.IORef

processCommand :: Socket -> [Option] -> AppM ()
processCommand sock opts = undefined

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  sock <- socket AF_UNIX Stream defaultProtocol
  isTerm <- hIsTerminalDevice stdin
  gen <- getStdGen
  input <- if isTerm
    then do
      connect sock (SockAddrUnix "/tmp/dc.sock")
      sendAll sock $ C.pack "{ \"action\": \"get\", \"payload\": \"all\"}"
      r <- recv sock 4096
      return (C.unpack r)
    else getContents
  let opts = map fst <$> mapM (runParser cliArg) args


  case runParser jsonObject input of
    Left e -> putStrLn $ "client recieved invalid JSON from server daemon: " <> e
    Right (json, _) -> do
      case opts of
        Right [Arg "scene", Arg sceneIdentifier] -> do
          connect sock (SockAddrUnix "/tmp/dc.sock")

          sendAll sock $ C.pack $ "{ \"action\": \"setScene\", \"payload\": \"" <> sceneIdentifier <> "\" }"
        _ -> do
          case getField "currentScene" json :: Maybe String of
            Nothing -> case M.lookup "entities" json of
              Nothing -> putStrLn "client recieved JSON without top-level \"entities\" entry"
              Just (JsonObject entityMap) -> do
                putStrLn "hi"
                let scenes = M.filter (\case
                     (JsonObject o) -> maybe False (== "scene") (getField "type" o)
                     _ -> False) entityMap

                putStrLn "No current active scene. Use \"dc scene\" passing the name or ID of the scene you want to be in."
                putStrLn "Currently saved scenes:"

          --       -- extractName :: JsonValue -> Maybe String
                let extractName (JsonObject o) = do
                      infoVal <- M.lookup "entityInfo" o
                      case infoVal of
                        JsonObject infoMap -> do
                          nameVal <- M.lookup "name" infoMap
                          case nameVal of
                            JsonString s -> Just s
                            _ -> Nothing
                        _ -> Nothing
                    extractName _ = Nothing

          --       -- collect (id, name) pairs and print them
                let namedList = [(k, n) | (k, v) <- M.toList scenes, Just n <- [extractName v]]
                mapM_ (\(k, n) -> putStrLn $ "ID: " ++ k ++ ", Name: " ++ n) namedList
            Just sceneIdentifier -> do
              case M.lookup "entities" json of
                Nothing -> hPutStrLn stderr "client recieved JSON without top-level \"entities\" entry"
                Just (JsonObject entities) -> do
                  case (fromJson =<< M.lookup sceneIdentifier entities) :: Maybe Entity of
                    Nothing -> hPutStrLn stderr "Current scene not found in entities list"
                    Just currentScene -> do
                      connect sock (SockAddrUnix "/tmp/dc.sock")
                      case opts of
                        Left e -> putStrLn $ "Client CLI parsing error: " <> e
                        Right o -> do
                          let gen = mkStdGen 100
                          let gameState = GameState {commits=[], scene=currentScene, gen=gen}
                          stateRef <- newIORef gameState
                          let env = Env { socketPath="/tmp/dc.sock", dbPath="db.json", state=stateRef}

                          result <- runReaderT (runExceptT $ processCommand sock o) env

                          case result of
                            Left e -> hPutStrLn stderr $ "Game logic error: " <> e
                            Right () -> undefined --TODO: Handle JSON ouput, stderr user output, and sending result to daemon






