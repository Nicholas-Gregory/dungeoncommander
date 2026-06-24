{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import Network.Socket
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (sendAll, recv)
import Control.Applicative (optional)
import System.IO (hReady, stdin, hIsTerminalDevice, hPutStrLn, stderr)
import Control.Monad (join, when)
import DC.Parse (Parser(runParser))
import DC.Json (jsonObject, writeJsonValue, JsonValue (..), FromJson (fromJson), getField, JsonObjectMap, ToJson (toJson))
import System.Random (getStdGen, mkStdGen)
import Text.Read (readMaybe)
import DC.Dice (processExpression)
import qualified Data.Map as M
import Debug.Trace (trace)
import Data.Map (mapMaybe)
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Data.IORef
import Control.Monad.Trans (MonadIO(liftIO))
import DC.Actions
import Data.Traversable (traverse)
import DC.Types 
import DC.Error (AppM)
import Options.Applicative
import DC.Opts
import Data.Foldable (traverse_)

runApp :: RootOptions -> Socket -> AppM Env ()
runApp opts sock = do
  addrPath <- asks socketPath
  json <- getJson sock
  initEntities json

  case opts of
    RootOptions _ verbosity
      (Just (SceneCommand 
        (SceneOptions focus [] Nothing Nothing Nothing))) -> do
          scenes <- getScenes

          traverse_ (printScene verbosity) $ M.keys scenes
    RootOptions _ _
      (Just (SceneCommand 
        (SceneOptions focus _ _ _ 
          (Just (SceneCreate 
            (CreateScene id eName x y)))))) -> do
      let info = EntityInfo { name = eName, children = EntityChildren [] }
      let entity = Scene { entityInfo = info, dimensions = (x, y) }

      saveEntity id entity
    RootOptions _ _ (Just (RollCommand (RollOptions (Just expression) Nothing Nothing Nothing Nothing False False))) -> do
      diceRollResult expression

  when (rootSave opts) $ do
    sock <- refreshSocketConn
    saveJsonToDaemon sock

main :: IO ()
main = withSocketsDo $ do
  gen <- getStdGen
  opts <- execParser rootInfo
  sock <- socket AF_UNIX Stream defaultProtocol
  let initState = GameState
        { currentScene = ""
        , entities = M.empty
        , commits = []
        }
  stateRef <- newIORef initState
  let env = Env
        { socketPath = "/tmp/dc.sock"
        , dbPath = "db.json"
        , state = stateRef
        , gen = gen
        }
  connect sock (SockAddrUnix $ socketPath env)
  result <- runExceptT (runReaderT (runApp opts sock) env)

  case result of
    Left e -> print e
    Right () -> return ()

  return ()