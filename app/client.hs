{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import Network.Socket
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (sendAll, recv)
import Control.Applicative (optional)
import System.IO (hReady, stdin, hIsTerminalDevice, hPutStrLn, stderr)
import Control.Monad (join)
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
import DC.Actions (tooFewArgumentsError, getJson, initCurrentScene, initEntities, saveEntity)
import Data.Traversable (traverse)
import DC.Types (Env(..), GameState (..), Entity (Scene, dimensions, entityInfo), EntityInfo (..), EntityChildren (EntityChildren))
import DC.Error (AppM)
import Options.Applicative
import DC.Opts (rootInfo, Command (SceneCommand), SceneAction (SceneCreate), CreateScene (CreateScene))

runApp :: AppM Env ()
runApp = do
  sock <- liftIO $ socket AF_UNIX Stream defaultProtocol
  addrPath <- asks socketPath
  liftIO $ connect sock (SockAddrUnix addrPath)
  json <- getJson sock
  initCurrentScene json
  initEntities json
  opts <- liftIO $ execParser rootInfo

  case opts of
    SceneCommand (SceneCreate (CreateScene id eName x y)) -> do
      let info = EntityInfo { name = eName, children = EntityChildren [] }
      let entity = Scene { entityInfo = info, dimensions = (x, y) }

      saveEntity id entity

  return ()

main :: IO ()
main = withSocketsDo $ do
  gen <- getStdGen
  let initState = GameState
        { currentScene = ""
        , entities = M.empty
        , gen = gen
        , commits = []
        }
  stateRef <- newIORef initState
  let env = Env
        { socketPath = "/tmp/dc.sock"
        , dbPath = "db.json"
        , state = stateRef
        }
  
  result <- runExceptT (runReaderT runApp env)

  case result of
    Left e -> print e
    Right () -> do
      c <- readIORef $ state env

      print $ entities c

  return ()