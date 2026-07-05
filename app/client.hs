{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import Network.Socket
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (sendAll, recv)
import Control.Applicative (optional)
import System.IO (hReady, stdin, hIsTerminalDevice, hPutStrLn, stderr, hPrint)
import Control.Monad (join, when, unless)
import DC.Parse (Parser(runParser))
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
import DC.Error (AppM, AppError (AppError), throwBaseError, ErrorDetail (OtherError))
import Options.Applicative ( execParser )
import DC.Opts
import Data.Foldable (traverse_)
import Data.Either (rights)
import Data.Maybe (fromMaybe, isJust)
import Data.Function ((&))
import Control.Applicative
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BS

processCommand :: KM.KeyMap Entity -> EntityAction -> AppM Env ()
processCommand _ (SceneA (SceneCreate (CreateScene id sName x y))) = do
    let info = EntityInfo { name = sName, children = EntityChildren [] }
    let entity = Scene { entityInfo = info, dimensions = (x, y) }

    saveEntity id entity
    addEntityToOutputEntities id entity
processCommand scenes (SceneA (SceneUpdate (UpdateScene nId sName x y))) = do
  traverse_ (\(k, s) -> do
    let id = K.toString k
    let newId = fromMaybe id nId
    let newName = fromMaybe (name $ entityInfo s) sName
    let newX = fromMaybe (fst $ dimensions s) x
    let newY = fromMaybe (snd $ dimensions s) y
    let newInfo = (entityInfo s) { name = newName }
    let newScene = Scene {
      entityInfo = newInfo,
      dimensions = (newX, newY)
    } 

    saveEntity newId newScene
    addEntityToOutputEntities newId newScene

    when (id /= newId) $ do 
      deleteEntity id
      removeEntityFromOutputEntities id) $ KM.toList scenes
processCommand scenes (SceneA SceneDelete) = traverse_ (\(k, _) -> deleteEntity $ K.toString k) $ KM.toList scenes

runApp :: RootOptions -> Socket -> AppM Env ()
runApp opts sock = do
  addrPath <- asks socketPath
  json <- getJson sock
  initEntities json
  entities <- getEntities
  setOutputEntities entities

  case opts of
    RootOptions _ verbosity _ _ (Just rootCommand) -> 
          let baseEntities e = case rootCommand of
                (SceneCommand {}) -> KM.filter (\case
                  Scene {} -> True
                  _ -> False) e
              opt = case rootCommand of
                (SceneCommand opt) -> opt
              filteringCondition = case opt of
                (SceneOptions ids filterX filterY _) -> not $ null ids || isJust (filterX <|> filterY)
              ids = case opt of
                (SceneOptions ids _ _ _) -> ids
              filters = case opt of
                (SceneOptions _ filterX filterY _) ->
                  [ \s -> maybe True (\fx -> fx == fst (dimensions s)) filterX
                  , \s -> maybe True (\fy -> fy == snd (dimensions s)) filterY]
          in if filteringCondition
            then do
              -- CLI entity selection, filtering by command options
              entities <- getEntitiesByIds ids
              let filteredScenes = entities
                    & baseEntities
                    & KM.filter (\e -> all ($ e) filters)

              setOutputEntities filteredScenes
              case entityCommand opt of
                Just c -> processCommand filteredScenes c
                Nothing -> traverse_ (printScene verbosity . K.toString) $ KM.keys filteredScenes
            else do
              isTerm <- asks isTerm
        
              if not isTerm
                then do
                  -- Pipe input, entities coming from stdin
                  scenes <- getScenes

                  setOutputEntities entities
                  case entityCommand opt of
                    Just c -> processCommand scenes c
                    Nothing -> traverse_ (printScene verbosity . K.toString) $ KM.keys scenes
                else do
                  -- No CLI options, no pipe input, assuming command applies to all entities or focused entities
                  focus <- getFocusFromDaemon
                  -- liftIO $ hPrint stderr focus
                  entities <- if null focus
                    then getEntities
                    else getEntitiesByIds focus
                  let e = baseEntities entities
                  
                  setOutputEntities entities
                  case entityCommand opt of
                    Just c -> processCommand e c
                    Nothing -> traverse_ (printScene verbosity . K.toString) $ KM.keys e

  when (rootSave opts) $ do
    sock <- refreshSocketConn
    saveJsonToDaemon sock
  
  when (focus opts) $ do
    sock <- refreshSocketConn
    stateRef <- asks state
    gameState <- liftIO $ readIORef stateRef
    let currentOutput = output gameState
    let outputEntities = outEntities currentOutput

    sendFocusToDaemon sock (map (K.toString . fst) (KM.toList outputEntities))

  unless (noOutput opts) $ do
    stateRef <- asks state
    gameState <- liftIO $ readIORef stateRef
    let out = output gameState
    case outError out of
      Just e -> throwError e
      Nothing -> do
        let outEntitiesJson = JSON.toJSON (outEntities out)
        let actionsJson = JSON.toJSON (outActions out)
        let outJson = JSON.toJSON $ M.fromList [
              ("entities", outEntitiesJson),
              ("actions", actionsJson)
              ]

        liftIO $ putStrLn $ C.unpack $ BS.toStrict $ JSON.encode outJson

main :: IO ()
main = withSocketsDo $ do
  gen <- getStdGen
  opts <- execParser rootInfo
  sock <- socket AF_UNIX Stream defaultProtocol
  isTerm <- hIsTerminalDevice stdin
  let initState = GameState
        { currentScene = ""
        , entities = KM.empty
        , output = Output { outEntities = KM.empty, outActions = KM.empty, outError = Nothing }
        , commits = []
        }
  stateRef <- newIORef initState
  let env = Env
        { socketPath = "/tmp/dc.sock"
        , dbPath = "db.json"
        , state = stateRef
        , gen = gen
        , isTerm = isTerm
        }
  connect sock (SockAddrUnix $ socketPath env)
  result <- runExceptT (runReaderT (runApp opts sock) env)

  case result of
    Left e -> print e
    Right () -> return ()