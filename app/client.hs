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
import Options.Applicative ( execParser )
import DC.Opts
import Data.Foldable (traverse_)
import Data.Either (rights)

runApp :: RootOptions -> Socket -> AppM Env ()
runApp opts sock = do
  addrPath <- asks socketPath
  json <- getJson sock
  initEntities json

  case opts of
    RootOptions _ verbosity _
      (Just (SceneCommand 
        (SceneOptions [] Nothing Nothing False False False Nothing))) -> do
          scenes <- getScenes
          stateRef <- asks state
          gameState <- liftIO $ readIORef stateRef
          let oldOutput = output gameState
          let newOutEntities = scenes
          let newOutput = oldOutput { outEntities = newOutEntities }

          liftIO $ atomicModifyIORef' stateRef $ \st ->
            (st { output = newOutput}, ())
          traverse_ (printScene verbosity) $ M.keys scenes
          

    RootOptions _ verbosity _
      (Just (ActorCommand
        (ActorOptions [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing False False False False False False Nothing))) -> do
          actors <- getActors

          traverse_ (printActor verbosity) $ M.keys actors
    RootOptions _ verbosity _
      (Just (ObjectCommand 
        (ObjectOptions [] Nothing Nothing Nothing Nothing Nothing Nothing))) -> do
          objects <- getObjects

          traverse_ (printObject verbosity) $ M.keys objects
    RootOptions _ verbosity _
      (Just (TrapCommand 
        (TrapOptions [] Nothing Nothing Nothing Nothing Nothing Nothing))) -> do
          traps <- getTraps

          traverse_ (printTrap verbosity) $ M.keys traps
    RootOptions _ verbosity _
      (Just (ItemCommand 
        (ItemOptions [] Nothing Nothing Nothing))) -> do
          items <- getItems

          traverse_ (printItem verbosity) $ M.keys items
    RootOptions _ verbosity _
      (Just (ArmorCommand 
        (ArmorOptions [] Nothing Nothing Nothing Nothing Nothing))) -> do
          armors <- getArmors

          traverse_ (printArmor verbosity) $ M.keys armors
    RootOptions _ verbosity _
      (Just (WeaponCommand 
        (WeaponOptions [] Nothing Nothing Nothing Nothing))) -> do
          weapons <- getWeapons

          traverse_ (printWeapon verbosity) $ M.keys weapons
    RootOptions _ verbosity _
      (Just (ContainerCommand 
        (ContainerOptions [] Nothing Nothing))) -> do
          containers <- getContainers

          traverse_ (printContainer verbosity) $ M.keys containers
    RootOptions _ verbosity _
      (Just (MountCommand 
        (MountOptions [] Nothing Nothing Nothing))) -> do
          mounts <- getMounts

          traverse_ (printMount verbosity) $ M.keys mounts
    RootOptions _ verbosity _
      (Just (SpellCommand 
        (SpellOptions [] Nothing Nothing Nothing))) -> do
          spells <- getSpells

          traverse_ (printSpell verbosity) $ M.keys spells
    RootOptions _ verbosity _
      (Just (MoneyCommand 
        (MoneyOptions [] Nothing Nothing))) -> do
          monies <- getMoney

          traverse_ (printMoney verbosity) $ M.keys monies
    RootOptions _ _ _
      (Just (SceneCommand 
        (SceneOptions _ _ _ _ _ _ 
          (Just (SceneCreate 
            (CreateScene id eName x y)))))) -> do
      let info = EntityInfo { name = eName, children = EntityChildren [] }
      let entity = Scene { entityInfo = info, dimensions = (x, y) }

      saveEntity id entity
    RootOptions _ _ _
      (Just (ActorCommand
        (ActorOptions _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
          (Just (ActorCreate
            (CreateActor id eName x y cHp mHp cha int con str dex wis hd ac l sp wp)))))) -> do
      let info = EntityInfo { name = eName, children = EntityChildren [] }
      let entity = Actor { 
        entityInfo = info,
        position = (x, y),
        currentHp = cHp,
        maxHp = mHp,
        cha = cha,
        int = int,
        con = con,
        str = str,
        dex = dex,
        wis = wis,
        hitDice = hd,
        ac = ac,
        level = l,
        saveProficiencies = SaveProficiencies $ rights sp,
        weaponProficiencies = WeaponProficiencies $ rights wp
      }

      saveEntity id entity
    RootOptions _ _ _ (Just (RollCommand (RollOptions (Just expression) Nothing Nothing Nothing Nothing False False))) -> do
      diceRollResult expression

  when (rootSave opts) $ do
    sock <- refreshSocketConn
    saveJsonToDaemon sock
  
  when (focus opts) $ do
    sock <- refreshSocketConn
    stateRef <- asks state
    gameState <- liftIO $ readIORef stateRef
    let currentOutput = output gameState
    let outputEntities = outEntities currentOutput

    sendFocusToDaemon sock (map fst (M.toList outputEntities))


main :: IO ()
main = withSocketsDo $ do
  gen <- getStdGen
  opts <- execParser rootInfo
  sock <- socket AF_UNIX Stream defaultProtocol
  let initState = GameState
        { currentScene = ""
        , entities = M.empty
        , output = Output M.empty M.empty [] Nothing
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