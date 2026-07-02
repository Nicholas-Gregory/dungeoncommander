{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import Network.Socket
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (sendAll, recv)
import Control.Applicative (optional)
import System.IO (hReady, stdin, hIsTerminalDevice, hPutStrLn, stderr)
import Control.Monad (join, when, unless)
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
import DC.Error (AppM, AppError (AppError), throwBaseError, ErrorDetail (OtherError))
import Options.Applicative ( execParser )
import DC.Opts
import Data.Foldable (traverse_)
import Data.Either (rights)
import DC.Json
import Data.Maybe (fromMaybe)
import Data.Function ((&))

runApp :: RootOptions -> Socket -> AppM Env ()
runApp opts sock = do
  addrPath <- asks socketPath
  json <- getJson sock
  initEntities json
  entities <- getEntities
  setOutputEntities entities

  case opts of
    RootOptions _ verbosity _ _
      (Just (SceneCommand 
        (SceneOptions ids filterX filterY command))) -> do
          entities <- getEntitiesByIds ids
          let filteredScenes = entities
                & M.filter (\case
                      Scene {} -> True
                      _ -> False)
                & M.filter (\s -> maybe True (\fx -> fx == fst (dimensions s)) filterX)
                & M.filter (\s -> maybe True (\fy -> fy == snd (dimensions s)) filterY)

          setOutputEntities filteredScenes

          case command of
            Nothing -> traverse_ (printScene verbosity) $ M.keys filteredScenes
            Just (SceneCreate
              (CreateScene id sName x y)) -> do
                let info = EntityInfo { name = sName, children = EntityChildren [] }
                let entity = Scene { entityInfo = info, dimensions = (x, y) }

                saveEntity id entity
                addEntityToOutputEntities id entity
            Just (SceneUpdate
              (UpdateScene nId sName x y)) -> 
                let performUpdate scenes =
                      traverse_ (\(id, s) -> do
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
                        removeEntityFromOutputEntities id) $ M.toList scenes
                in case (ids, filterX, filterY, command) of
                  ([], Nothing, Nothing, Just _) -> do
                    entities <- getEntities

                    if M.size entities /= 1
                      then throwBaseError $ OtherError "Can't perform action on more than one piped entity"
                      else performUpdate entities
                  _ -> performUpdate filteredScenes
    RootOptions _ verbosity _ _
      (Just (ActorCommand
        (ActorOptions [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing False False False False False False Nothing))) -> do
          actors <- getActors

          setOutputEntities actors
          traverse_ (printActor verbosity) $ M.keys actors
    RootOptions _ verbosity _ _
      (Just (ObjectCommand 
        (ObjectOptions [] Nothing Nothing Nothing Nothing Nothing Nothing))) -> do
          objects <- getObjects

          setOutputEntities objects
          traverse_ (printObject verbosity) $ M.keys objects
    RootOptions _ verbosity _ _
      (Just (TrapCommand 
        (TrapOptions [] Nothing Nothing Nothing Nothing Nothing Nothing))) -> do
          traps <- getTraps

          setOutputEntities traps
          traverse_ (printTrap verbosity) $ M.keys traps
    RootOptions _ verbosity _ _
      (Just (ItemCommand 
        (ItemOptions [] Nothing Nothing Nothing))) -> do
          items <- getItems

          setOutputEntities items
          traverse_ (printItem verbosity) $ M.keys items
    RootOptions _ verbosity _ _
      (Just (ArmorCommand 
        (ArmorOptions [] Nothing Nothing Nothing Nothing Nothing))) -> do
          armors <- getArmors

          setOutputEntities armors
          traverse_ (printArmor verbosity) $ M.keys armors
    RootOptions _ verbosity _ _
      (Just (WeaponCommand 
        (WeaponOptions [] Nothing Nothing Nothing Nothing))) -> do
          weapons <- getWeapons

          setOutputEntities weapons
          traverse_ (printWeapon verbosity) $ M.keys weapons
    RootOptions _ verbosity _ _
      (Just (ContainerCommand 
        (ContainerOptions [] Nothing Nothing))) -> do
          containers <- getContainers

          setOutputEntities containers
          traverse_ (printContainer verbosity) $ M.keys containers
    RootOptions _ verbosity _ _
      (Just (MountCommand 
        (MountOptions [] Nothing Nothing Nothing))) -> do
          mounts <- getMounts

          setOutputEntities mounts
          traverse_ (printMount verbosity) $ M.keys mounts
    RootOptions _ verbosity _ _
      (Just (SpellCommand 
        (SpellOptions [] Nothing Nothing Nothing))) -> do
          spells <- getSpells

          setOutputEntities spells
          traverse_ (printSpell verbosity) $ M.keys spells
    RootOptions _ verbosity _ _
      (Just (MoneyCommand 
        (MoneyOptions [] Nothing Nothing))) -> do
          monies <- getMoney

          setOutputEntities monies
          traverse_ (printMoney verbosity) $ M.keys monies
    RootOptions _ _ _ _
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
      addEntityToOutputEntities id entity
    RootOptions _ _ _ _
      (Just (ObjectCommand
        (ObjectOptions _ _ _ _ _ _
          (Just (ObjectCreate
            (CreateObject id eName ac mHp cHp x y)))))) -> do
              let info = EntityInfo { name = eName, children = EntityChildren [] }
              let entity = Object {
                entityInfo = info,
                position = (x, y),
                ac = ac,
                maxHp = mHp,
                currentHp = cHp
              }

              saveEntity id entity
              addEntityToOutputEntities id entity
    RootOptions _ _ _ _
      (Just (TrapCommand
        (TrapOptions _ _ _ _ _ _ 
          (Just (TrapCreate
            (CreateTrap id eName dDc ab sDc d x y)))))) -> do
              let info = EntityInfo { name = eName, children = EntityChildren [] }
              let entity = Trap {
                entityInfo = info,
                position = (x, y),
                detectDc = dDc,
                attackBonus = ab,
                saveDc = sDc,
                trapDamage = d
              }

              saveEntity id entity
              addEntityToOutputEntities id entity
    RootOptions _ _ _ _
      (Just (ItemCommand
        (ItemOptions _ _ _
          (Just (ItemCreate
            (CreateItem id eName c w)))))) -> do
              let info = EntityInfo { name = eName, children = EntityChildren [] }
              let iInfo = ItemInfo { cost = c, weight = w}
              let entity = Item {
                entityInfo = info,
                itemInfo = iInfo
              } 

              saveEntity id entity
              addEntityToOutputEntities id entity
    RootOptions _ _ _ _
      (Just (ArmorCommand
        (ArmorOptions _ _ _ _ _
          (Just (ArmorCreate
            (CreateArmor id eName c w ac str sd t)))))) -> do
            let info = EntityInfo { name = eName, children = EntityChildren [] }
            let iInfo = ItemInfo { cost = c, weight = w }
            let entity = Armor {
              entityInfo = info,
              itemInfo = iInfo,
              ac = ac,
              str = str,
              stealthDisadvantage = sd,
              armorType = t
            }

            saveEntity id entity
            addEntityToOutputEntities id entity
    RootOptions _ _ _ _
      (Just (WeaponCommand
        (WeaponOptions _ _ _ _
          (Just (WeaponCreate
            (CreateWeapon id eName c weight d dt p w)))))) -> do
              case w of 
                Left e -> throwError e
                Right weapon -> do
                  case dt of
                    Left e -> throwError e
                    Right dType -> do
                      let info = EntityInfo { name = eName, children = EntityChildren [] }
                      let iInfo = ItemInfo { cost = c, weight = weight }
                      let entity = Weapon {
                        entityInfo = info,
                        itemInfo = iInfo,
                        weaponDamage = (d, dType),
                        properties = WeaponProperties $ rights p,
                        weapon = weapon
                      }

                      saveEntity id entity
                      addEntityToOutputEntities id entity
    RootOptions _ _ _ _
      (Just (ContainerCommand
        (ContainerOptions _ _ 
          (Just (ContainerCreate
            (CreateContainer id eName cost weight capacity)))))) -> do
              let info = EntityInfo { name = eName, children = EntityChildren [] }
              let iInfo = ItemInfo { cost = cost, weight = weight }
              let entity = Container {
                entityInfo = info,
                itemInfo = iInfo,
                capacity = capacity
              }

              saveEntity id entity
              addEntityToOutputEntities id entity
    RootOptions _ _ _ _
      (Just (MountCommand
        (MountOptions _ _ _
          (Just (MountCreate
            (CreateMount id name speed carrying)))))) -> do
              let info = EntityInfo { name = name, children = EntityChildren [] }
              let entity = Mount {
                entityInfo = info,
                speed = speed,
                carryingCapacity = carrying
              }

              saveEntity id entity
              addEntityToOutputEntities id entity
    RootOptions _ _ _ _
      (Just (SpellCommand
        (SpellOptions _ _ _
          (Just (SpellCreate
            (CreateSpell id name level ritual action range components duration targets aoe save attack)))))) -> do
              let info = EntityInfo { name = name, children = EntityChildren [] }
              let entity = Spell {
                entityInfo = info,
                level = level,
                ritual = ritual,
                action = action,
                range = range,
                components = components,
                duration = duration,
                targets = targets,
                aoe = aoe,
                save = save,
                attack = attack
              }

              saveEntity id entity
              addEntityToOutputEntities id entity
    RootOptions _ _ _ _
      (Just (MoneyCommand
        (MoneyOptions _ _
          (Just (MoneyCreate
            (CreateMoney id name amount)))))) -> do
              let info = EntityInfo { name = name, children = EntityChildren [] }
              let entity = Money {
                entityInfo = info,
                amount = amount
              }

              saveEntity id entity
              addEntityToOutputEntities id entity
    RootOptions _ _ _  _(Just (RollCommand (RollOptions (Just expression) Nothing Nothing Nothing Nothing False False))) -> do
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

  unless (noOutput opts) $ do
    stateRef <- asks state
    gameState <- liftIO $ readIORef stateRef
    let out = output gameState
    case outError out of
      Just e -> throwError e
      Nothing -> do
        let outEntitiesJson = JsonObject $ M.map toJson (outEntities out)
        let actionsJson = JsonObject $ outActions out
        let focusJson = JsonArray $ map toJson (outFocus out)
        let outJson = JsonObject $ M.fromList [
              ("entities", outEntitiesJson),
              ("actions", actionsJson),
              ("focus", focusJson)
              ]

        liftIO $ putStrLn $ writeJsonValue outJson

main :: IO ()
main = withSocketsDo $ do
  gen <- getStdGen
  opts <- execParser rootInfo
  sock <- socket AF_UNIX Stream defaultProtocol
  let initState = GameState
        { currentScene = ""
        , entities = M.empty
        , output = Output { outEntities = M.empty, outActions = M.empty, outFocus = [], outError = Nothing }
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