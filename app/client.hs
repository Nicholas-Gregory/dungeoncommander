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
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans (lift))
import DC.Actions
import Data.Traversable (traverse)
import DC.Types 
import DC.Error (AppM, AppError (AppError, errorDetail), throwBaseError, ErrorDetail (OtherError, ParseError))
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
import qualified Data.Text as T
import Control.Concurrent (writeList2Chan)

processCommand :: KM.KeyMap Entity -> EntityAction -> AppM Env ()
processCommand _ (SceneA (SceneCreate (CreateScene id sName x y))) = do
    let info = EntityInfo { name = sName, children = EntityChildren [] }
    let entity = Scene { entityInfo = info, dimensions = (x, y) }

    saveEntity id entity
    addEntityToOutputEntities id entity
processCommand scenes (SceneA (SceneUpdate (UpdateScene nId sName x y))) = do
  when (KM.size scenes > 1 && isJust nId) 
        $ do throwBaseError $ OtherError "Cannot update multiple IDs simultaneously"
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
processCommand scenes (SceneA (SceneAddActor (AddActorScene ids))) = do
  traverse_ (\(k, _) -> traverse_ (addChild ActorLocation k) ids) $ M.toList $ KM.toMapText scenes
  traverse_ (\(k, v) -> addEntityToOutputEntities (T.unpack k) v)$ M.toList $ KM.toMapText scenes
processCommand _ (ActorA (ActorCreate (CreateActor id name x y cHp mHp cha int con str dex wis hd ac l sp wp))) = do
  let info = EntityInfo { name = name, children = EntityChildren [] }
  case (sequenceA sp, sequenceA wp) of
    (Right sp', Right wp') -> do
      let entity = Actor
            { entityInfo = info
            , position = (x, y)
            , currentHp = cHp
            , maxHp = mHp
            , cha = cha
            , int = int
            , con = con
            , str = str
            , dex = dex
            , wis = wis
            , hitDice = hd
            , ac = ac
            , level = l
            , saveProficiencies = SaveProficiencies sp'
            , weaponProficiencies = WeaponProficiencies wp'}

      saveEntity id entity
      addEntityToOutputEntities id entity
    (Left _, Right _) -> throwBaseError $ ParseError "Unknown save proficiency"
    (Right _, Left _) -> throwBaseError $ ParseError "Unkown weapon proficiency"
    (Left _, Left _) -> throwBaseError $ ParseError "Unkown save proficiency, unknown weapon proficiency"
processCommand actors (ActorA (ActorUpdate (UpdateActor nId nName x y cHp mHp ncha nint ncon nstr ndex nwis hd nac l sp wp))) = do
  case (sequenceA sp, sequenceA wp) of
    (Right sp', Right wp') -> do
      when (KM.size actors > 1 && isJust nId) 
        $ do throwBaseError $ OtherError "Cannot update multiple IDs simultaneously"

      traverse_ (\(k, a) -> do
        let id = K.toString k
        let newId = fromMaybe id nId
        let newName = fromMaybe (name $ entityInfo a) nName
        let newX = fromMaybe (fst $ position a) x
        let newY = fromMaybe (snd $ position a) y
        let newCurrentHp = fromMaybe (currentHp a) cHp
        let newMaxHp = fromMaybe (maxHp a) mHp
        let newCha = fromMaybe (cha a) ncha
        let newInt = fromMaybe (int a) nint
        let newCon = fromMaybe (con a) ncon
        let newStr = fromMaybe (str a) nstr
        let newDex = fromMaybe (dex a) ndex
        let newWis = fromMaybe (wis a) nwis
        let newHd = fromMaybe (hitDice a) hd
        let newAc = fromMaybe (ac a) nac
        let newLevel = fromMaybe (level a) l
        let newInfo = (entityInfo a) { name = newName }
        let newActor = Actor 
              { entityInfo = newInfo
              , position = (newX, newY)
              , currentHp = newCurrentHp
              , maxHp = newMaxHp
              , cha = newCha
              , int = newInt
              , con = newCon
              , str = newStr
              , dex = newDex
              , wis = newWis
              , hitDice = newHd
              , ac = newAc
              , level = newLevel
              , weaponProficiencies = WeaponProficiencies wp'
              , saveProficiencies = SaveProficiencies sp'}
        
        saveEntity newId newActor
        addEntityToOutputEntities newId newActor
        
        when (id /= newId) $ do
          deleteEntity id
          removeEntityFromOutputEntities id) $ KM.toList actors
    (Left _, Right _) -> throwBaseError $ ParseError "Unknown save proficiency"
    (Right _, Left _) -> throwBaseError $ ParseError "Unkown weapon proficiency"
    (Left _, Left _) -> throwBaseError $ ParseError "Unkown save proficiency, unknown weapon proficiency"
processCommand actors (ActorA ActorDelete) = traverse_ (\(k, _) -> deleteEntity $ K.toString k) $ KM.toList actors
processCommand objects (ObjectA (ObjectCreate (CreateObject id n ac mhp chp x y))) = do
  let info = EntityInfo { name = n, children = EntityChildren [] }
  let entity = Object
        { entityInfo = info
        , ac = ac
        , maxHp = mhp
        , currentHp = chp
        , position = (x, y)}

  saveEntity id entity
  addEntityToOutputEntities id entity
processCommand objects (ObjectA (ObjectUpdate (UpdateObject nid n nac mhp chp x y))) = do
  when (KM.size objects > 1 && isJust nid)
    $ do throwBaseError $ OtherError "Cannot update multiple IDs simultaneously"

  traverse_ (\(k, o) -> do
    let id = K.toString k
    let newId = fromMaybe id nid
    let newName = fromMaybe (name $ entityInfo o) n
    let newAc = fromMaybe (ac o) nac
    let newMaxHp = fromMaybe (maxHp o) mhp
    let newCurrentHp = fromMaybe (currentHp o) chp
    let newX = fromMaybe (fst $ position o) x
    let newY = fromMaybe (snd $ position o) y
    let newInfo = (entityInfo o) { name = newName }
    let newObject = Object
          { entityInfo = newInfo
          , ac = newAc
          , maxHp = newMaxHp
          , currentHp = newCurrentHp
          , position = (newX, newY)}
          
    saveEntity newId newObject
    addEntityToOutputEntities newId newObject
    
    when (id /= newId) $ do
      deleteEntity id
      removeEntityFromOutputEntities id) $ KM.toList objects
processCommand objects (ObjectA ObjectDelete) = traverse_ (\(k, _) -> deleteEntity $ K.toString k) $ KM.toList objects
processCommand _ (TrapA (TrapCreate (CreateTrap id n ddc ab sdc d x y))) = do
  let newInfo = EntityInfo { name = n, children = EntityChildren [] }
  let newTrap = Trap
        { detectDc = ddc
        , attackBonus = ab
        , saveDc = sdc
        , trapDamage = d
        , position = (x, y)
        , entityInfo = newInfo}

  saveEntity id newTrap
  addEntityToOutputEntities id newTrap
processCommand traps (TrapA (TrapUpdate (UpdateTrap nid n ddc ab sdc d x y))) = do
  when (KM.size traps > 1 && isJust nid)
    $ do throwBaseError $ OtherError "Cannot update multiple IDs simultaneously"
  traverse_ (\(k, t) -> do
    let id = K.toString k
    let newId = fromMaybe id nid
    let newName = fromMaybe (name $ entityInfo t) n
    let newDetectDc = fromMaybe (detectDc t) ddc
    let newAttackBonus = fromMaybe (attackBonus t) ab
    let newSaveDc = fromMaybe (saveDc t) sdc
    let newDamage = fromMaybe (trapDamage t) d
    let newX = fromMaybe (fst $ position t) x
    let newY = fromMaybe (snd $ position t) y
    let newInfo = (entityInfo t) { name = newName }
    let newTrap = Trap
          { entityInfo = newInfo
          , detectDc = newDetectDc
          , attackBonus = newAttackBonus
          , saveDc = newSaveDc
          , trapDamage = newDamage
          , position = (newX, newY)}
    
    saveEntity newId newTrap
    addEntityToOutputEntities newId newTrap
    
    when (id /= newId) $ do
      deleteEntity id
      removeEntityFromOutputEntities id) $ KM.toList traps
processCommand traps (TrapA TrapDelete) = traverse_ (\(k, _) -> deleteEntity $ K.toString k) $ KM.toList traps
processCommand _ (ItemA (ItemCreate (CreateItem id n c w))) = do
  let info = EntityInfo { name = n, children = EntityChildren [] }
  let iInfo = ItemInfo { cost = c, weight = w }
  let newItem = Item { entityInfo = info, itemInfo = iInfo }

  saveEntity id newItem
  addEntityToOutputEntities id newItem
processCommand items (ItemA (ItemUpdate (UpdateItem nid n c w))) = do
  when (KM.size items > 1 && isJust nid)
    $ do throwBaseError $ OtherError "Cannot update multiple IDs simultaneously"
  
  traverse_ (\(k, i) -> do
    let id = K.toString k
    let newId = fromMaybe id nid
    let newName = fromMaybe (name $ entityInfo i) n
    let newCost = fromMaybe (cost $ itemInfo i) c
    let newWeight = fromMaybe (weight $ itemInfo i) w
    let newInfo = (entityInfo i) { name = newName }
    let newItemInfo = ItemInfo { cost = newCost, weight = newWeight }
    let newItem = Item { entityInfo = newInfo, itemInfo = newItemInfo }
    
    saveEntity newId newItem
    addEntityToOutputEntities newId newItem
    
    when (id /= newId) $ do
      deleteEntity id
      removeEntityFromOutputEntities id) $ KM.toList items
processCommand items (ItemA ItemDelete) = traverse_ (\(k, _) -> deleteEntity $ K.toString k) $ KM.toList items
processCommand _ (ArmorA (ArmorCreate (CreateArmor id n c w ac str sd t))) = do
  let info = EntityInfo { name = n, children = EntityChildren [] }
  let iInfo = ItemInfo { cost = c, weight = w }
  let newArmor = Armor
        { entityInfo = info
        , itemInfo = iInfo
        , ac = ac
        , str = str
        , stealthDisadvantage = sd
        , armorType = t}
  
  saveEntity id newArmor
  addEntityToOutputEntities id newArmor
processCommand armor (ArmorA (ArmorUpdate (UpdateArmor nid n c w nac nstr sd t))) = do
  when (KM.size armor > 1 && isJust nid) 
    $ do throwBaseError $ OtherError "Cannot update multiple IDs simultaneously"

  traverse_ (\(k, a) -> do
    let id = K.toString k
    let newId = fromMaybe id nid
    let newName = fromMaybe (name $ entityInfo a) n
    let newCost = fromMaybe (cost $ itemInfo a) c
    let newWeight = fromMaybe (weight $ itemInfo a) w
    let newAc = fromMaybe (ac a) nac
    let newStr = fromMaybe (str a) nstr
    let newStealthDisadvantage = fromMaybe (stealthDisadvantage a) sd
    let newType = fromMaybe (armorType a) t
    let newInfo = (entityInfo a) { name = newName }
    let newItemInfo = ItemInfo { cost = newCost, weight = newWeight }
    let newArmor = Armor
          { entityInfo = newInfo
          , itemInfo = newItemInfo
          , ac = newAc
          , str = newStr
          , stealthDisadvantage = newStealthDisadvantage
          , armorType = newType}
    
    saveEntity newId newArmor
    addEntityToOutputEntities newId newArmor
    
    when (id /= newId) $ do
          deleteEntity id
          removeEntityFromOutputEntities id
    ) $ KM.toList armor
processCommand armor (ArmorA ArmorDelete) = traverse_ (\(k, _) -> deleteEntity $ K.toString k) $ KM.toList armor
processCommand _ (WeaponA (WeaponCreate (CreateWeapon id n c w d dt wp wn))) = do
  case (dt, sequenceA wp, wn) of
    (Right dt', Right wp', Right wn') -> do
      let info = EntityInfo { name = n, children = EntityChildren [] }
      let iInfo = ItemInfo { cost = c, weight = w }
      let newWeapon = Weapon
            { entityInfo = info
            , itemInfo = iInfo
            , weaponDamage = (d, dt')
            , properties = WeaponProperties wp'
            , weapon = wn'}

      saveEntity id newWeapon
      addEntityToOutputEntities id newWeapon
    _ -> throwBaseError $ OtherError "Unexpected weapon type, weapon properties, or damage type"
processCommand weapons (WeaponA (WeaponUpdate (UpdateWeapon nid n c w d dt wp wn))) = undefined

runApp :: RootOptions -> Socket -> AppM Env ()
runApp opts sock = do
  addrPath <- asks socketPath
  json <- getJson sock
  initEntities json
  entities <- getEntities
  setOutputEntities entities

  case opts of
    RootOptions _ verbosity _ _ (Just rootCommand) -> 
          let baseEntities = case rootCommand of
                (SceneCommand {}) -> KM.filter (\case
                  Scene {} -> True
                  _ -> False)
                (ActorCommand {}) -> KM.filter (\case
                  Actor {} -> True
                  _ -> False)
                (ObjectCommand {}) -> KM.filter (\case
                  Object {} -> True
                  _ -> False)
                (TrapCommand {}) -> KM.filter (\case
                  Trap {} -> True
                  _ -> False)
                (ItemCommand {}) -> KM.filter (\case
                  Item {} -> True
                  _ -> False)
              opt = case rootCommand of
                (SceneCommand opt) -> opt
                (ActorCommand opt) -> opt
                (ObjectCommand opt) -> opt
                (TrapCommand opt) -> opt
                (ItemCommand opt) -> opt
              filteringCondition = case opt of
                (SceneOptions ids filterX filterY _) -> not (null ids) || isJust (filterX <|> filterY)
                (ActorOptions ids filterX filterY filterCHp filterMHp filterCha filterInt filterCon filterStr filterDex filterWis filterHd filterAc filterLevel _) -> not (null ids) || isJust (filterX <|> filterY <|> filterCHp)
                (ObjectOptions ids filterX filterY filterAc filterMhp filterChp _) -> not (null ids) || isJust (filterX <|> filterY <|> filterAc <|> filterMhp <|> filterChp)
                (TrapOptions ids filterX filterY filterDdc filterAb filterSdc _) -> not (null ids) || isJust (filterX <|> filterY <|> filterDdc <|> filterAb <|> filterSdc)
                (ItemOptions ids filterCost filterWeight _) -> not (null ids) || isJust (filterCost <|> filterWeight)
              ids = case opt of
                (SceneOptions ids _ _ _) -> ids
                (ActorOptions { actorIds = ids }) -> ids
                (ObjectOptions { objectIds = ids }) -> ids
                (TrapOptions { trapIds = ids }) -> ids
                (ItemOptions { itemIds = ids }) -> ids
              filters = case opt of
                (SceneOptions _ filterX filterY _) ->
                  [ \s -> maybe True (\fx -> fx == fst (dimensions s)) filterX
                  , \s -> maybe True (\fy -> fy == snd (dimensions s)) filterY]
                (ActorOptions _ filterX filterY filterCHp filterMHp filterCha filterInt filterCon filterStr filterDex filterWis filterHd filterAc filterLevel _) ->
                  [ \a -> maybe True (\fx -> fx == fst (position a)) filterX
                  , \a -> maybe True (\fy -> fy == snd (position a)) filterY
                  , \a -> maybe True (\fMhp -> fMhp == maxHp a) filterMHp
                  , \a -> maybe True (\fChp -> fChp == currentHp a) filterCHp
                  , \a -> maybe True (\fCha -> fCha == cha a) filterCha
                  , \a -> maybe True (\fInt -> fInt == int a) filterInt
                  , \a -> maybe True (\fCon -> fCon == con a) filterCon
                  , \a -> maybe True (\fStr -> fStr == str a) filterStr
                  , \a -> maybe True (\fDex -> fDex == dex a) filterDex
                  , \a -> maybe True (\fWis -> fWis == wis a) filterWis
                  , \a -> maybe True (\fHd -> fHd == hitDice a) filterHd
                  , \a -> maybe True (\fAc -> fAc == ac a) filterAc
                  , \a -> maybe True (\fL -> fL == level a) filterLevel]
                (ObjectOptions _ filterX filterY filterAc filterMhp filterChp _) ->
                  [ \o -> maybe True (\fx -> fx == fst (position o)) filterX
                  , \o -> maybe True (\fy -> fy == snd (position o)) filterY
                  , \o -> maybe True (\fac -> fac == ac o) filterAc
                  , \o -> maybe True (\fmhp -> fmhp == maxHp o) filterMhp
                  , \o -> maybe True (\fchp -> fchp == currentHp o) filterChp]
                (TrapOptions _ filterX filterY filterDdc filterAb filterSdc _) ->
                  [ \t -> maybe True (\fx -> fx == fst (position t)) filterX
                  , \t -> maybe True (\fy -> fy == snd (position t)) filterY
                  , \t -> maybe True (\fddc -> fddc == detectDc t) filterDdc
                  , \t -> maybe True (\fab -> fab == attackBonus t) filterAb
                  , \t -> maybe True (\fsdc -> fsdc == saveDc t) filterSdc]
                (ItemOptions _ filterCost filterWeight _) ->
                  [ \i -> maybe True (\fc -> fc == cost (itemInfo i)) filterCost
                  , \i -> maybe True (\fw -> fw == weight (itemInfo i)) filterWeight]
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
                  entities <- getEntities

                  setOutputEntities entities
                  case entityCommand opt of
                    Just c -> processCommand entities c
                    Nothing -> traverse_ (printScene verbosity . K.toString) $ KM.keys entities
                else do
                  -- No CLI options, no pipe input, assuming command applies to all entities or focused entities
                  focus <- getFocusFromDaemon
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