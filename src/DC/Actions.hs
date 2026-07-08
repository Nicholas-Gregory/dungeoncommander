{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module DC.Actions (
  getAbilityScore,
  getAbilityModifier,
  abilityCheck,
  hasSaveProficiency,
  getProficiencyBonus,
  savingThrow,
  hasWeaponProficiency,
  attackRoll,
  saveEntity,
  addChild,
  removeChild,
  tooFewArgumentsError,
  getJsonFromDaemon,
  getJsonFromInput,
  getJson,
  initCurrentScene,
  initEntities,
  setCurrentScene,
  getEntityById,
  printScene,
  diceRollResult,
  getEntities,
  saveJsonToDaemon,
  refreshSocketConn,
  getScenes,
  getActors,
  getObjects,
  getTraps,
  getItems,
  getArmors,
  getWeapons,
  getContainers,
  getMounts,
  getSpells,
  getMoney,
  printActor,
  printObject,
  printTrap,
  printItem,
  printArmor,
  printWeapon,
  printContainer,
  printMount,
  printSpell,
  printMoney,
  sendFocusToDaemon,
  setOutputEntities,
  addEntityToOutputEntities,
  setEntities,
  deleteEntity,
  getEntitiesByIds,
  removeEntityFromOutputEntities,
  getFocusFromDaemon
) where
import DC.Types
import qualified DC.Types (Entity(..), EntityInfo(..), EntityChildType(..), EntityChildren(..), EntityChild(..), SaveProficiencies(..), WeaponProficiencies(..), Ability(..), CheckSuccess, WeaponProficiency (Simple, Martial, Specific), Weapon (SimpleMelee, SimpleRanged, MartialMelee, MartialRanged))
import DC.Dice (rollDice, processExpression)
import System.Random (StdGen, getStdGen)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (atomicModifyIORef', readIORef)
import Control.Monad.Reader (asks, MonadTrans (lift))
import qualified Data.Map as M
import System.IO (hPutStrLn, stderr, getContents', hIsTerminalDevice, stdin, hPrint)
import DC.Error (AppError (AppError), newBaseError, ErrorDetail (OtherError, CliParseError, SocketError, JsonValidationError), throwBaseError, err, AppM)
import System.Environment (getArgs)
import DC.Parse (Parser(runParser))
import Control.Monad.Except (MonadError(throwError))
import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import System.Timeout (timeout)
import Data.Foldable (Foldable(foldl'))
import Debug.Trace (trace)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import DC.Error
import qualified Data.Text as T
import qualified Data.Vector as V

getAbilityScore :: Entity -> Ability -> Either AppError Int
getAbilityScore (Actor { cha }) Charisma = Right cha
getAbilityScore (Actor { int }) Intelligence = Right int
getAbilityScore (Actor { con }) Constitution = Right con
getAbilityScore (Actor { str }) Strength = Right str
getAbilityScore (Actor { dex }) Dexterity = Right dex
getAbilityScore (Actor { wis }) Wisdom = Right wis
getAbilityScore e a = Left
  $ newBaseError
  $ OtherError
  $ "Unknown Ability type or not an Actor for getAbilityScore. Ability type: "
  <> show a
  <> "Entity: "
  <> show e

getAbilityModifier :: Entity -> Ability -> Either AppError Int
getAbilityModifier e a = (\s -> (s - 10) `div` 2) <$> getAbilityScore e a

abilityCheck :: StdGen -> Entity -> Ability -> Int -> Either AppError CheckSuccess
abilityCheck gen entity ability dc = (\m -> (m + rollDice gen 1 20) >= dc)
  <$> getAbilityModifier entity ability

hasSaveProficiency :: Entity -> Ability -> Either AppError Bool
hasSaveProficiency (Actor { saveProficiencies = SaveProficiencies xs }) ability = Right
  $ ability `elem` xs
hasSaveProficiency e a = Left
  $ newBaseError
  $ OtherError
  $ "Unknown Ability type or not an Actor for hasSaveProficiency. Ability type: "
  <> show a
  <> "Entity: "
  <> show e

getProficiencyBonus :: Entity -> Either AppError Int
getProficiencyBonus (Actor { level }) = Right $ ((level - 1) `div` 4) + 2
getProficiencyBonus x = Left
  $ newBaseError
  $ OtherError
  $ "Expected an Actor for getProficiencyBonus, found: "
  <> show x


savingThrow :: StdGen -> Entity -> Ability -> Int -> Either AppError CheckSuccess
savingThrow gen entity ability dc = do
  pro <- hasSaveProficiency entity ability
  abilityModifier <- getAbilityModifier entity ability
  proficiencyBonus <- getProficiencyBonus entity
  let roll = rollDice gen 1 20

  return $ if pro
    then roll + abilityModifier + proficiencyBonus >= dc
    else roll + abilityModifier >= dc

hasWeaponProficiency :: Entity -> WeaponProficiency -> Either AppError Bool
hasWeaponProficiency (Actor { weaponProficiencies = WeaponProficiencies xs }) proficiency = Right
  $ proficiency `elem` xs
hasWeaponProficiency e x = Left
  $ newBaseError
  $ OtherError
  $ "Expected Actor and WeaponProficiency for hasWeaponProficiency. Entity: "
  <> show e
  <> "Object: "
  <> show x

attackRoll :: StdGen -> Entity -> Entity -> Ability -> Int -> Either AppError CheckSuccess
attackRoll gen (WeaponEntity { weapon }) entity ability dc = do
  abilityModifier <- getAbilityModifier entity ability
  proficiencyBonus <- getProficiencyBonus entity
  hasProficiency <- (case weapon of
        SimpleMelee w -> liftA2 (||) (hasWeaponProficiency entity Simple) (hasWeaponProficiency entity (Specific $ SimpleMelee w))
        SimpleRanged w -> liftA2 (||) (hasWeaponProficiency entity Simple) (hasWeaponProficiency entity (Specific $ SimpleRanged w))
        MartialMelee w -> liftA2 (||) (hasWeaponProficiency entity Martial) (hasWeaponProficiency entity (Specific $ MartialMelee w))
        MartialRanged w -> liftA2 (||) (hasWeaponProficiency entity Martial) (hasWeaponProficiency entity (Specific $ MartialRanged w)))
  let roll = rollDice gen 1 20

  return $ case roll of
    1 -> False
    20 -> True
    _ -> if hasProficiency
    then roll + abilityModifier + proficiencyBonus >= dc
    else roll + abilityModifier >= dc
attackRoll _ e _ _ _ = Left
  $ newBaseError
  $ OtherError
  $ "Expected Weapon for attackRoll, found: "
  <> show e

saveEntity :: String -> Entity -> AppM Env ()
saveEntity k e = err "updateEntity" [("entity_id", show k), ("update_payload", show e)] $ do
  stateRef <- asks state

  liftIO $ atomicModifyIORef' stateRef $ \st ->
    let newEntities = KM.insert (K.fromString k) e (entities st)
    in (st { entities=newEntities }, ())

addChild :: EntityChildType -> T.Text -> String -> AppM Env ()
addChild t p c = err "addChild"
  [ ("child_type", show t)
  , ("parent_id", show p)
  , ("child_id", show c)
  ] $ do
  stateRef <- asks state

  liftIO $ atomicModifyIORef' stateRef $ \st ->
    let newEntities = M.adjust
          (\parent ->
             let oldInfo = entityInfo parent
                 oldChildren = children oldInfo
                 newChildren = case oldChildren of
                   EntityChildren xs -> EntityChildren (EntityChild { childType = t, childId = c } : xs)
             in parent { entityInfo = oldInfo { children = newChildren } }
          ) p (KM.toMapText $ entities st)
    in (st { entities= KM.fromMapText newEntities }, ())

removeChild :: EntityChildType -> T.Text -> String -> AppM Env ()
removeChild t p c = err "removeChild"
  [ ("child_type", show t)
  , ("parent_id", show p)
  , ("child_id", show c)
  ] $ do
  stateRef <- asks state

  liftIO $ atomicModifyIORef' stateRef $ \st ->
    let newEntities = M.adjust
          (\parent ->
            let oldInfo = entityInfo parent
                oldChildren = children oldInfo
                newChildren = case oldChildren of
                  EntityChildren xs -> EntityChildren (filter (\child -> childType child /= t && childId child /= c) xs)
            in parent { entityInfo = oldInfo { children = newChildren } }
          ) p (KM.toMapText $ entities st)
    in (st { entities = KM.fromMapText newEntities }, ())

tooFewArgumentsError :: AppM Env ()
tooFewArgumentsError = liftIO $ hPutStrLn stderr "Too few arguments"

getEntities :: AppM Env (KM.KeyMap Entity)
getEntities = err "getEntities" [] $ do
  stateRef <- asks state
  state <- liftIO $ readIORef stateRef

  return $ entities state

refreshSocketConn :: AppM Env Socket
refreshSocketConn = do
  sPath <- asks socketPath
  sock <- liftIO $ socket AF_UNIX Stream defaultProtocol
  liftIO $ connect sock (SockAddrUnix sPath)

  return sock

saveJsonToDaemon :: Socket -> AppM Env ()
saveJsonToDaemon sock = err "saveJsonToDaemon" [] $ do
  entities <- getEntities
  r <- liftIO $ timeout 3000000 $ sendAll sock
    $ BS.toStrict 
    $ BS.pack "{ \"action\": \"save\", \"payload\": "
    <> JSON.encode entities <> BS.pack "}"
  sr <- liftIO $ timeout 3000000 $ recv sock 4096

  case r of
    Nothing -> throwBaseError $ SocketError "Socket timed out"
    Just _ -> case sr of
      Nothing -> throwBaseError $ SocketError "Socket timed out"
      Just sr' -> case BS.unpack $ BS.fromStrict sr' of
        "SUCCESS" -> liftIO $ hPutStrLn stderr "[SYSTEM] Saved session state to disk"
        _ -> liftIO $ hPutStrLn stderr "[SYSTEM] Daemon encountered an error (check logs)"

sendFocusToDaemon :: Socket -> [String] -> AppM Env ()
sendFocusToDaemon conn entityIds = err "sendFocusToDaemon" [("entity_ids", show entityIds)] $ do
  r <- liftIO $ timeout 3000000 $ sendAll conn
    $ BS.toStrict
    $ BS.pack "{\"action\": \"focus\", \"payload\": "
    <> JSON.encode entityIds <> BS.pack "}"
  sr <- liftIO $ timeout 3000000 $ recv conn 4096

  case (r, sr) of
    (Just _, Just sr') -> case BS.unpack $ BS.fromStrict sr' of
      "SUCCESS" -> liftIO $ hPutStrLn stderr $ "[SYSTEM] Entities focused: " <> unwords entityIds
      _ -> liftIO $ hPutStrLn stderr "[SYSTEM] Daemon encountered an error (check logs)"
    _ -> throwBaseError $ SocketError "Socket timed out"

getJsonFromDaemon :: Socket -> AppM Env JSON.Value
getJsonFromDaemon sock = err "getJsonFromDaemon" [] $ do
  liftIO $ sendAll sock 
    $ BS.toStrict
    $ BS.pack "{ \"action\": \"get\", \"payload\": \"all\"}"
  r <- liftIO $ timeout 3000000 $ recv sock 4096
  
  case r of
    Nothing -> throwBaseError $ SocketError "Socket timed out"
    Just d -> case JSON.eitherDecode $ BS.fromStrict d :: Either String JSON.Value of
      Left e -> throwBaseError $ ParseError e
      Right o -> return o

getJsonFromInput :: AppM Env JSON.Value
getJsonFromInput = err "getJsonFromInput" [] $ do
  input <- liftIO B.getContents
  
  case JSON.eitherDecode $ BS.fromStrict input :: Either String JSON.Value of
    Left e -> throwBaseError $ ParseError e
    Right o -> return o

getJson :: Socket -> AppM Env JSON.Value
getJson sock = do
  isTerm <- asks isTerm

  if isTerm
    then do getJsonFromDaemon sock
    else getJsonFromInput

initCurrentScene :: JSON.Value -> AppM Env ()
initCurrentScene (JSON.Object o) = err "initCurrentScene" [("input_json", show o)] $ do
  case KM.lookup "currentScene" o of
    Nothing -> throwBaseError $ JsonValidationError "No \"currentScene\" field in input JSON"
    Just (JSON.String s) -> do
      stateRef <- asks state

      liftIO $ atomicModifyIORef' stateRef $ \st ->
        (st { currentScene = T.unpack s }, ())
    Just x -> throwBaseError
      $ JsonValidationError
      $ "Expected JSON String for \"currentScene\", found: "
      <> show x
initCurrentScene x = err "initCurrentScene" [("input_json", show x)] $ throwBaseError
  $ JsonValidationError "Expected JSON Object"

getEntityJson :: JSON.Value -> AppM Env (KM.KeyMap JSON.Value)
getEntityJson (JSON.Object o) = err "getEntityJson" [("input_json", show o)] $ do
  case KM.lookup "entities" o of
    Nothing -> throwBaseError $ JsonValidationError "Need \"entities\" field in input JSON"
    Just (JSON.Object entities) -> return entities
    Just _ -> throwBaseError $ JsonValidationError "Expected JSON Object for \"entities\" field"
getEntityJson x = err "getEntityJson" [("input_json", show x)] $ throwBaseError
  $ JsonValidationError "Expected JSON Object"

initEntities :: JSON.Value -> AppM Env ()
initEntities o = err "initEntities" [("input_json", show o)] $ do
  entityMap <- getEntityJson o

  case traverse (\v -> JSON.fromJSON v :: JSON.Result Entity) entityMap of
    JSON.Error e -> throwBaseError $ JsonValidationError e
    JSON.Success entities -> do
      stateRef <- asks state

      liftIO $ atomicModifyIORef' stateRef $ \st ->
        (st { entities = entities }, ())

setCurrentScene :: String -> AppM Env ()
setCurrentScene scene = do
  stateRef <- asks state

  liftIO $ atomicModifyIORef' stateRef $ \st ->
    (st { currentScene = scene }, ())

getEntityById :: String -> AppM Env Entity
getEntityById entityId = err "getEntityById" [("entity_id", show entityId)] $ do
  stateRef <- asks state
  gameState <- liftIO $ readIORef stateRef
  let gameEntities = entities gameState

  case KM.lookup (K.fromString entityId) gameEntities of
    Nothing -> throwBaseError $ OtherError "Entity with this ID does not exist in state"
    Just entity -> return entity

getScenes :: AppM Env (KM.KeyMap Entity)
getScenes = KM.filter (\case
  (Scene _ _) -> True
  _ -> False) <$> getEntities

getActors :: AppM Env (KM.KeyMap Entity)
getActors = KM.filter (\case
  (Actor {}) -> True
  _ -> False) <$> getEntities

getObjects :: AppM Env (KM.KeyMap Entity)
getObjects = KM.filter (\case
  (Object {}) -> True
  _ -> False) <$> getEntities

getTraps :: AppM Env (KM.KeyMap Entity)
getTraps = KM.filter (\case
  (Trap {}) -> True
  _ -> False) <$> getEntities

getItems :: AppM Env (KM.KeyMap Entity)
getItems = KM.filter (\case
  (Item {}) -> True
  _ -> False) <$> getEntities

getArmors :: AppM Env (KM.KeyMap Entity)
getArmors = KM.filter (\case
  (Armor {}) -> True
  _ -> False) <$> getEntities

getWeapons :: AppM Env (KM.KeyMap Entity)
getWeapons = KM.filter (\case
  (WeaponEntity {}) -> True
  _ -> False) <$> getEntities

getContainers :: AppM Env (KM.KeyMap Entity)
getContainers = KM.filter (\case
  (Container {}) -> True
  _ -> False) <$> getEntities

getMounts :: AppM Env (KM.KeyMap Entity)
getMounts = KM.filter (\case
  (Mount {}) -> True
  _ -> False) <$> getEntities

getSpells :: AppM Env (KM.KeyMap Entity)
getSpells = KM.filter (\case
  (Spell {}) -> True
  _ -> False) <$> getEntities

getMoney :: AppM Env (KM.KeyMap Entity)
getMoney = KM.filter (\case
  (Money {}) -> True
  _ -> False) <$> getEntities

printScene :: VerbosityLevel -> String -> AppM Env ()
printScene v eid = err "printScene"
  [ ("verbosity", show v)
  , ("entity_id", show eid)
  ] $ do
    entity <- getEntityById eid

    let info = entityInfo entity
        eName = name info
        (EntityChildren eChildren) = children info
        (x, y) = dimensions entity
        vNameString = "ID: " <> eid <> ", Name: " <> eName
        vStatsString = vNameString <> ", Dimensions: " <> show x <> " by " <> show y
        vAllString = vStatsString <> ", Children IDs: " <> foldl' (++) "" (map (\c -> childId c ++ ", ") eChildren)
    case v of
      Name -> liftIO $ hPutStrLn stderr vNameString
      Stats -> liftIO $ hPutStrLn stderr vStatsString
      All -> liftIO $ hPutStrLn stderr vAllString

printActor :: VerbosityLevel -> String -> AppM Env ()
printActor v eid = err "printActor"
  [ ("verbosity", show v)
  , ("entity_id", show eid)
  ] $ do
    entity <- getEntityById eid

    let info = entityInfo entity
        eName = name info
        (EntityChildren eChildren) = children info
        (x, y) = position entity
        eCHp = currentHp entity
        eMHp = maxHp entity
        eCha = cha entity
        eInt = int entity
        eCon = con entity
        eStr = str entity
        eDex = dex entity
        eWis = wis entity
        eHd = hitDice entity
        eAc = ac entity
        eL = level entity
        vNameString = "ID: " <> eid <> ", Name: " <> eName
        vStatsString = vNameString
          <> ", Position: (" <> show x <> ", " <> show y
          <> "), Current HP: " <> show eCHp
          <> ", Max HP: " <> show eMHp
          <> ", Charisma: " <> show eCha
          <> ", Intelligence: " <> show eInt
          <> ", Constitution: " <> show eCon
          <> ", Strength: " <> show eStr
          <> ", Dexterity: " <> show eDex
          <> ", Wisdom: " <> show eWis
          <> ", Hit Dice: " <> eHd
          <> ", Armor Class: " <> show eAc
          <> ", Level: " <> show eL
          <> ", Save Proficiencies: " <> foldl' (++) "" (map (\p -> case JSON.toJSON p of 
            JSON.String s -> T.unpack s <> ", "
            _ -> "") (case saveProficiencies entity of SaveProficiencies a -> a))
          <> ", Weapon Proficiencies: " <> foldl' (++) "" (map (\p -> case JSON.toJSON p of 
          JSON.String s -> T.unpack s <> ", "
          _ -> "") (case weaponProficiencies entity of WeaponProficiencies a -> a))
        vAllString = vStatsString <> ", Children IDs: " <> foldl' (++) "" (map (\c -> childId c ++ ", ") eChildren)

    case v of
      Name -> liftIO $ hPutStrLn stderr vNameString
      Stats -> liftIO $ hPutStrLn stderr vStatsString
      All -> liftIO $ hPutStrLn stderr vAllString

diceRollResult :: String -> AppM Env ()
diceRollResult expression = err "diceRollResult" [ ("expression", show expression ) ] $ do
  gen <- asks gen

  --TODO: stdout roll result action to pipe to next command in chain
  case processExpression gen expression of
    Left e -> throwError e
    Right r -> liftIO $ hPutStrLn stderr
      $ "[ROLL] dice-expression: " <> expression
      <> ". Result: " <> show r

printObject :: VerbosityLevel -> String -> AppM Env ()
printObject v eid = err "printObject"
  [ ("verbosity", show v)
  , ("entity_id", show eid)
  ] $ do
    entity <- getEntityById eid

    let info = entityInfo entity
        eName = name info
        (EntityChildren eChildren) = children info
        (x, y) = position entity
        eAc = ac entity
        eCHp = currentHp entity
        eMHp = maxHp entity
        vNameString = "ID: " <> eid <> ", Name: " <> eName
        vStatsString = vNameString
          <> ", Position: (" <> show x <> ", " <> show y
          <> "), Current HP: " <> show eCHp
          <> ", Max HP: " <> show eMHp
          <> ", Armor Class: " <> show eAc
        vAllString = vStatsString <> ", Children IDs: " <> foldl' (++) "" (map (\c -> childId c ++ ", ") eChildren)

    case v of
      Name -> liftIO $ hPutStrLn stderr vNameString
      Stats -> liftIO $ hPutStrLn stderr vStatsString
      All -> liftIO $ hPutStrLn stderr vAllString

printTrap :: VerbosityLevel -> String -> AppM Env ()
printTrap v eid = err "printTrap"
  [ ("verbosity", show v)
  , ("entity_id", show eid)
  ] $ do
    entity <- getEntityById eid

    let info = entityInfo entity
        eName = name info
        (EntityChildren eChildren) = children info
        (x, y) = position entity
        eDetect = detectDc entity
        eAttack = attackBonus entity
        eSave = saveDc entity
        d = trapDamage entity
        vNameString = "ID: " <> eid <> ", Name: " <> eName
        vStatsString = vNameString
          <> ", Position: (" <> show x <> ", " <> show y <> ")"
          <> ", Detect DC: " <> show eDetect
          <> ", Attack Bonus: " <> show eAttack
          <> ", Save DC: " <> show eSave
          <> ", Damage: " <> d
        vAllString = vStatsString <> ", Children IDs: " <> foldl' (++) "" (map (\c -> childId c ++ ", ") eChildren)

    case v of
      Name -> liftIO $ hPutStrLn stderr vNameString
      Stats -> liftIO $ hPutStrLn stderr vStatsString
      All -> liftIO $ hPutStrLn stderr vAllString

printItem :: VerbosityLevel -> String -> AppM Env ()
printItem v eid = err "printItem"
  [ ("verbosity", show v)
  , ("entity_id", show eid)
  ] $ do
    entity <- getEntityById eid

    let info = entityInfo entity
        eName = name info
        (EntityChildren eChildren) = children info
        iiJson = BS.unpack $ JSON.encode (itemInfo entity)
        vNameString = "ID: " <> eid <> ", Name: " <> eName
        vStatsString = vNameString <> ", ItemInfo: " <> iiJson
        vAllString = vStatsString <> ", Children IDs: " <> foldl' (++) "" (map (\c -> childId c ++ ", ") eChildren)

    case v of
      Name -> liftIO $ hPutStrLn stderr vNameString
      Stats -> liftIO $ hPutStrLn stderr vStatsString
      All -> liftIO $ hPutStrLn stderr vAllString

printArmor :: VerbosityLevel -> String -> AppM Env ()
printArmor v eid = err "printArmor"
  [ ("verbosity", show v)
  , ("entity_id", show eid)
  ] $ do
    entity <- getEntityById eid

    let info = entityInfo entity
        eName = name info
        (EntityChildren eChildren) = children info
        iiJson = BS.unpack $ JSON.encode (itemInfo entity)
        eAc = ac entity
        eStr = str entity
        eSd = stealthDisadvantage entity
        eAt = armorType entity
        vNameString = "ID: " <> eid <> ", Name: " <> eName
        vStatsString = vNameString
          <> ", ItemInfo: " <> iiJson
          <> ", Armor Class: " <> show eAc
          <> ", Strength Req: " <> show eStr
          <> ", Stealth Disadvantage: " <> show eSd
          <> ", Type: " <> eAt
        vAllString = vStatsString <> ", Children IDs: " <> foldl' (++) "" (map (\c -> childId c ++ ", ") eChildren)

    case v of
      Name -> liftIO $ hPutStrLn stderr vNameString
      Stats -> liftIO $ hPutStrLn stderr vStatsString
      All -> liftIO $ hPutStrLn stderr vAllString

printWeapon :: VerbosityLevel -> String -> AppM Env ()
printWeapon v eid = err "printWeapon"
  [ ("verbosity", show v)
  , ("entity_id", show eid)
  ] $ do
    entity <- getEntityById eid

    let info = entityInfo entity
        eName = name info
        (EntityChildren eChildren) = children info
        iiJson = BS.unpack $ JSON.encode (itemInfo entity)
        d = weaponDamage entity
        props = properties entity
        w = weapon entity
        vNameString = "ID: " <> eid <> ", Name: " <> eName
        vStatsString = vNameString
          <> ", ItemInfo: " <> iiJson
          <> ", Damage: " <> show d
          <> ", Properties: " <> foldl' (++) "" (map (\p -> case JSON.toJSON p of 
            JSON.String s -> T.unpack s
            _ -> "") (case props of WeaponProperties a -> a))
          <> ", Weapon: " <> show w
        vAllString = vStatsString <> ", Children IDs: " <> foldl' (++) "" (map (\c -> childId c ++ ", ") eChildren)

    case v of
      Name -> liftIO $ hPutStrLn stderr vNameString
      Stats -> liftIO $ hPutStrLn stderr vStatsString
      All -> liftIO $ hPutStrLn stderr vAllString

printContainer :: VerbosityLevel -> String -> AppM Env ()
printContainer v eid = err "printContainer"
  [ ("verbosity", show v)
  , ("entity_id", show eid)
  ] $ do
    entity <- getEntityById eid

    let info = entityInfo entity
        eName = name info
        (EntityChildren eChildren) = children info
        iiJson = BS.unpack $ JSON.encode (itemInfo entity)
        cap = capacity entity
        vNameString = "ID: " <> eid <> ", Name: " <> eName
        vStatsString = vNameString <> ", ItemInfo: " <> iiJson <> ", Capacity: " <> cap
        vAllString = vStatsString <> ", Children IDs: " <> foldl' (++) "" (map (\c -> childId c ++ ", ") eChildren)

    case v of
      Name -> liftIO $ hPutStrLn stderr vNameString
      Stats -> liftIO $ hPutStrLn stderr vStatsString
      All -> liftIO $ hPutStrLn stderr vAllString

printMount :: VerbosityLevel -> String -> AppM Env ()
printMount v eid = err "printMount"
  [ ("verbosity", show v)
  , ("entity_id", show eid)
  ] $ do
    entity <- getEntityById eid

    let info = entityInfo entity
        eName = name info
        (EntityChildren eChildren) = children info
        spd = speed entity
        cap = carryingCapacity entity
        vNameString = "ID: " <> eid <> ", Name: " <> eName
        vStatsString = vNameString <> ", Speed: " <> show spd <> ", CarryingCapacity: " <> show cap
        vAllString = vStatsString <> ", Children IDs: " <> foldl' (++) "" (map (\c -> childId c ++ ", ") eChildren)

    case v of
      Name -> liftIO $ hPutStrLn stderr vNameString
      Stats -> liftIO $ hPutStrLn stderr vStatsString
      All -> liftIO $ hPutStrLn stderr vAllString

printSpell :: VerbosityLevel -> String -> AppM Env ()
printSpell v eid = err "printSpell"
  [ ("verbosity", show v)
  , ("entity_id", show eid)
  ] $ do
    entity <- getEntityById eid

    let info = entityInfo entity
        eName = name info
        (EntityChildren eChildren) = children info
        lvl = level entity
        ri = ritual entity
        eAction = action entity
        eRange = range entity
        eComponents = components entity
        eDuration = duration entity
        eTargets = targets entity
        eAoe = aoe entity
        eSave = save entity
        eAttack = attack entity
        vNameString = "ID: " <> eid <> ", Name: " <> eName
        vStatsString = vNameString
          <> ", Level: " <> show lvl
          <> ", Ritual: " <> show ri
          <> ", Action: " <> eAction
          <> ", Range: " <> eRange
          <> ", Components: " <> eComponents
          <> ", Duration: " <> eDuration
          <> ", Targets: " <> eTargets
          <> ", AOE: " <> eAoe
          <> ", Save: " <> eSave
          <> ", Attack: " <> eAttack
        vAllString = vStatsString <> ", Children IDs: " <> foldl' (++) "" (map (\c -> childId c ++ ", ") eChildren)

    case v of
      Name -> liftIO $ hPutStrLn stderr vNameString
      Stats -> liftIO $ hPutStrLn stderr vStatsString
      All -> liftIO $ hPutStrLn stderr vAllString

printMoney :: VerbosityLevel -> String -> AppM Env ()
printMoney v eid = err "printMoney"
  [ ("verbosity", show v)
  , ("entity_id", show eid)
  ] $ do
    entity <- getEntityById eid

    let info = entityInfo entity
        eName = name info
        (EntityChildren eChildren) = children info
        amt = amount entity
        vNameString = "ID: " <> eid <> ", Name: " <> eName
        vStatsString = vNameString <> ", Amount: " <> amt
        vAllString = vStatsString <> ", Children IDs: " <> foldl' (++) "" (map (\c -> childId c ++ ", ") eChildren)

    case v of
      Name -> liftIO $ hPutStrLn stderr vNameString
      Stats -> liftIO $ hPutStrLn stderr vStatsString
      All -> liftIO $ hPutStrLn stderr vAllString

setOutputEntities :: KM.KeyMap Entity -> AppM Env ()
setOutputEntities entities = err "setOutputEntities" [("new_entities", show entities)] $ do
  stateRef <- asks state
  gameState <- liftIO $ readIORef stateRef
  let oldOutput = output gameState
  let newOutput = oldOutput { outEntities = entities }

  liftIO $ atomicModifyIORef' stateRef $ \st ->
    (st { output = newOutput }, ())

addEntityToOutputEntities :: String -> Entity -> AppM Env ()
addEntityToOutputEntities id entity = err "addEntityToOutputEntities" [("entity", show entity)] $ do
  stateRef <- asks state
  gameState <- liftIO $ readIORef stateRef
  let oldOutput = output gameState
  let oldEntities = outEntities oldOutput
  let newEntities = KM.insert (K.fromString id) entity oldEntities

  setOutputEntities newEntities

removeEntityFromOutputEntities :: String -> AppM Env ()
removeEntityFromOutputEntities id = err "removeEntityFromOutputEntities" [("entity_id", show id)] $ do
  stateRef <- asks state
  gameState <- liftIO $ readIORef stateRef
  let oldOutput = output gameState
  let oldEntities = outEntities oldOutput

  setOutputEntities $ KM.delete (K.fromString id) oldEntities

setEntities :: KM.KeyMap Entity -> AppM Env ()
setEntities entities = err "setEntities" [("entities", show entities)] $ do
  stateRef <- asks state

  liftIO $ atomicModifyIORef' stateRef $ \st ->
    (st { entities = entities }, ())

deleteEntity :: String -> AppM Env ()
deleteEntity id = err "deleteEntity" [("entity_id", show id)] $ do
  entities <- getEntities
  
  setEntities $ KM.delete (K.fromString id) entities

getEntitiesByIds :: [String] -> AppM Env (KM.KeyMap Entity)
getEntitiesByIds ids = err "getEntitiesByIds" [("entity_ids", show ids)] $ do
  entities <- getEntities

  return $ case ids of
    [] -> entities
    ids' -> KM.filterWithKey (\k _ -> k `elem` map K.fromString ids') entities

getFocusFromDaemon :: AppM Env [String]
getFocusFromDaemon = err "getFocusFromDaemon" [] $ do
  conn <- refreshSocketConn
  liftIO $ sendAll conn $ C.pack "{ \"action\": \"get\", \"payload\": \"focus\" }"
  r <- liftIO $ timeout 3000000 $ recv conn 4096
  
  case r of
    Nothing -> throwBaseError $ SocketError "Socket timed out"
    Just d -> case JSON.eitherDecode $ BS.fromStrict d :: Either String JSON.Value of
      Left e -> throwBaseError $ ParseError e
      Right (JSON.Array a) -> return $ map (\case
        (JSON.String s) -> T.unpack s
        _ -> "unknown input from daemon") (V.toList a)
      Right _ -> throwBaseError $ JsonValidationError "Expected JSON array for focus list"