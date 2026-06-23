{-# LANGUAGE NamedFieldPuns #-}

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
  diceRollResult
) where
import DC.Types
import qualified DC.Types (Entity(..), EntityInfo(..), EntityChildType(..), EntityChildren(..), EntityChild(..), SaveProficiencies(..), WeaponProficiencies(..), Ability(..), CheckSuccess, WeaponProficiency (Simple, Martial, Specific), Weapon (SimpleMelee, SimpleRanged, MartialMelee, MartialRanged))
import DC.Dice (rollDice, processExpression)
import System.Random (StdGen, getStdGen)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (atomicModifyIORef', readIORef)
import Control.Monad.Reader (asks, MonadTrans (lift))
import qualified Data.Map as M
import System.IO (hPutStrLn, stderr, getContents', hIsTerminalDevice, stdin)
import DC.Error (AppError (AppError), newBaseError, ErrorDetail (OtherError, CliParseError, SocketError, JsonValidationError), throwBaseError, err, AppM)
import System.Environment (getArgs)
import DC.Parse (Parser(runParser))
import Control.Applicative (Alternative(empty))
import Control.Monad.Except (MonadError(throwError))
import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.ByteString.Char8 as C
import DC.Json (JsonValue (JsonObject, JsonString), jsonObject, FromJson (fromJson), JsonObjectMap)
import System.Timeout (timeout)
import Data.Foldable (Foldable(foldl'))

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
attackRoll gen (Weapon { weapon }) entity ability dc = do
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
    let newEntities = M.insert k e (entities st)
    in (GameState {commits=commits st, entities=newEntities, currentScene=currentScene st}, ())

addChild :: EntityChildType -> String -> String -> AppM Env ()
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
          ) p (entities st)
    in (GameState {commits=commits st, entities=newEntities, currentScene=currentScene st}, ())

removeChild :: EntityChildType -> String -> String -> AppM Env ()
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
          ) p (entities st)
    in (st { entities = newEntities }, ())

tooFewArgumentsError :: AppM Env ()
tooFewArgumentsError = liftIO $ hPutStrLn stderr "Too few arguments"

getJsonFromDaemon :: Socket -> AppM Env JsonValue
getJsonFromDaemon sock = err "getJsonFromDaemon" [] $ do
  liftIO $ sendAll sock $ C.pack "{ \"action\": \"get\", \"payload\": \"all\"}"
  r <- liftIO $ timeout 3000000 $ recv sock 4096

  case r of
    Nothing -> throwBaseError $ SocketError "Socket timed out"
    Just d -> case runParser jsonObject (C.unpack d) of
      Left e -> throwError e
      Right o -> return $ JsonObject $ fst o

getJsonFromInput :: AppM Env JsonValue
getJsonFromInput = err "getJsonFromInput" [] $ do
  input <- liftIO getContents

  case runParser jsonObject input of
    Left e -> throwError e
    Right o -> return $ JsonObject $ fst o

getJson :: Socket -> AppM Env JsonValue
getJson sock = do
  isTerm <- liftIO $ hIsTerminalDevice stdin
  if isTerm
    then do getJsonFromDaemon sock
    else getJsonFromInput

initCurrentScene :: JsonValue -> AppM Env ()
initCurrentScene (JsonObject o) = err "initCurrentScene" [("input_json", show o)] $ do
  case M.lookup "currentScene" o of
    Nothing -> throwBaseError $ JsonValidationError "No \"currentScene\" field in input JSON"
    Just (JsonString s) -> do
      stateRef <- asks state

      liftIO $ atomicModifyIORef' stateRef $ \st ->
        (st { currentScene = s }, ())
    Just x -> throwBaseError 
      $ JsonValidationError 
      $ "Expected JSON String for \"currentScene\", found: "
      <> show x
initCurrentScene x = err "initCurrentScene" [("input_json", show x)] $ throwBaseError
  $ JsonValidationError "Expected JSON Object"

getEntityJson :: JsonValue -> AppM Env (M.Map String JsonValue)
getEntityJson (JsonObject o) = err "getEntityJson" [("input_json", show o)] $ do 
  case M.lookup "entities" o of
    Nothing -> throwBaseError $ JsonValidationError "Need \"entities\" field in input JSON"
    Just (JsonObject entities) -> return entities
    Just _ -> throwBaseError $ JsonValidationError "Expected JSON Object for \"entities\" field"
getEntityJson x = err "getEntityJson" [("input_json", show x)] $ throwBaseError
  $ JsonValidationError "Expected JSON Object"

initEntities :: JsonValue -> AppM Env ()
initEntities o = err "initEntities" [("input_json", show o)] $ do
  entityMap <- getEntityJson o

  case traverse fromJson entityMap of
    Left e -> throwError e
    Right entities -> do
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

  case M.lookup entityId gameEntities of
    Nothing -> throwBaseError $ OtherError "Entity with this ID does not exist in state"
    Just entity -> return entity

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
        vAllString = vNameString <> ", Children IDs: " <> foldl' (++) "" (map (\c -> childId c ++ ", ") eChildren)
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