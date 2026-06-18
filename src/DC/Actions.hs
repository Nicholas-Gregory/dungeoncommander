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
  parseCliArgs,
  getJsonFromDaemon,
  getJsonFromInput
) where
import DC.Types
import qualified DC.Types (Entity(..), EntityInfo(..), EntityChildType(..), EntityChildren(..), EntityChild(..), SaveProficiencies(..), WeaponProficiencies(..), Ability(..), CheckSuccess, WeaponProficiency (Simple, Martial, Specific), Weapon (SimpleMelee, SimpleRanged, MartialMelee, MartialRanged))
import DC.Dice (rollDice)
import System.Random (StdGen)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (atomicModifyIORef')
import Control.Monad.Reader (asks, MonadTrans (lift))
import qualified Data.Map as M
import System.IO (hPutStrLn, stderr, getContents')
import DC.Error (AppError (AppError), newBaseError, ErrorDetail (OtherError, CliParseError, SocketError, JsonValidationError), throwBaseError)
import DC.Opts (Option, cliArg)
import System.Environment (getArgs)
import DC.Parse (Parser(runParser))
import Control.Applicative (Alternative(empty))
import Control.Monad.Except (MonadError(throwError))
import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.ByteString.Char8 as C
import DC.Json (JsonValue (JsonObject), jsonObject)
import System.Timeout (timeout)

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
  
saveEntity :: String -> Entity -> GameM ()
saveEntity k e = do
  stateRef <- asks state

  liftIO $ atomicModifyIORef' stateRef $ \st ->
    let newEntities = M.insert k e (entities st)
    in (GameState {commits=commits st, entities=newEntities, currentScene=currentScene st, gen=gen st}, ())

addChild :: EntityChildType -> String -> String -> GameM ()
addChild t p c = do
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
    in (GameState {commits=commits st, entities=newEntities, currentScene=currentScene st, gen=gen st}, ())

removeChild :: EntityChildType -> String -> String -> GameM ()
removeChild t p c = do
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

tooFewArgumentsError :: GameM ()
tooFewArgumentsError = liftIO $ hPutStrLn stderr "Too few arguments"

parseCliArgs :: GameM [Option]
parseCliArgs = do
  args <- liftIO getArgs
  let result = map fst <$> traverse (runParser cliArg) args

  case result of
    Left err -> throwError err
    Right opts -> return opts

getJsonFromDaemon :: Socket -> GameM JsonValue
getJsonFromDaemon sock = do
  liftIO $ sendAll sock $ C.pack "{ \"action\": \"get\", \"payload\": \"all\"}"
  r <- liftIO $ timeout 3000000 $ recv sock 4096

  case r of
    Nothing -> lift $ throwBaseError $ SocketError "Socket timed out"
    Just d -> case runParser jsonObject (C.unpack d) of
      Left e -> throwError e
      Right o -> return $ JsonObject $ fst o

getJsonFromInput :: GameM JsonValue
getJsonFromInput = do
  input <- liftIO getContents

  case runParser jsonObject input of
    Left e -> throwError e
    Right o -> return $ JsonObject $ fst o

getAllEntities :: JsonValue -> Either AppError JsonValue
getAllEntities (JsonObject json) = case M.lookup "entities" json of
    Nothing -> Left 
      $ newBaseError 
      $ JsonValidationError "Could not find \"entities\" field in JSON document"
    Just e -> Right e
getAllEntities x = Left
  $ newBaseError
  $ JsonValidationError
  $ "Expected JSON Object for getAllEntites, found: "
  <> show x