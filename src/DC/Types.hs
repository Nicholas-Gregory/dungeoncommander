{-# LANGUAGE InstanceSigs #-}

module DC.Types (
  CheckSuccess,
  Ability(..),
  SimpleMelee(..),
  SimpleRanged(..),
  MartialMelee(..),
  MartialRanged(..),
  Weapon(..),
  WeaponProficiency(..),
  WeaponProficiencies(..),
  SaveProficiencies(..),
  Entity(..),
  GameState(..),
  Env(..),
  EntityChildType(..),
  EntityChildren(..),
  EntityChild(..),
  EntityInfo(..),
  VerbosityLevel(..)
) where
import DC.Json (ToJson (toJson), JsonValue (JsonString, JsonObject, JsonArray), IsJson (fromValue), FromJson (fromJson), getField)
import Data.List (find)
import qualified Data.Map as M
import System.Random
import Control.Monad.Trans
import Data.IORef
import Control.Monad.Reader
import DC.Error

data GameState = GameState 
  { currentScene :: String,
    entities :: M.Map String Entity
  , commits :: [Entity]
  }

data Env = Env
  { socketPath :: FilePath
  , dbPath :: FilePath
  , state :: IORef GameState
  , gen :: StdGen
  }

data VerbosityLevel
  = Name
  | Stats
  | All
  deriving (Show, Eq)

type CheckSuccess = Bool

data Ability = 
  Charisma |
  Intelligence |
  Constitution |
  Strength |
  Dexterity |
  Wisdom
  deriving (Show, Eq)

instance ToJson Ability where
  toJson :: Ability -> JsonValue
  toJson Charisma = toJson "cha"
  toJson Intelligence = toJson "int"
  toJson Constitution = toJson "con"
  toJson Strength = toJson "str"
  toJson Dexterity = toJson "dex"
  toJson Wisdom = toJson "wis"

instance IsJson Ability where
  fromValue :: JsonValue -> Either AppError Ability
  fromValue (JsonString "cha") = Right Charisma
  fromValue (JsonString "int") = Right Intelligence
  fromValue (JsonString "con") = Right Constitution
  fromValue (JsonString "str") = Right Strength
  fromValue (JsonString "dex") = Right Dexterity
  fromValue (JsonString "wis") = Right Wisdom
  fromValue _ = Left 
    $ newBaseError 
    $ EntityValidationError "Expected a valid ability"

data SimpleMelee =
  Club |
  Dagger |
  Greatclub |
  Handaxe |
  Javelin |
  LightHammer |
  Mace |
  Quarterstaff |
  Sickle |
  Spear
  deriving (Show, Eq)

data SimpleRanged =
  LightCrossbow |
  Dart |
  Shortbow |
  Sling
  deriving (Show, Eq)

data MartialMelee = 
  Battleaxe |
  Flail |
  Glaive |
  Greataxe |
  Greatsword |
  Halberd |
  Lance |
  Longsword |
  Maul |
  Morningstar |
  Pike |
  Rapier |
  Scimitar |
  Shortsword |
  Trident |
  WarPick |
  Warhammer |
  Whip
  deriving (Show, Eq)

data MartialRanged =
  Blowgun |
  HandCrossbow |
  HeavyCrossbow |
  Longbow |
  Net
  deriving (Show, Eq)

data Weapon =
  SimpleMelee SimpleMelee |
  SimpleRanged SimpleRanged |
  MartialMelee MartialMelee |
  MartialRanged MartialRanged
  deriving (Show, Eq)

weaponNameTable :: [(Weapon, String)]
weaponNameTable = [ (SimpleMelee Club, "club")
                   ,(SimpleMelee Dagger, "dagger")
                   ,(SimpleMelee Greatclub, "greatclub")
                   ,(SimpleMelee Handaxe, "handaxe")
                   ,(SimpleMelee Javelin, "javelin")
                   ,(SimpleMelee LightHammer, "light hammer")
                   ,(SimpleMelee Mace, "mace")
                   ,(SimpleMelee Quarterstaff, "quarterstaff")
                   ,(SimpleMelee Sickle, "sickle")
                   ,(SimpleMelee Spear, "spear")
                   ,(SimpleRanged LightCrossbow, "crossbow, light")
                   ,(SimpleRanged Dart, "dart")
                   ,(SimpleRanged Shortbow, "shortbow")
                   ,(SimpleRanged Sling, "sling")
                   ,(MartialMelee Battleaxe, "battleaxe")
                   ,(MartialMelee Flail, "flail")
                   ,(MartialMelee Glaive, "glaive")
                   ,(MartialMelee Greataxe, "greataxe")
                   ,(MartialMelee Greatsword, "greatsword")
                   ,(MartialMelee Halberd, "halberd")
                   ,(MartialMelee Lance, "lance")
                   ,(MartialMelee Longsword, "longsword")
                   ,(MartialMelee Maul, "maul")
                   ,(MartialMelee Morningstar, "morningstar")
                   ,(MartialMelee Pike, "pike")
                   ,(MartialMelee Rapier, "rapier")
                   ,(MartialMelee Scimitar, "scimitar")
                   ,(MartialMelee Shortsword, "shortsword")
                   ,(MartialMelee Trident, "trident")
                   ,(MartialMelee WarPick, "war pick")
                   ,(MartialMelee Warhammer, "warhammer")
                   ,(MartialMelee Whip, "whip")
                   ,(MartialRanged Blowgun, "blowgun")
                   ,(MartialRanged HandCrossbow, "crossbow, hand")
                   ,(MartialRanged HeavyCrossbow, "crossbow, heavy")
                   ,(MartialRanged Longbow, "longbow")
                   ,(MartialRanged Net, "net")
                  ]

instance ToJson SimpleMelee where
  toJson :: SimpleMelee -> JsonValue
  toJson w = maybe (toJson "unknown weapon") toJson (lookup (SimpleMelee w) weaponNameTable)

instance ToJson SimpleRanged where
  toJson :: SimpleRanged -> JsonValue
  toJson w = maybe (toJson "unknown weapon") toJson (lookup (SimpleRanged w) weaponNameTable)

instance ToJson MartialMelee where
  toJson :: MartialMelee -> JsonValue
  toJson w = maybe (toJson "unknown weapon") toJson (lookup (MartialMelee w) weaponNameTable)

instance ToJson MartialRanged where
  toJson :: MartialRanged -> JsonValue
  toJson w = maybe (toJson "unknown weapon") toJson (lookup (MartialRanged w) weaponNameTable)

instance IsJson SimpleMelee where
  fromValue :: JsonValue -> Either AppError SimpleMelee
  fromValue (JsonString s) =
    case fmap fst (find ((== s) . snd) weaponNameTable) of
      Just (SimpleMelee w') -> Right w'
      Just x                -> Left 
        $ newBaseError
        $ EntityValidationError 
        $ show x
        <> " is not a SimpleMelee"
      Nothing               -> Left 
        $ newBaseError
        $ EntityValidationError "Unknown weapon name for SimpleMelee"
  fromValue x = Left 
    $ newBaseError
    $ EntityValidationError 
    $ "Expected JSON string for SimpleMelee, found: "
    <> show x

instance IsJson SimpleRanged where
  fromValue :: JsonValue -> Either AppError SimpleRanged
  fromValue (JsonString s) = 
    case fst <$> find ((==s) . snd) weaponNameTable of
      Just (SimpleRanged w') -> Right w'
      Just x -> Left 
        $ newBaseError
        $ EntityValidationError
        $ show x
        <> " is not a SimpleRanged"
      Nothing -> Left 
        $ newBaseError
        $ EntityValidationError "Unknown weapon name for SimpleRanged"
  fromValue x = Left 
    $ newBaseError
    $ EntityValidationError 
    $ "Expected JSON string for SimpleRanged, found: "
    <> show x

instance IsJson MartialMelee where
  fromValue :: JsonValue -> Either AppError MartialMelee
  fromValue (JsonString s) =
    case fst <$> find ((==s) . snd) weaponNameTable of
      Just (MartialMelee w') -> Right w'
      Just x -> Left 
        $ newBaseError
        $ EntityValidationError
        $ show x
        <> " is not a MartialMelee"
      Nothing -> Left 
        $ newBaseError
        $ EntityValidationError "Unknown weapon name for MartialMelee"
  fromValue x = Left 
    $ newBaseError
    $ EntityValidationError 
    $ "Expected JSON string for MartialMelee, found: "
    <> show x

instance IsJson MartialRanged where
  fromValue :: JsonValue -> Either AppError MartialRanged
  fromValue (JsonString s) =
    case fst <$> find ((==s) . snd) weaponNameTable of
      Just (MartialRanged w') -> Right w'
      Just x -> Left 
        $ newBaseError
        $ EntityValidationError
        $ show x
        <> " is not a MartalRanged"
      Nothing -> Left 
        $ newBaseError
        $ EntityValidationError "Unknown weapon name for MartialRanged"
  fromValue x = Left 
    $ newBaseError
    $ EntityValidationError 
    $ "Expected JSON string for MartialMelee, found: "
    <> show x

instance IsJson Weapon where
  fromValue :: JsonValue -> Either AppError Weapon
  fromValue v =
    case (fromValue v :: Either AppError SimpleMelee) of
      Right w -> Right (SimpleMelee w)
      Left _ -> case (fromValue v :: Either AppError SimpleRanged) of
        Right w -> Right (SimpleRanged w)
        Left _ -> case (fromValue v :: Either AppError MartialMelee) of
          Right w -> Right (MartialMelee w)
          Left _ -> case (fromValue v :: Either AppError MartialRanged) of
            Right w -> Right (MartialRanged w)
            Left x -> Left 
              $ newBaseError
              $ EntityValidationError
              $ "Expected JSON value for a Weapon, found: "
              <> show x

instance ToJson Weapon where
  toJson :: Weapon -> JsonValue
  toJson (SimpleMelee w)   = toJson w
  toJson (SimpleRanged w)  = toJson w
  toJson (MartialMelee w)  = toJson w
  toJson (MartialRanged w) = toJson w

data WeaponProficiency =
  Simple |
  Martial |
  Specific Weapon
  deriving (Show, Eq)

instance ToJson WeaponProficiency where
  toJson :: WeaponProficiency -> JsonValue
  toJson Simple = toJson "simple"
  toJson Martial = toJson "martial"
  toJson (Specific w) = toJson w

instance IsJson WeaponProficiency where
  fromValue :: JsonValue -> Either AppError WeaponProficiency
  fromValue (JsonString "simple") = Right Simple
  fromValue (JsonString "martial") = Right Martial
  fromValue v = Specific <$> fromValue v

data EntityChildType =
  ActorLocation |
  ObjectLocation |
  CarriedItem |
  ContainedItem |
  HeldItem |
  Condition |
  KnownSpell |
  PreparedSpell |
  DonnedArmor |
  WieldedWeapon
  deriving (Show, Eq)

data EntityChild = EntityChild { 
  childType :: EntityChildType,
  childId :: String
} deriving (Show, Eq)

newtype EntityChildren = EntityChildren [EntityChild] deriving (Show, Eq)

data EntityInfo = EntityInfo { 
  children :: EntityChildren, 
  name :: String
} deriving (Show, Eq)

newtype SaveProficiencies = SaveProficiencies [Ability] deriving (Show, Eq)

newtype WeaponProficiencies = WeaponProficiencies [WeaponProficiency] deriving (Show, Eq)

data ItemInfo = ItemInfo { cost :: String, weight :: String } deriving (Show, Eq)

data Entity = 
  Scene {
    entityInfo :: EntityInfo,
    dimensions :: (Int, Int)
  } |
  Actor {
    entityInfo :: EntityInfo,
    position :: (Int, Int),
    currentHp :: Int,
    maxHp :: Int,
    cha :: Int,
    int :: Int,
    con :: Int,
    str :: Int,
    dex :: Int,
    wis :: Int,
    hitDice :: String,
    ac :: Int,
    level :: Int,
    saveProficiencies :: SaveProficiencies,
    weaponProficiencies :: WeaponProficiencies
    -- spellSaveDc :: Int,
    -- spellAttackBonus :: Int,
    -- passivePerception :: Int,
    -- proficiencyBonus :: Int,
    -- spellSlots :: [Int]
  } |
  Object {
    entityInfo :: EntityInfo,
    ac :: Int,
    maxHp :: Int,
    currentHp :: Int,
    position :: (Int, Int)
  } |
  Trap {
    entityInfo :: EntityInfo,
    detectDc :: Int,
    attackBonus :: Int,
    saveDc :: Int,
    damage :: (String, String),
    position ::  (Int, Int)
  } |
  Item {
    entityInfo :: EntityInfo,
    itemInfo :: ItemInfo
  } |
  Armor {
    entityInfo :: EntityInfo,
    itemInfo :: ItemInfo,
    ac :: Int,
    str :: Int,
    stealthDisadvantage :: Bool,
    armorType :: String
  } |
  Weapon {
    entityInfo :: EntityInfo,
    itemInfo :: ItemInfo,
    damage :: (String, String),
    properties :: [String],
    weapon :: Weapon
  } |
  Container {
    entityInfo :: EntityInfo,
    itemInfo :: ItemInfo,
    capacity :: String
  } |
  Mount {
    entityInfo :: EntityInfo,
    speed :: Int,
    carryingCapacity :: Int
  } |
  Spell {
    entityInfo :: EntityInfo,
    level :: Int,
    ritual :: Bool,
    action :: String,
    range :: String,
    components :: String,
    duration :: String,
    targets :: String,
    aoe :: String,
    save :: String,
    attack :: String
  } | 
  Money {
    entityInfo :: EntityInfo,
    amount :: String
  } deriving (Show, Eq)

childTypeMap :: [(String, EntityChildType)]
childTypeMap = 
  [
   ("actorlocation", ActorLocation)
  ,("objectlocation", ObjectLocation)
  ,("carrieditem", CarriedItem)
  ,("containeditem", ContainedItem)
  ,("helditem", HeldItem)
  ,("condition", Condition)
  ,("knownspell", KnownSpell)
  ,("preparedspell", PreparedSpell)
  ,("donnedarmor", DonnedArmor)
  ,("wieldedweapon", WieldedWeapon)
  ]

instance IsJson EntityChild where
  fromValue :: JsonValue -> Either AppError EntityChild
  fromValue (JsonObject m) = do
    t <- getField "childType" m
    cid <- getField "childId" m
    childTypeValue <- case lookup t childTypeMap of
      Just v -> Right v
      Nothing -> Left 
        $ newBaseError 
        $ EntityValidationError 
        $ "Unkown child type "
        <> show t

    return $ EntityChild { childType = childTypeValue, childId = cid }
  fromValue x = Left
    $ newBaseError
    $ EntityValidationError
    $ "Expected JSON Object for child type, found: "
    <> show x

instance ToJson EntityChild where
  toJson :: EntityChild -> JsonValue
  toJson (EntityChild t cid) = case find ((==t) . snd) childTypeMap of
    Just r -> JsonObject $ M.fromList [("childType", toJson (fst r)), ("childId", toJson cid)]
    Nothing -> JsonObject $ M.fromList [("childType", toJson "unknown"), ("childId", toJson cid)]

instance IsJson EntityChildren where
  fromValue :: JsonValue -> Either AppError EntityChildren
  fromValue (JsonArray arr) = EntityChildren <$> traverse fromValue arr
  fromValue x = Left
    $ newBaseError
    $ EntityValidationError
    $ "Expected JSON Array for entity children, found: "
    <> show x

instance ToJson EntityChildren where
  toJson :: EntityChildren -> JsonValue
  toJson (EntityChildren xs) = JsonArray $ map toJson xs

instance ToJson EntityInfo where
  toJson :: EntityInfo -> JsonValue
  toJson (EntityInfo c n) = JsonObject $ M.fromList [("children", toJson c), ("name", toJson n)]

instance ToJson ItemInfo where
  toJson :: ItemInfo -> JsonValue
  toJson (ItemInfo c w) = JsonObject $ M.fromList [("cost", toJson c), ("weight", toJson w)]

instance IsJson EntityInfo where
  fromValue :: JsonValue -> Either AppError EntityInfo
  fromValue (JsonObject m) = do
    ch <- getField "children" m
    nm <- getField "name" m
    return $ EntityInfo { children = ch, name = nm }
  fromValue x = Left
    $ newBaseError
    $ EntityValidationError
    $ "Expected JSON Object for EntityInfo, found: "
    <> show x

instance IsJson ItemInfo where
  fromValue :: JsonValue -> Either AppError ItemInfo
  fromValue (JsonObject m) = ItemInfo 
    <$> getField "cost" m
    <*> getField "weight" m
  fromValue x = Left
    $ newBaseError
    $ EntityValidationError
    $ "Expected JSON Object for ItemInfo, found: "
    <> show x

instance IsJson SaveProficiencies where
  fromValue :: JsonValue -> Either AppError SaveProficiencies
  fromValue (JsonArray a) = SaveProficiencies <$> traverse fromValue a
  fromValue x = Left
    $ newBaseError
    $ EntityValidationError
    $ "Expected JSON Array for SaveProficiencies, found: "
    <> show x

instance ToJson SaveProficiencies where
  toJson :: SaveProficiencies -> JsonValue
  toJson (SaveProficiencies xs) = JsonArray $ map toJson xs

instance IsJson WeaponProficiencies where
  fromValue :: JsonValue -> Either AppError WeaponProficiencies
  fromValue (JsonArray a) = WeaponProficiencies <$> traverse fromValue a
  fromValue x = Left
    $ newBaseError
    $ EntityValidationError
    $ "Expected JSON Array for WeaponProficiencies, found: "
    <> show x

instance ToJson WeaponProficiencies where
  toJson :: WeaponProficiencies -> JsonValue
  toJson (WeaponProficiencies xs) = JsonArray $ map toJson xs

instance ToJson Entity where
  toJson :: Entity -> JsonValue
  toJson (Scene info dim) = JsonObject $ M.fromList
    [
      ("type", toJson "scene"),
      ("entityInfo", toJson info),
      ("dimensions", toJson dim)
    ]
  toJson (Actor info pos chp mhp cha int con str dex wis hd ac l spro wpro) = JsonObject $ M.fromList
    [
      ("type", toJson "actor"),
      ("entityInfo", toJson info),
      ("position", toJson pos),
      ("currentHp", toJson chp),
      ("maxHp", toJson mhp),
      ("cha", toJson cha),
      ("int", toJson int),
      ("con", toJson con),
      ("str", toJson str),
      ("dex", toJson dex),
      ("wis", toJson wis),
      ("hitDice", toJson hd),
      ("ac", toJson ac),
      ("level", toJson l),
      ("saveProficiencies", toJson spro),
      ("weaponProficiencies", toJson wpro)
    ]
  toJson (Object info ac mhp chp pos) = JsonObject $ M.fromList
    [
      ("type", toJson "object"),
      ("entityInfo", toJson info),
      ("ac", toJson ac),
      ("maxHp", toJson mhp),
      ("currentHp", toJson chp),
      ("position", toJson pos)
    ]
  toJson (Trap info ddc ab sdc d pos) = JsonObject $ M.fromList
    [
      ("type", toJson "trap"),
      ("entityInfo", toJson info),
      ("detectDc", toJson ddc),
      ("attackBonus", toJson ab),
      ("saveDc", toJson sdc),
      ("damage", toJson d),
      ("position", toJson pos)
    ]
  toJson (Item eInfo iInfo) = JsonObject $ M.fromList
    [
      ("type", toJson "item"),
      ("entityInfo", toJson eInfo),
      ("itemInfo", toJson iInfo)
    ]
  toJson (Armor eInfo iInfo ac str sd at) = JsonObject $ M.fromList
    [
      ("type", toJson "armor"),
      ("entityInfo", toJson eInfo),
      ("itemInfo", toJson iInfo),
      ("ac", toJson ac),
      ("str", toJson str),
      ("stealthDisadvantage", toJson sd),
      ("armorType", toJson at)
    ]
  toJson (Weapon eInfo iInfo d props weapon) = JsonObject $ M.fromList
    [
      ("type", toJson "weapon"),
      ("entityInfo", toJson eInfo),
      ("itemInfo", toJson iInfo),
      ("damage", toJson d),
      ("properties", toJson props),
      ("weapon", toJson weapon)
    ]
  toJson (Container eInfo iInfo c) = JsonObject $ M.fromList
    [
      ("type", toJson "container"),
      ("entityInfo", toJson eInfo),
      ("itemInfo", toJson iInfo),
      ("capacity", toJson c)
    ]
  toJson (Mount eInfo s c) = JsonObject $ M.fromList
    [
      ("type", toJson "mount"),
      ("entityInfo", toJson eInfo),
      ("speed", toJson s),
      ("carryingCapacity", toJson c)
    ]
  toJson (Spell info l ri a ra c d t aoe save att) = JsonObject $ M.fromList
    [
      ("type", toJson "spell"),
      ("entityInfo", toJson info),
      ("level", toJson l),
      ("ritual", toJson ri),
      ("action", toJson a),
      ("range", toJson ra),
      ("components", toJson c),
      ("duration", toJson d),
      ("targets", toJson t),
      ("aoe", toJson aoe),
      ("save", toJson save),
      ("attack", toJson att)
    ]
  toJson (Money info a) = JsonObject $ M.fromList
    [
      ("type", toJson "money"),
      ("entityInfo", toJson info),
      ("amount", toJson a)
    ]

instance FromJson Entity where
  fromJson :: JsonValue -> Either AppError Entity
  fromJson (JsonObject o) = do
    t <- getField "type" o

    case t :: String of
      "actor" -> Actor 
        <$> getField "entityInfo" o
        <*> getField "position" o
        <*> getField "currentHp" o
        <*> getField "maxHp" o
        <*> getField "cha" o
        <*> getField "int" o
        <*> getField "con" o
        <*> getField "str" o
        <*> getField "dex" o
        <*> getField "wis" o
        <*> getField "hitDice" o
        <*> getField "ac" o
        <*> getField "level" o
        <*> getField "saveProficiencies" o
        <*> getField "weaponProficiencies" o
      "scene" -> Scene
        <$> getField "entityInfo" o
        <*> getField "dimensions" o
      "object" -> Object
        <$> getField "entityInfo" o
        <*> getField "ac" o
        <*> getField "maxHp" o
        <*> getField "currentHp" o
        <*> getField "position" o
      "trap" -> Trap
        <$> getField "entityInfo" o
        <*> getField "detectDc" o
        <*> getField "attackBonus" o
        <*> getField "saveDc" o
        <*> getField "damage" o
        <*> getField "position" o
      "item" -> Item
        <$> getField "entityInfo" o
        <*> getField "itemInfo" o
      "armor" -> Armor
        <$> getField "entityInfo" o
        <*> getField "itemInfo" o
        <*> getField "ac" o
        <*> getField "str" o
        <*> getField "stealthDisadvantage" o
        <*> getField "armorType" o
      "weapon" -> Weapon
        <$> getField "entityInfo" o
        <*> getField "itemInfo" o
        <*> getField "damage" o
        <*> getField "properties" o
        <*> getField "weapon" o
      "container" -> Container
        <$> getField "entityInfo" o
        <*> getField "itemInfo" o
        <*> getField "capacity" o
      "mount" -> Mount
        <$> getField "entityInfo" o
        <*> getField "speed" o
        <*> getField "carryingCapacity" o
      "spell" -> Spell
        <$> getField "entityInfo" o
        <*> getField "level" o
        <*> getField "ritual" o
        <*> getField "action" o
        <*> getField "range" o
        <*> getField "components" o
        <*> getField "duration" o
        <*> getField "targets" o
        <*> getField "aoe" o
        <*> getField "save" o
        <*> getField "attack" o
      "money" -> Money
        <$> getField "entityInfo" o
        <*> getField "amount" o
      x -> Left
        $ newBaseError
        $ EntityValidationError
        $ "Unknown Entity type '"
        <> show x
        <> "'"
  fromJson x = Left
    $ newBaseError
    $ EntityValidationError
    $ "Expected JSON Object for Entity, found: "
    <> show x
