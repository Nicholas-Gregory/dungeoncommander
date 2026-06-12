{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module DC.Entity (
  EntityChildType (..),
  EntityChild (..),
  Entity (..),
  EntityChildren (..),
  EntityInfo (..),
  SaveProficiencies(..),
  WeaponProficiencies(..),
  ItemInfo (..),
) where
import DC.Json (FromJson(..), IsJson (fromValue), JsonValue (JsonObject, JsonString, JsonArray), getField, ToJson (toJson))
import qualified Data.Map as M
import Data.List (find)
import DC.Types (Ability (..), WeaponProficiency)

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
    properties :: [String]
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
  fromValue :: JsonValue -> Maybe EntityChild
  fromValue (JsonObject m) = do
    t <- getField "childType" m
    cid <- getField "childId" m
    childTypeValue <- lookup t childTypeMap

    Just $ EntityChild { childType = childTypeValue, childId = cid }
  fromValue _ = Nothing

instance ToJson EntityChild where
  toJson :: EntityChild -> JsonValue
  toJson (EntityChild t cid) = case find ((==t) . snd) childTypeMap of
    Just r -> JsonObject $ M.fromList [("childType", toJson (fst r)), ("childId", toJson cid)]
    Nothing -> JsonObject $ M.fromList [("childType", toJson "unknown"), ("childId", toJson cid)]

instance IsJson EntityChildren where
  fromValue :: JsonValue -> Maybe EntityChildren
  fromValue (JsonArray arr) = EntityChildren <$> traverse fromValue arr
  fromValue _ = Nothing

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
  fromValue :: JsonValue -> Maybe EntityInfo
  fromValue (JsonObject m) = do
    ch <- getField "children" m
    nm <- getField "name" m
    return $ EntityInfo { children = ch, name = nm }
  fromValue _ = Nothing

instance IsJson ItemInfo where
  fromValue :: JsonValue -> Maybe ItemInfo
  fromValue (JsonObject m) = ItemInfo 
    <$> getField "cost" m
    <*> getField "weight" m
  fromValue _ = Nothing

instance IsJson SaveProficiencies where
  fromValue :: JsonValue -> Maybe SaveProficiencies
  fromValue (JsonArray a) = SaveProficiencies <$> traverse fromValue a
  fromValue _ = Nothing

instance ToJson SaveProficiencies where
  toJson :: SaveProficiencies -> JsonValue
  toJson (SaveProficiencies xs) = JsonArray $ map toJson xs

instance IsJson WeaponProficiencies where
  fromValue :: JsonValue -> Maybe WeaponProficiencies
  fromValue (JsonArray a) = WeaponProficiencies <$> traverse fromValue a
  fromValue _ = Nothing

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
  toJson (Weapon eInfo iInfo d props) = JsonObject $ M.fromList
    [
      ("type", toJson "weapon"),
      ("entityInfo", toJson eInfo),
      ("itemInfo", toJson iInfo),
      ("damage", toJson d),
      ("properties", toJson props)
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
  fromJson :: JsonValue -> Maybe Entity
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
      _ -> Nothing
  fromJson _ = Nothing
