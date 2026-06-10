{-# LANGUAGE InstanceSigs #-}

module DC.Entity (
  EntityChildType (..),
  EntityChild (..),
  Entity (..),
  EntityChildren (..),
  EntityInfo (..),
  Proficiency(..),
  ItemInfo (..)
) where
import DC.Json (FromJson(..), IsJson (fromValue), JsonValue (JsonObject, JsonString, JsonArray), getField)
import qualified Data.Map as M

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
  deriving (Show)

data EntityChild = EntityChild { 
  childType :: EntityChildType,
  childId :: String
} deriving (Show)

newtype EntityChildren = EntityChildren [EntityChild] deriving (Show)

data EntityInfo = EntityInfo { 
  children :: EntityChildren, 
  name :: String
} deriving (Show)

data Proficiency = Proficiency { proficiencyType :: String, proficiencyName :: String }

data ItemInfo = ItemInfo { cost :: String, weight :: String } deriving (Show)

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
    level :: Int
    -- spellSaveDc :: Int,
    -- spellAttackBonus :: Int,
    -- passivePerception :: Int,
    -- proficiencyBonus :: Int,
    -- proficiencies :: [Proficiency],
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
  } deriving (Show)

instance IsJson EntityChild where
  fromValue :: JsonValue -> Maybe EntityChild
  fromValue (JsonObject m)
    | (Just (JsonString t), Just (JsonString cid)) <- (,) (M.lookup "childType" m) (M.lookup "childId" m) = case t of
      "actorlocation" -> Just $ EntityChild { childType = ActorLocation, childId = cid }
      "objectlocation" -> Just $ EntityChild { childType = ObjectLocation, childId = cid }
      "carrieditem" -> Just $ EntityChild { childType = CarriedItem, childId = cid }
      "containeditem" -> Just $ EntityChild { childType = ContainedItem, childId = cid }
      "helditem" -> Just $ EntityChild { childType = HeldItem, childId = cid }
      "condition" -> Just $ EntityChild { childType = Condition, childId = cid }
      "knownspell" -> Just $ EntityChild { childType = KnownSpell, childId = cid }
      "preparedspell" -> Just $ EntityChild { childType = PreparedSpell, childId = cid }
      "donnedarmor" -> Just $ EntityChild { childType = DonnedArmor, childId = cid }
      "wieldedweapon" -> Just $ EntityChild { childType = WieldedWeapon, childId = cid }
      _ -> Nothing
    | otherwise = Nothing
  fromValue _ = Nothing

instance IsJson EntityChildren where
  fromValue :: JsonValue -> Maybe EntityChildren
  fromValue (JsonArray arr) = EntityChildren <$> traverse fromValue arr
  fromValue _ = Nothing

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