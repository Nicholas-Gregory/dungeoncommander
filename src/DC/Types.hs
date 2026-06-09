module Types (
  EntityChildType (..),
  EntityChild (..),
  Entity (..),
  EntityChildren (..),
  EntityInfo (..),
  Proficiency(..),
  ItemInfo (..)
) where

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

newtype EntityChild = EntityChild { childType :: EntityChildType }

newtype EntityChildren = EntityChildren [EntityChild]

data EntityInfo = EntityInfo { 
  children :: EntityChildren, 
  parent :: Entity,
  id :: String,
  name :: String
}

data Proficiency = Proficiency { proficiencyType :: String, proficiencyName :: String }

data ItemInfo = ItemInfo { cost :: String, weight :: String }

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
    spellSaveDc :: Int,
    spellAttackBonus :: Int,
    passivePerception :: Int,
    proficiencyBonus :: Int,
    proficiencies :: [Proficiency],
    spellSlots :: [Int]
  } |
  Object {
    entityInfo :: EntityInfo,
    ac :: Int,
    maxHp :: Int,
    currentHp :: Int
  } |
  Trap {
    entityInfo :: EntityInfo,
    detectDc :: Int,
    attackBonus :: Int,
    saveDc :: Int,
    damage :: (String, String)
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
  }