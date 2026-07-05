{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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
  VerbosityLevel(..),
  WeaponProperty(..),
  WeaponProperties(..),
  DEnv(..),
  DState(..),
  DaemonM,
  Output(..),
  ItemInfo(..),
  DamageType(..)
) where
import Data.List (find)
import qualified Data.Map as M
import System.Random
import Control.Monad.Trans
import Data.IORef
import Control.Monad.Reader
import DC.Error
import DC.Parse
import Control.Applicative (Alternative((<|>)), optional)
import Control.Monad.Except (ExceptT)
import Network.Socket (Socket)
import Data.Char
import GHC.Generics (Generic)
import Data.Aeson (Value, FromJSON, ToJSON)
import Data.Aeson.KeyMap
import qualified Data.Aeson.KeyMap as KM

data Output = Output
  { outEntities :: KM.KeyMap Entity
  , outActions :: KM.KeyMap Value
  , outError :: Maybe AppError}

data GameState = GameState 
  { currentScene :: String,
    entities :: KeyMap Entity
  , output :: Output
  , commits :: [Entity]
  }

data Env = Env
  { socketPath :: FilePath
  , dbPath :: FilePath
  , state :: IORef GameState
  , gen :: StdGen
  , isTerm :: Bool
  }

data DState = DState
  { focusedEntities :: [String]
  , recentEdits :: [M.Map String Entity]
  }

type DaemonM a = ReaderT DEnv (ExceptT AppError IO) a

data DEnv = DEnv
  { dSocketPath :: FilePath 
  , dDbPath :: FilePath
  , dState :: IORef DState
  , dConn :: Socket}

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
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

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
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data SimpleRanged =
  LightCrossbow |
  Dart |
  Shortbow |
  Sling
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

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
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data MartialRanged =
  Blowgun |
  HandCrossbow |
  HeavyCrossbow |
  Longbow |
  Net
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Weapon =
  SimpleMelee SimpleMelee |
  SimpleRanged SimpleRanged |
  MartialMelee MartialMelee |
  MartialRanged MartialRanged
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data WeaponProperty 
  = Ammunition
  | Finesse
  | Heavy
  | Light
  | Loading
  | Range (Int, Int)
  | Reach
  | Special
  | Thrown
  | TwoHanded
  | Versatile Int
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype WeaponProperties = WeaponProperties [WeaponProperty] deriving (Show, Eq, Generic, FromJSON, ToJSON)

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

data WeaponProficiency =
  Simple |
  Martial |
  Specific Weapon
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data DamageType
  = Acid
  | Bludgeoning
  | Cold
  | Fire
  | Force
  | Lightning
  | Necrotic
  | Piercing
  | Poison
  | Psychic
  | Radiant
  | Slashing
  | Thunder
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

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
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data EntityChild = EntityChild { 
  childType :: EntityChildType,
  childId :: String
} deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype EntityChildren = EntityChildren [EntityChild] deriving (Show, Eq, Generic, FromJSON, ToJSON)

data EntityInfo = EntityInfo { 
  children :: EntityChildren, 
  name :: String
} deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype SaveProficiencies = SaveProficiencies [Ability] deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype WeaponProficiencies = WeaponProficiencies [WeaponProficiency] deriving (Show, Eq, Generic, FromJSON, ToJSON)

data ItemInfo = ItemInfo { cost :: String, weight :: String } deriving (Show, Eq, Generic, FromJSON, ToJSON)

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
    trapDamage :: String,
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
    weaponDamage :: (String, DamageType),
    properties :: WeaponProperties,
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
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

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