{-# LANGUAGE InstanceSigs #-}

module DC.Types (
  CheckSuccess,
  Ability(..),
  SimpleMelee(..),
  SimpleRanged(..),
  MartialMelee(..),
  MartialRanged(..),
  Weapon(..),
  WeaponProficiency(..)
) where
import DC.Json (ToJson (toJson), JsonValue (JsonString), IsJson (fromValue))
import Data.List (find)

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
  fromValue :: JsonValue -> Maybe Ability
  fromValue (JsonString "cha") = Just Charisma
  fromValue (JsonString "int") = Just Intelligence
  fromValue (JsonString "con") = Just Constitution
  fromValue (JsonString "str") = Just Strength
  fromValue (JsonString "dex") = Just Dexterity
  fromValue (JsonString "wis") = Just Wisdom
  fromValue _ = Nothing

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
  fromValue :: JsonValue -> Maybe SimpleMelee
  fromValue (JsonString s) = do
    (SimpleMelee w) <- fst <$> find ((==s) . snd) weaponNameTable

    return w
  fromValue _ = Nothing

instance IsJson SimpleRanged where
  fromValue :: JsonValue -> Maybe SimpleRanged
  fromValue (JsonString s) = do
    (SimpleRanged w) <- fst <$> find ((==s) . snd) weaponNameTable

    return w
  fromValue _ = Nothing

instance IsJson MartialMelee where
  fromValue :: JsonValue -> Maybe MartialMelee
  fromValue (JsonString s) = do
    (MartialMelee w) <- fst <$> find ((==s) . snd) weaponNameTable

    return w
  fromValue _ = Nothing

instance IsJson MartialRanged where
  fromValue :: JsonValue -> Maybe MartialRanged
  fromValue (JsonString s) = do
    (MartialRanged w) <- fst <$> find ((==s) . snd) weaponNameTable

    return w
  fromValue _ = Nothing

instance IsJson Weapon where
  fromValue :: JsonValue -> Maybe Weapon
  fromValue v =
    case (fromValue v :: Maybe SimpleMelee) of
      Just w -> Just (SimpleMelee w)
      Nothing -> case (fromValue v :: Maybe SimpleRanged) of
        Just w -> Just (SimpleRanged w)
        Nothing -> case (fromValue v :: Maybe MartialMelee) of
          Just w -> Just (MartialMelee w)
          Nothing -> MartialRanged <$> (fromValue v :: Maybe MartialRanged)

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
  fromValue :: JsonValue -> Maybe WeaponProficiency
  fromValue (JsonString "simple") = Just Simple
  fromValue (JsonString "martial") = Just Martial
  fromValue v = Specific <$> fromValue v