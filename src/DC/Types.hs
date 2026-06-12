{-# LANGUAGE InstanceSigs #-}

module DC.Types (
  CheckSuccess,
  Ability(..)
) where
import DC.Json (ToJson (toJson), JsonValue (JsonString), IsJson (fromValue))

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