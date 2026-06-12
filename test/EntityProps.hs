{-# LANGUAGE ScopedTypeVariables #-}
module EntityProps (props) where

import DC.Json
import Test.QuickCheck 
import DC.Types (Ability(..))
import DC.Entity 

instance Arbitrary EntityChildType where
  arbitrary = elements [ ActorLocation
                        , ObjectLocation
                        , CarriedItem
                        , ContainedItem
                        , HeldItem
                        , Condition
                        , KnownSpell
                        , PreparedSpell
                        , DonnedArmor
                        , WieldedWeapon
                        ]

instance Arbitrary EntityChild where
  arbitrary = EntityChild <$> arbitrary <*> arbitrary

instance Arbitrary EntityChildren where
  arbitrary = EntityChildren <$> listOf arbitrary

instance Arbitrary EntityInfo where
  arbitrary = EntityInfo <$> arbitrary <*> arbitrary

instance Arbitrary ItemInfo where
  arbitrary = ItemInfo <$> arbitrary <*> arbitrary

instance Arbitrary Ability where
  arbitrary = elements [ Charisma
                        ,Intelligence
                        ,Constitution
                        ,Strength
                        ,Dexterity
                        ,Wisdom
                       ]

instance Arbitrary SaveProficiencies where
  arbitrary = SaveProficiencies <$> listOf arbitrary

instance Arbitrary Entity where
  arbitrary = do
    let tupInt = (,) <$> arbitrary <*> arbitrary
        tupStr = (,) <$> arbitrary <*> arbitrary
    oneof
      [ Scene <$> arbitrary <*> tupInt
      , Actor <$> arbitrary <*> tupInt <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , Object <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> tupInt
      , Trap <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> tupStr <*> tupInt
      , Item <$> arbitrary <*> arbitrary
      , Armor <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , Weapon <$> arbitrary <*> arbitrary <*> tupStr <*> listOf arbitrary
      , Container <$> arbitrary <*> arbitrary <*> arbitrary
      , Mount <$> arbitrary <*> arbitrary <*> arbitrary
      , Spell <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , Money <$> arbitrary <*> arbitrary
      ]

-- contract: toJson . fromJson == id for JsonValue values that represent Entities
prop_jsonRoundtrip_entity :: Entity -> Bool
prop_jsonRoundtrip_entity e = case fromJson (toJson e) :: Maybe Entity of
  Just e' -> e' == e
  Nothing -> False

-- since fromJson expects a JsonValue input, check that serializing and parsing give back the same JsonValue for entities
prop_fromTo_fromJson_toJson :: Entity -> Bool
prop_fromTo_fromJson_toJson e = case fromJson (toJson e) :: Maybe Entity of
  Just e' -> toJson e' == toJson e
  Nothing -> False

props :: [Property]
props = [ property prop_jsonRoundtrip_entity
        , property prop_fromTo_fromJson_toJson
        ]
