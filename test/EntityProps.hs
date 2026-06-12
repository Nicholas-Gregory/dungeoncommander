{-# LANGUAGE ScopedTypeVariables #-}
module EntityProps (props) where

import DC.Json
import Test.QuickCheck 
import qualified DC.Types as T
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

instance Arbitrary T.Ability where
  arbitrary = elements [ T.Charisma
                        ,T.Intelligence
                        ,T.Constitution
                        ,T.Strength
                        ,T.Dexterity
                        ,T.Wisdom
                       ]

instance Arbitrary SaveProficiencies where
  arbitrary = SaveProficiencies <$> listOf arbitrary

-- additional Arbitrary instances for weapon types and proficiencies
instance Arbitrary T.SimpleMelee where
  arbitrary = elements [ T.Club, T.Dagger, T.Greatclub, T.Handaxe, T.Javelin, T.LightHammer, T.Mace, T.Quarterstaff, T.Sickle, T.Spear ]

instance Arbitrary T.SimpleRanged where
  arbitrary = elements [ T.LightCrossbow, T.Dart, T.Shortbow, T.Sling ]

instance Arbitrary T.MartialMelee where
  arbitrary = elements [ T.Battleaxe, T.Flail, T.Glaive, T.Greataxe, T.Greatsword, T.Halberd, T.Lance, T.Longsword, T.Maul, T.Morningstar, T.Pike, T.Rapier, T.Scimitar, T.Shortsword, T.Trident, T.WarPick, T.Warhammer, T.Whip ]

instance Arbitrary T.MartialRanged where
  arbitrary = elements [ T.Blowgun, T.HandCrossbow, T.HeavyCrossbow, T.Longbow, T.Net ]

instance Arbitrary T.Weapon where
  arbitrary = oneof [ T.SimpleMelee <$> arbitrary, T.SimpleRanged <$> arbitrary, T.MartialMelee <$> arbitrary, T.MartialRanged <$> arbitrary ]

instance Arbitrary T.WeaponProficiency where
  arbitrary = oneof [ return T.Simple, return T.Martial, T.Weapon <$> arbitrary ]

instance Arbitrary WeaponProficiencies where
  arbitrary = WeaponProficiencies <$> listOf arbitrary

instance Arbitrary Entity where
  arbitrary = do
    let tupInt = (,) <$> arbitrary <*> arbitrary
        tupStr = (,) <$> arbitrary <*> arbitrary
    oneof
      [ Scene <$> arbitrary <*> tupInt
      , Actor <$> arbitrary <*> tupInt <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
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
