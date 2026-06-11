{-# LANGUAGE InstanceSigs #-}

module DC.Types (
  CheckSuccess,
  Ability(..)
) where

type CheckSuccess = Bool

data Ability = 
  Charisma |
  Intelligence |
  Constitution |
  Strength |
  Dexterity |
  Wisdom
