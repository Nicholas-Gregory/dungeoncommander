{-# LANGUAGE NamedFieldPuns #-}

module DC.Actions (
  getAbilityScore,
  getAbilityModifier,
  abilityCheck
) where
import DC.Entity (Entity(..))
import DC.Types (Ability(..), CheckSuccess)
import DC.Dice (rollDice)
import System.Random (StdGen)

getAbilityScore :: Entity -> Ability -> Maybe Int
getAbilityScore (Actor { cha }) Charisma = Just cha
getAbilityScore (Actor { int }) Intelligence = Just int
getAbilityScore (Actor { con }) Constitution = Just con
getAbilityScore (Actor { str }) Strength = Just str
getAbilityScore (Actor { dex }) Dexterity = Just dex
getAbilityScore (Actor { wis }) Wisdom = Just wis
getAbilityScore _ _ = Nothing

getAbilityModifier :: Entity -> Ability -> Maybe Int
getAbilityModifier e a = (\s -> (s - 10) `div` 2) <$> getAbilityScore e a

abilityCheck :: StdGen -> Entity -> Ability -> Int -> Maybe CheckSuccess
abilityCheck gen entity ability dc = (\m -> (m + rollDice gen 1 20) >= dc) 
  <$> getAbilityModifier entity ability