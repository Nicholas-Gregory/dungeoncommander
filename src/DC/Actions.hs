{-# LANGUAGE NamedFieldPuns #-}

module DC.Actions (
  getAbilityScore,
  getAbilityModifier,
  abilityCheck,
  hasSaveProficiency,
  getProficiencyBonus,
  savingThrow
) where
import DC.Entity (Entity(..), SaveProficiencies(..))
import DC.Types (Ability(..), CheckSuccess)
import DC.Dice (rollDice)
import System.Random (StdGen)
import GHC.Base (undefined)

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

hasSaveProficiency :: Entity -> Ability -> Maybe Bool
hasSaveProficiency (Actor { saveProficiencies = SaveProficiencies xs }) ability = Just $ ability `elem` xs
hasSaveProficiency _ _ = Nothing

getProficiencyBonus :: Entity -> Maybe Int
getProficiencyBonus (Actor { level }) = Just $ ((level - 1) `div` 4) + 2
getProficiencyBonus _ = Nothing

savingThrow :: StdGen -> Entity -> Ability -> Int -> Maybe CheckSuccess
savingThrow gen entity ability dc = do
  pro <- hasSaveProficiency entity ability
  abilityModifier <- getAbilityModifier entity ability
  proficiencyBonus <- getProficiencyBonus entity
  let roll = rollDice gen 1 20

  return $ if pro 
    then roll + abilityModifier + proficiencyBonus >= dc
    else roll + abilityModifier >= dc

attackRoll :: StdGen -> Entity -> Ability -> Int -> Maybe CheckSuccess
attackRoll gen entity ability dc = undefined