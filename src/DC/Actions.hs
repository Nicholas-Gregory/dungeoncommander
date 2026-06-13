{-# LANGUAGE NamedFieldPuns #-}

module DC.Actions (
  getAbilityScore,
  getAbilityModifier,
  abilityCheck,
  hasSaveProficiency,
  getProficiencyBonus,
  savingThrow,
  hasWeaponProficiency,
  attackRoll
) where
import DC.Entity (Entity(..), SaveProficiencies(..), WeaponProficiencies (WeaponProficiencies), EntityInfo (children))
import qualified DC.Types as T (Ability(..), CheckSuccess, WeaponProficiency (Simple, Martial, Specific), Weapon (SimpleMelee, SimpleRanged, MartialMelee, MartialRanged))
import DC.Dice (rollDice)
import System.Random (StdGen)
import GHC.Base (undefined)
import DC.Types (WeaponProficiency(Specific))

getAbilityScore :: Entity -> T.Ability -> Maybe Int
getAbilityScore (Actor { cha }) T.Charisma = Just cha
getAbilityScore (Actor { int }) T.Intelligence = Just int
getAbilityScore (Actor { con }) T.Constitution = Just con
getAbilityScore (Actor { str }) T.Strength = Just str
getAbilityScore (Actor { dex }) T.Dexterity = Just dex
getAbilityScore (Actor { wis }) T.Wisdom = Just wis
getAbilityScore _ _ = Nothing

getAbilityModifier :: Entity -> T.Ability -> Maybe Int
getAbilityModifier e a = (\s -> (s - 10) `div` 2) <$> getAbilityScore e a

abilityCheck :: StdGen -> Entity -> T.Ability -> Int -> Maybe T.CheckSuccess
abilityCheck gen entity ability dc = (\m -> (m + rollDice gen 1 20) >= dc) 
  <$> getAbilityModifier entity ability

hasSaveProficiency :: Entity -> T.Ability -> Maybe Bool
hasSaveProficiency (Actor { saveProficiencies = SaveProficiencies xs }) ability = Just $ ability `elem` xs
hasSaveProficiency _ _ = Nothing

getProficiencyBonus :: Entity -> Maybe Int
getProficiencyBonus (Actor { level }) = Just $ ((level - 1) `div` 4) + 2
getProficiencyBonus _ = Nothing

savingThrow :: StdGen -> Entity -> T.Ability -> Int -> Maybe T.CheckSuccess
savingThrow gen entity ability dc = do
  pro <- hasSaveProficiency entity ability
  abilityModifier <- getAbilityModifier entity ability
  proficiencyBonus <- getProficiencyBonus entity
  let roll = rollDice gen 1 20

  return $ if pro 
    then roll + abilityModifier + proficiencyBonus >= dc
    else roll + abilityModifier >= dc

hasWeaponProficiency :: Entity -> T.WeaponProficiency -> Maybe Bool
hasWeaponProficiency (Actor { weaponProficiencies = WeaponProficiencies xs }) proficiency = Just $ proficiency `elem` xs
hasWeaponProficiency _ _ = Nothing

attackRoll :: StdGen -> Entity -> Entity -> T.Ability -> Int -> Maybe T.CheckSuccess
attackRoll gen (Weapon { weapon }) entity ability dc = do
  abilityModifier <- getAbilityModifier entity ability
  proficiencyBonus <- getProficiencyBonus entity
  hasProficiency <- (case weapon of
        T.SimpleMelee w -> liftA2 (||) (hasWeaponProficiency entity T.Simple) (hasWeaponProficiency entity (T.Specific $ T.SimpleMelee w))
        T.SimpleRanged w -> liftA2 (||) (hasWeaponProficiency entity T.Simple) (hasWeaponProficiency entity (T.Specific $ T.SimpleRanged w))
        T.MartialMelee w -> liftA2 (||) (hasWeaponProficiency entity T.Martial) (hasWeaponProficiency entity (T.Specific $ T.MartialMelee w))
        T.MartialRanged w -> liftA2 (||) (hasWeaponProficiency entity T.Martial) (hasWeaponProficiency entity (T.Specific $ T.MartialRanged w)))
  let roll = rollDice gen 1 20
  
  return $ case roll of
    1 -> False
    20 -> True
    _ -> if hasProficiency
    then roll + abilityModifier + proficiencyBonus >= dc
    else roll + abilityModifier >= dc
attackRoll _ _ _ _ _ = Nothing
  