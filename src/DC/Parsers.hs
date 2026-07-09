module DC.Parsers (
  weaponProperty,
  runParser,
  costItem,
  costString,
  weightString,
  armorTypeString,
  diceRollString,
  diceDropString,
  diceKeepString,
  diceRerollString,
  diceExplosionString,
  diceExpressionString,
  abilityString,
  weaponString
) where

import DC.Parse
import DC.Types
import Control.Applicative ( Alternative((<|>), many), optional )
import Data.Foldable (Foldable(foldl'))
import DC.Error (newBaseError, ErrorDetail (ParseError))
import Data.Maybe (fromMaybe)

weaponProperty :: Parser WeaponProperty
weaponProperty = Ammunition <$ string "ammunition"
  <|> (Finesse <$ string "finesse")
  <|> (Heavy <$ string "heavy")
  <|> (Light <$ string "light")
  <|> (Loading <$ string "loading")
  <|> ((\_ _ _ n _ _ l _ _ -> Range (n, l))
    <$> string "range" 
    <*> space 
    <*> char '('
    <*> number
    <*> char ','
    <*> optional space
    <*> number
    <*> char ')'
    <*> optional space)
  <|> (Reach <$ string "reach")
  <|> (Special <$ string "special")
  <|> (Thrown <$ string "thrown")
  <|> (TwoHanded <$ string "two-handed")
  <|> ((\_ _ _ d _ _ -> Versatile d)
    <$> string "versatile"
    <*> space
    <*> char '('
    <*> number
    <*> char ')'
    <*> optional space)

costItem :: Parser (Int, String)
costItem = (\n _ t -> (n, t))
  <$> number
  <*> optional space
  <*> (string "pp"
    <|> string "gp"
    <|> string "ep"
    <|> string "sp"
    <|> string "cp")

costString :: Parser Cost
costString = foldl' (\acc@Cost { pp = opp, gp = ogp, ep = oep, sp = osp, cp = ocp } (n, t) -> case t of
  "pp" -> acc { pp = opp + n }
  "gp" -> acc { gp = ogp + n }
  "ep" -> acc { ep = oep + n }
  "sp" -> acc { sp = osp + n }
  "cp" -> acc { cp = ocp + n }
  _ -> acc) Cost { pp = 0, gp = 0, ep = 0, sp = 0, cp  = 0 }
  <$> many costItem

weightString :: Parser Weight
weightString = do
  n <- number
  _ <- optional space
  slash <- optional $ char '/'
  _ <- optional space
  d <- optional number
  _ <- optional space
  _ <- string "lb."

  case (slash, d) of
    (Just '/', Just d') -> return $ Fraction (n, d')
    (Nothing, Nothing) -> return $ Whole n
    _ -> Parser $ const $ Left $ newBaseError $ ParseError "Malformed weight string" 

armorTypeString :: Parser ArmorType
armorTypeString = (LightArmor Padded <$ caseInsensitiveString "padded")
  <|> (LightArmor Leather <$ caseInsensitiveString "leather")
  <|> (LightArmor StuddedLeather <$ caseInsensitiveString "studded leather")
  <|> (MediumArmor Hide <$ caseInsensitiveString "hide")
  <|> (MediumArmor ChainShirt <$ caseInsensitiveString "chain shirt")
  <|> (MediumArmor ScaleMail <$ caseInsensitiveString "scale mail")
  <|> (MediumArmor HalfPlate <$ caseInsensitiveString "half plate")
  <|> (HeavyArmor RingMail <$ caseInsensitiveString "ring mail")
  <|> (HeavyArmor ChainMail <$ caseInsensitiveString "chain mail")
  <|> (HeavyArmor Splint <$ caseInsensitiveString "splint")
  <|> (HeavyArmor Plate <$ caseInsensitiveString "plate")
  <|> (Shield <$ caseInsensitiveString "shield")

diceRollString :: Parser DiceRoll
diceRollString = (\n _ t -> DiceRoll { numDice = n, diceType = t })
  <$> number
  <*> caseInsensitiveChar 'd'
  <*> number

diceKeepString :: Parser DiceKeep
diceKeepString = (KeepHighest <$> (string "kh" *> number))
  <|> (KeepLowest <$> (string "kl" *> number))

diceDropString :: Parser DiceDrop
diceDropString = (DropHighest <$> (string "dh" *> number))
 <|> (DropLowest <$> (string "dl" *> number))

diceRerollString :: Parser DiceReroll
diceRerollString = DiceReroll
 <$> (char 'r' *> number)

diceExplosionString :: Parser DiceExplosion
diceExplosionString = (True <$ (char '!' <|> char 'e')) 
  <|> pure False

diceExpressionString :: Parser DiceExpression
diceExpressionString = (,,,,)
 <$> diceRollString
 <*> optional diceKeepString
 <*> many diceDropString
 <*> many diceRerollString
 <*> diceExplosionString

abilityString :: Parser Ability
abilityString = (Charisma <$ (caseInsensitiveString "charisma" <|> caseInsensitiveString "cha"))
  <|> (Intelligence <$ (caseInsensitiveString "intelligence" <|> caseInsensitiveString "int"))
  <|> (Constitution <$ (caseInsensitiveString "constitution" <|> caseInsensitiveString "con"))
  <|> (Strength <$ (caseInsensitiveString "strength" <|> caseInsensitiveString "str"))
  <|> (Dexterity <$ (caseInsensitiveString "dexterity" <|> caseInsensitiveString "dex"))
  <|> (Wisdom <$ (caseInsensitiveString "wisdom" <|> caseInsensitiveString "wis"))

weaponString :: Parser Weapon
weaponString = (SimpleMelee Club <$ caseInsensitiveString "club")
  <|> (SimpleMelee Dagger <$ caseInsensitiveString "dagger")
  <|> (SimpleMelee Greatclub 
    <$ (caseInsensitiveString "greatclub"
      <|> caseInsensitiveString "great club"
      <|> caseInsensitiveString "club, great"))
  <|> (SimpleMelee Handaxe 
    <$ (caseInsensitiveString "handaxe"
      <|> caseInsensitiveString "hand axe"
      <|> caseInsensitiveString "axe, hand"))
  <|> (SimpleMelee Javelin <$ caseInsensitiveString "javelin")
  <|> (SimpleMelee LightHammer 
    <$ (caseInsensitiveString "light hammer"
      <|> caseInsensitiveString "lighthammer"
      <|> caseInsensitiveString "hammer, light"))
  <|> (SimpleMelee Mace <$ caseInsensitiveString "mace")
  <|> (SimpleMelee Quarterstaff 
    <$ (caseInsensitiveString "quarterstaff"
      <|> caseInsensitiveString "quarter staff"
      <|> caseInsensitiveString "staff, quarter"))
  <|> (SimpleMelee Sickle <$ caseInsensitiveString "sickle")
  <|> (SimpleMelee Spear <$ caseInsensitiveString "spear")
  <|> (SimpleRanged LightCrossbow 
    <$ (caseInsensitiveString "crossbow, light" 
      <|> caseInsensitiveString "light crossbow"
      <|> caseInsensitiveString "lightcrossbow"))
  <|> (SimpleRanged Dart <$ caseInsensitiveString "dart")
  <|> (SimpleRanged Sling <$ caseInsensitiveString "sling")
  <|> (MartialMelee Battleaxe 
    <$ (caseInsensitiveString "battleaxe"
      <|> caseInsensitiveString "battle axe"
      <|> caseInsensitiveString "axe, battle"))
  <|> (MartialMelee Flail <$ caseInsensitiveString "flail")
  <|> (MartialMelee Glaive <$ caseInsensitiveString "glaive")
  <|> (MartialMelee Greataxe
    <$ (caseInsensitiveString "greataxe"
      <|> caseInsensitiveString "great axe"
      <|> caseInsensitiveString "axe, great"))
  <|> (MartialMelee Greatsword
    <$ (caseInsensitiveString "greatsword"
      <|> caseInsensitiveString "great sword"
      <|> caseInsensitiveString "sword, great"))
  <|> (MartialMelee Halberd <$ caseInsensitiveString "halberd")
  <|> (MartialMelee Lance <$ caseInsensitiveString "lance")
  <|> (MartialMelee Longsword
    <$ (caseInsensitiveString "longsword"
      <|> caseInsensitiveString "long sword"
      <|> caseInsensitiveString "sword, long"))
  <|> (MartialMelee Maul <$ caseInsensitiveString "maul")
  <|> (MartialMelee Morningstar <$ caseInsensitiveString "morningstar")
  <|> (MartialMelee Pike <$ caseInsensitiveString "pike")
  <|> (MartialMelee Rapier <$ caseInsensitiveString "rapier")
  <|> (MartialMelee Scimitar <$ caseInsensitiveString "scimitar")
  <|> (MartialMelee Shortsword 
    <$ (caseInsensitiveString "shortsword"
      <|> caseInsensitiveString "short sword"
      <|> caseInsensitiveString "sword, short"))
  <|> (MartialMelee Trident <$ caseInsensitiveString "trident")
  <|> (MartialMelee WarPick
    <$ (caseInsensitiveString "warpick"
      <|> caseInsensitiveString "war pick"
      <|> caseInsensitiveString "pick, war"))
  <|> (MartialMelee Warhammer
    <$ (caseInsensitiveString "warhammer"
      <|> caseInsensitiveString "war hammer"
      <|> caseInsensitiveString "hammer, war"))
  <|> (MartialMelee Whip <$ caseInsensitiveString "whip")
  <|> (MartialRanged Blowgun <$ caseInsensitiveString "blowgun")
  <|> (MartialRanged HandCrossbow
    <$ (caseInsensitiveString "handcrossbow"
      <|> caseInsensitiveString "hand crossbow"
      <|> caseInsensitiveString "crossbow, hand"))
  <|> (MartialRanged HeavyCrossbow
    <$ (caseInsensitiveString "heavycrossbow"
      <|> caseInsensitiveString "heavy crossbow"
      <|> caseInsensitiveString "crossbow, heavy"))
  <|> (MartialRanged Longbow
    <$ (caseInsensitiveString "longbow"
      <|> caseInsensitiveString "long bow"
      <|> caseInsensitiveString "bow, long"))
  <|> (MartialRanged Net <$ caseInsensitiveString "net")