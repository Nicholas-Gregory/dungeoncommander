module DC.Opts (
 rootInfo,
 Command(..),
 SceneAction(..),
 CreateScene(..),
 RollOptions(..),
 RootOptions(..)
) where

import Options.Applicative
import DC.Types (Ability)

data Command
  = RollCommand RollOptions
  | SceneCommand SceneAction
  | ActorCommand ActorAction
  | ObjectCommand ObjectAction
  | TrapCommand TrapAction
  | ItemCommand ItemAction
  | ArmorCommand ArmorAction
  | WeaponCommand WeaponAction
  | ContainerCommand ContainerAction
  | MountCommand MountAction
  | SpellCommand SpellAction
  | MoneyCommand MoneyAction
  | ChaCommand AbilityCheck
  | IntCommand AbilityCheck
  | ConCommand AbilityCheck
  | StrCommand AbilityCheck
  | DexCommand AbilityCheck
  | WisCommand AbilityCheck
  deriving (Show, Eq)

data SceneAction
  = SceneCreate CreateScene
  | SceneDelete DeleteScene
  | SceneUpdate UpdateScene
  | SceneAddActor AddActorScene
  | SceneAddObject AddObjectScene
  | SceneRemoveActor RemoveActorScene
  | SceneRemoveObject
  | SceneAddTo
  | SceneRemoveFrom
  | SceneSelect SelectScene
  deriving (Show, Eq)

data ActorAction
  = ActorCreate
  | ActorDelete
  | ActorUpdate UpdateActor
  | ActorAdd
  | ActorRemove
  | ActorAddTo
  | ActorRemoveFrom
  deriving (Show, Eq)

rootInfo :: ParserInfo RootOptions
rootInfo = info (helper <*> rootParser) (progDesc "Manage game state for a Dungeons and Dragons 5e session using various commands")

data RootOptions = RootOptions
  { save :: Bool
  , rootCommand :: Maybe Command
  }

rootParser :: Parser RootOptions
rootParser = RootOptions
  <$> switch
    (long "save"
    <> short 's'
    <> help "Use this flag to commit the result to disk")
  <*> optional (hsubparser
    ( command "scene" (info (helper <*> (SceneCommand <$> sceneAction)) 
      (progDesc "Select a Scene, or manage Scenes"))
    <> command "actor" (info (helper <*> (ActorCommand <$> actorAction))
      (progDesc "Select an Actor, or manage Actors"))
    <> command "object" (info (helper <*> (ObjectCommand <$> objectAction))
      (progDesc "Select an Object, or manage Objects"))
    <> command "trap" (info (helper <*> (TrapCommand <$> trapAction))
      (progDesc "Select a Trap, or manage Traps"))
    <> command "item" (info (helper <*> (ItemCommand <$> itemAction))
      (progDesc "Select an Item, or manage Items"))
    <> command "armor" (info (helper <*> (ArmorCommand <$> armorAction))
      (progDesc "Select Armor, or manage Armor"))
    <> command "weapon" (info (helper <*> (WeaponCommand <$> weaponAction))
      (progDesc "Select a Weapon, or manage Weapons"))
    <> command "container" (info (helper <*> (ContainerCommand <$> containerAction))
      (progDesc "Select a Container, or manage Containers"))
    <> command "mount" (info (helper <*> (MountCommand <$> mountAction))
      (progDesc "Select a Mount, or manage Mounts"))
    <> command "spell" (info (helper <*> (SpellCommand <$> spellAction))
      (progDesc "Select a Spell, or manage Spells"))
    <> command "money" (info (helper <*> (MoneyCommand <$> moneyAction))
      (progDesc "Select Money, or manage Money"))
    <> command "cha" (info (helper <*> (ChaCommand <$> abilityCheck))
      (progDesc "Have an Actor perform a Charisma Check"))
    <> command "int" (info (helper <*> (IntCommand <$> abilityCheck))
      (progDesc "Have an Actor perform an Intelligence Check"))
    <> command "con" (info (helper <*> (ConCommand <$> abilityCheck))
      (progDesc "Have an Actor perform a Constitution Check"))
    <> command "str" (info (helper <*> (StrCommand <$> abilityCheck))
      (progDesc "Have an Actor perform a Strength Check"))
    <> command "dex" (info (helper <*> (DexCommand <$> abilityCheck))
      (progDesc "Have an Actor perform a Dexterity Check"))
    <> command "wis" (info (helper <*> (WisCommand <$> abilityCheck))
      (progDesc "Have an Actor perform a Wisdom Check"))
    <> command "roll" (info (helper <*> (RollCommand <$> rollCommand))
      (progDesc "Perform the proceeding action chain, with computed rolls or provided rolls. Can also use to perform one-off dice rolls"))
    ))

data RollOptions = RollOptions
  { rollExpression :: Maybe String
  , rollDiceNumber :: Maybe Int
  , rollDiceType :: Maybe Int
  , rollAttack :: Maybe Int
  , rollDamage :: Maybe Int
  , advantage :: Bool
  , disadvantage :: Bool
  } deriving (Show, Eq)

rollCommand :: Parser RollOptions
rollCommand = RollOptions
  <$> optional (strOption
    (long "expression"
    <> short 'e'
    <> metavar "DICE"
    <> help "Input an entire standard TTRPG dice expression string to have the engine roll the specified dice and display the result"))
  <*> optional (option auto
    (long "number"
    <> short 'n'
    <> metavar "INTEGER"
    <> help "Specify the number of dice to roll"))
  <*> optional (option auto
    (long "type"
    <> short 't'
    <> metavar "INTEGER"
    <> help "Specify the type of dice to roll"))
  <*> optional (option auto 
    (long "attack"
    <> short 'a'
    <> metavar "INTEGER"
    <> help "Specify the result of an Attack Roll. The engine computes the preceeding chain with this result."))
  <*> optional (option auto
    (long "damage"
    <> short 'd'
    <> metavar "INTEGER"
    <> help "Specify the result of a Damage Roll. The engine computes the preceeding chain with this result."))
  <*> switch
    (long "advantage"
    <> help "Use this flag to compute the roll with advantage")
  <*> switch
    (long "disadvantage"
    <> help "Use this flag to compute the roll with disadvantage")

data AbilityCheck = AbilityCheck
  { checkDc :: Int,
    contested :: Maybe String,
    checkActor :: String,
    checkSave :: Bool
  } deriving (Show, Eq)

abilityCheck :: Parser AbilityCheck
abilityCheck = AbilityCheck
  <$> option auto
    ( long "dc"
    <> metavar "INTEGER"
    <> help "The difficulty class of the ability check")
  <*> optional (strOption
    (long "contested"
    <> metavar "ACTOR"
    <> help "The ID of the Actor the contested check is with"))
  <*> strOption
    (long "actor"
    <> short 'a'
    <> metavar "ACTOR"
    <> help "The Actor performing the ability check")
  <*> switch
    (long "saving-throw"
    <> short 's'
    <> help "Include this switch if the check is a saving throw")

sceneAction :: Parser SceneAction
sceneAction = hsubparser
  ( command "update" (info (helper <*> updateScene) 
    (progDesc "Directly update values for a particular Scene"))
  <> command "create" (info (helper <*> createScene)
    (progDesc "Create an entirely new Scene"))
  <> command "delete" (info (helper <*> deleteScene)
    (progDesc "Delete a Scene entirely"))
  <> command "add-actor" (info (helper <*> addActorScene)
    (progDesc "Add an Actor to a Scene"))
  <> command "add-object" (info (helper <*> addObjectScene)
    (progDesc "Add an Object to a Scene"))
  <> command "remove-actor" (info (helper <*> removeActorScene)
    (progDesc "Remove an Actor from a Scene")))

data SelectScene = SelectScene
  { selectSceneId :: String } deriving (Show, Eq)

selectScene :: Parser SceneAction
selectScene = SceneSelect <$> (SelectScene
  <$> strOption
    ( long "id"
    <> metavar "SCENE-ID"
    <> help "The ID of the Scene to select"))

data RemoveActorScene = RemoveActorScene
  { removeActorSceneId :: Maybe String
  , removeActorActorId :: Maybe String
  } deriving (Show, Eq)

removeActorScene :: Parser SceneAction
removeActorScene = SceneRemoveActor <$> (RemoveActorScene
  <$> optional (strOption
    ( long "scene-id"
    <> metavar "SCENE-ID"
    <> help "The ID of the Scene to remove an Actor from"))
  <*> optional (strOption
    ( long "actor-id"
    <> metavar "ACTOR-ID"
    <> help "The ID of the Actor to remove from the Scene")))

data AddObjectScene = AddObjectScene
  { addObjectSceneId :: Maybe String
  , addObjectObjectId :: Maybe String
  } deriving (Show, Eq)

addObjectScene :: Parser SceneAction
addObjectScene = SceneAddObject <$> (AddObjectScene
  <$> optional (strOption
    ( long "scene-id"
    <> metavar "SCENE-ID"
    <> help "The ID of the Scene to add an Object to"))
  <*> optional (strOption
    ( long "object-id"
    <> metavar "OBJECT-ID"
    <> help "The ID of the Object to add to the Scene")))

data AddActorScene = AddActorScene
  { addActorSceneId :: Maybe String 
  , addActorActorId :: Maybe String
  } deriving (Show, Eq)

addActorScene :: Parser SceneAction
addActorScene = SceneAddActor <$> (AddActorScene
  <$> optional (strOption
    ( long "scene-id"
    <> metavar "SCENE-ID"
    <> help "The ID of the Scene to add an Actor to"))
  <*> optional (strOption
    ( long "actor-id"
    <> metavar "ACTOR-ID"
    <> help "The ID of the Actor to add to the Scene")))

data DeleteScene = DeleteScene
  { deleteSceneId :: Maybe String } deriving (Show, Eq)

deleteScene :: Parser SceneAction
deleteScene = SceneDelete <$> (DeleteScene
  <$> optional (strOption
    ( long "id"
    <> metavar "ID"
    <> help "The ID of the Scene to delete")))

data CreateScene = CreateScene
  { createSceneId :: String
  , createSceneName :: String
  , createSceneX :: Int
  , createSceneY :: Int
  } deriving (Show, Eq)

createScene :: Parser SceneAction
createScene = SceneCreate <$> (CreateScene
  <$> strOption
    ( long "id"
    <> metavar "ID"
    <> help "The ID of the new Scene")
  <*> strOption
    ( long "name"
    <> short 'n'
    <> metavar "NAME"
    <> help "The name of the new Scene")
  <*> option auto
    ( short 'x'
    <> metavar "INTEGER"
    <> help "The X dimension of the new Scene")
  <*> option auto
    ( short 'y'
    <> metavar "INTEGER"
    <> help "The Y dimension of the new Scene"))

data UpdateScene = UpdateScene
  { updateSceneId :: Maybe String
  , updateSceneName :: Maybe String
  , updateSceneX :: Maybe Int
  , updateSceneY :: Maybe Int
  } deriving (Show, Eq)

updateScene :: Parser SceneAction
updateScene = SceneUpdate <$> (UpdateScene
  <$> optional (strOption
    ( long "id"
    <> metavar "ID"
    <> help "The ID of the Scene to update"))
  <*> optional (strOption
    ( long "name"
    <> short 'n'
    <> metavar "NAME"
    <> help "The new name of the Scene"))
  <*> optional (option auto
    ( short 'x'
    <> metavar "INTEGER"
    <> help "The new X dimension of the Scene"))
  <*> optional (option auto
    ( short 'y'
    <> metavar "INTEGER"
    <> help "The new Y dimension of the Scene")))

actorAction :: Parser ActorAction
actorAction = hsubparser
  ( command "update" (info (helper <*> updateActor)
    (progDesc "Directly update values for a particular Actor")))

data UpdateActor = UpdateActor
  { actorName :: Maybe String
  , actorX :: Maybe Int
  , actorY :: Maybe Int
  , actorCurrentHp :: Maybe Int
  , actorMaxHp :: Maybe Int
  , actorCha :: Maybe Int
  , actorInt :: Maybe Int
  , actorCon :: Maybe Int
  , actorStr :: Maybe Int
  , actorDex :: Maybe Int
  , actorWis :: Maybe Int
  , actorHitDice :: Maybe Int
  , actorAc :: Maybe Int
  , actorLevel :: Maybe Int
  } deriving (Show, Eq)

updateActor :: Parser ActorAction
updateActor = ActorUpdate <$> (UpdateActor
  <$> optional (strOption
    ( long "name"
    <> short 'n'
    <> metavar "NAME"
    <> help "The new name of the Actor"))
  <*> optional (option auto
    ( short 'x'
    <> metavar "INTEGER"
    <> help "The new X coordinate of the Actor's position"))
  <*> optional (option auto
    ( short 'y'
    <> metavar "INTEGER"
    <> help "The new Y coordinate of the Actor's position"))
  <*> optional (option auto
    ( long "current-hp"
    <> metavar "INTEGER"
    <> help "The new current HP of the Actor"))
  <*> optional (option auto
    ( long "max-hp"
    <> metavar "INTEGER"
    <> help "The new maximum HP of the Actor"))
  <*> optional (option auto
    ( long "cha"
    <> metavar "INTEGER"
    <> help "The new Charisma ability score of the Actor"))
  <*> optional (option auto
    ( long "int"
    <> metavar "INTEGER"
    <> help "The new Intelligence ability score of the Actor"))
  <*> optional (option auto
    ( long "con"
    <> metavar "INTEGER"
    <> help "The new Constitution ability score of the Actor"))
  <*> optional (option auto
    ( long "str"
    <> metavar "INTEGER"
    <> help "The new Strength ability score of the Actor"))
  <*> optional (option auto
    ( long "dex"
    <> metavar "INTEGER"
    <> help "The new Dexterity ability score of the Actor"))
  <*> optional (option auto
    ( long "wis"
    <> metavar "INTEGER"
    <> help "Then new Wisdom ability score of the Actor"))
  <*> optional (option auto
    ( long "hd"
    <> metavar "DICE"
    <> help "A Dice Expression corresponding to the new Hit Dice of the Actor"))
  <*> optional (option auto
    ( long "ac"
    <> metavar "INTEGER"
    <> help "The new Armor Class of the Actor"))
  <*> optional (option auto
    ( long "level"
    <> short 'l'
    <> metavar "INTEGER"
    <> help "The new Level of the Actor")))

-- Object
data ObjectAction
  = ObjectCreate CreateObject
  | ObjectDelete DeleteObject
  | ObjectUpdate UpdateObject
  deriving (Show, Eq)

data CreateObject = CreateObject
  { createObjectId :: Maybe String
  , createObjectName :: String
  , createObjectAc :: Int
  , createObjectMaxHp :: Int
  , createObjectX :: Int
  , createObjectY :: Int
  } deriving (Show, Eq)

createObject :: Parser ObjectAction
createObject = ObjectCreate <$> (CreateObject
  <$> optional (strOption
    ( long "id"
    <> metavar "ID"
    <> help "The ID of the new Object"))
  <*> strOption
    ( long "name"
    <> short 'n'
    <> metavar "NAME"
    <> help "The name of the new Object")
  <*> option auto
    ( long "ac"
    <> metavar "INTEGER"
    <> help "The Armor Class of the new Object")
  <*> option auto
    ( long "max-hp"
    <> metavar "INTEGER"
    <> help "The maximum HP of the new Object")
  <*> option auto
    ( short 'x'
    <> metavar "INTEGER"
    <> help "The X coordinate of the new Object")
  <*> option auto
    ( short 'y'
    <> metavar "INTEGER"
    <> help "The Y coordinate of the new Object"))

data DeleteObject = DeleteObject { deleteObjectId :: Maybe String } deriving (Show, Eq)

deleteObject :: Parser ObjectAction
deleteObject = ObjectDelete <$> (DeleteObject
  <$> optional (strOption
    ( long "id"
    <> metavar "ID"
    <> help "The ID of the Object to delete")))

data UpdateObject = UpdateObject
  { updateObjectId :: Maybe String
  , updateObjectName :: Maybe String
  , updateObjectAc :: Maybe Int
  , updateObjectMaxHp :: Maybe Int
  , updateObjectCurrentHp :: Maybe Int
  , updateObjectX :: Maybe Int
  , updateObjectY :: Maybe Int
  } deriving (Show, Eq)

updateObject :: Parser ObjectAction
updateObject = ObjectUpdate <$> (UpdateObject
  <$> optional (strOption
    ( long "id"
    <> metavar "ID"
    <> help "The ID of the Object to update"))
  <*> optional (strOption
    ( long "name"
    <> short 'n'
    <> metavar "NAME"
    <> help "The new name of the Object"))
  <*> optional (option auto
    ( long "ac"
    <> metavar "INTEGER"
    <> help "The new Armor Class of the Object"))
  <*> optional (option auto
    ( long "max-hp"
    <> metavar "INTEGER"
    <> help "The new maximum HP of the Object"))
  <*> optional (option auto
    ( long "current-hp"
    <> metavar "INTEGER"
    <> help "The new current HP of the Object"))
  <*> optional (option auto
    ( short 'x'
    <> metavar "INTEGER"
    <> help "The new X coordinate of the Object"))
  <*> optional (option auto
    ( short 'y'
    <> metavar "INTEGER"
    <> help "The new Y coordinate of the Object")))

objectAction :: Parser ObjectAction
objectAction = hsubparser
  ( command "create" (info (helper <*> createObject)
    (progDesc "Create an Object"))
  <> command "delete" (info (helper <*> deleteObject)
    (progDesc "Delete an Object"))
  <> command "update" (info (helper <*> updateObject)
    (progDesc "Update an Object")))

-- Trap
data TrapAction
  = TrapCreate CreateTrap
  | TrapDelete DeleteTrap
  | TrapUpdate UpdateTrap
  deriving (Show, Eq)

data CreateTrap = CreateTrap
  { createTrapId :: Maybe String
  , createTrapName :: String
  , createTrapDetectDc :: Int
  , createTrapAttackBonus :: Int
  , createTrapSaveDc :: Int
  , createTrapDamage :: String
  , createTrapX :: Int
  , createTrapY :: Int
  } deriving (Show, Eq)

createTrap :: Parser TrapAction
createTrap = TrapCreate <$> (CreateTrap
  <$> optional (strOption
    ( long "id" <> metavar "ID" <> help "ID of the new Trap"))
  <*> strOption
    ( long "name" <> short 'n' <> metavar "NAME" <> help "Name of the new Trap")
  <*> option auto
    ( long "detect-dc" <> metavar "INTEGER" <> help "Detect DC for the Trap")
  <*> option auto
    ( long "attack-bonus" <> metavar "INTEGER" <> help "Attack bonus for the Trap")
  <*> option auto
    ( long "save-dc" <> metavar "INTEGER" <> help "Save DC for the Trap")
  <*> strOption
    ( long "damage" <> metavar "DICE" <> help "Damage expression for the Trap")
  <*> option auto
    ( short 'x' <> metavar "INTEGER" <> help "X coordinate of the Trap")
  <*> option auto
    ( short 'y' <> metavar "INTEGER" <> help "Y coordinate of the Trap")

  )

data DeleteTrap = DeleteTrap { deleteTrapId :: Maybe String } deriving (Show, Eq)

deleteTrap :: Parser TrapAction
deleteTrap = TrapDelete <$> (DeleteTrap
  <$> optional (strOption
    ( long "id" <> metavar "ID" <> help "ID of the Trap to delete")))

data UpdateTrap = UpdateTrap
  { updateTrapId :: Maybe String
  , updateTrapName :: Maybe String
  , updateTrapDetectDc :: Maybe Int
  , updateTrapAttackBonus :: Maybe Int
  , updateTrapSaveDc :: Maybe Int
  , updateTrapDamage :: Maybe String
  , updateTrapX :: Maybe Int
  , updateTrapY :: Maybe Int
  } deriving (Show, Eq)
updateTrap :: Parser TrapAction
updateTrap = TrapUpdate <$> (UpdateTrap
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of Trap to update"))
  <*> optional (strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "New name"))
  <*> optional (option auto ( long "detect-dc" <> metavar "INTEGER" <> help "New Detect DC"))
  <*> optional (option auto ( long "attack-bonus" <> metavar "INTEGER" <> help "New attack bonus"))
  <*> optional (option auto ( long "save-dc" <> metavar "INTEGER" <> help "New save DC"))
  <*> optional (strOption ( long "damage" <> metavar "DICE" <> help "New damage expression"))
  <*> optional (option auto ( short 'x' <> metavar "INTEGER" <> help "New X coordinate"))
  <*> optional (option auto ( short 'y' <> metavar "INTEGER" <> help "New Y coordinate")))

trapAction :: Parser TrapAction
trapAction = hsubparser
  ( command "create" (info (helper <*> createTrap) (progDesc "Create a Trap"))
  <> command "delete" (info (helper <*> deleteTrap) (progDesc "Delete a Trap"))
  <> command "update" (info (helper <*> updateTrap) (progDesc "Update a Trap")))

-- Item
data ItemAction
  = ItemCreate CreateItem
  | ItemDelete DeleteItem
  | ItemUpdate UpdateItem
  deriving (Show, Eq)

data CreateItem = CreateItem
  { createItemId :: Maybe String
  , createItemName :: String
  , createItemCost :: String
  , createItemWeight :: String
  } deriving (Show, Eq)

createItem :: Parser ItemAction
createItem = ItemCreate <$> (CreateItem
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of new Item"))
  <*> strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "Name of new Item")
  <*> strOption ( long "cost" <> metavar "COST" <> help "Cost of new Item")
  <*> strOption ( long "weight" <> metavar "WEIGHT" <> help "Weight of new Item"))

data DeleteItem = DeleteItem { deleteItemId :: Maybe String } deriving (Show, Eq)

deleteItem :: Parser ItemAction
deleteItem = ItemDelete <$> (DeleteItem
  <$> optional (strOption
    ( long "id"
    <> metavar "ID"
    <> help "ID of Item to delete")))

data UpdateItem = UpdateItem
  { updateItemId :: Maybe String
  , updateItemName :: Maybe String
  , updateItemCost :: Maybe String
  , updateItemWeight :: Maybe String
  } deriving (Show, Eq)

updateItem :: Parser ItemAction
updateItem = ItemUpdate <$> (UpdateItem
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of Item to update"))
  <*> optional (strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "New name"))
  <*> optional (strOption ( long "cost" <> metavar "COST" <> help "New cost"))
  <*> optional (strOption ( long "weight" <> metavar "WEIGHT" <> help "New weight")))

itemAction :: Parser ItemAction
itemAction = hsubparser
  ( command "create" (info (helper <*> createItem) (progDesc "Create an Item"))
  <> command "delete" (info (helper <*> deleteItem) (progDesc "Delete an Item"))
  <> command "update" (info (helper <*> updateItem) (progDesc "Update an Item")))

-- Armor
data ArmorAction
  = ArmorCreate CreateArmor
  | ArmorDelete DeleteArmor
  | ArmorUpdate UpdateArmor
  deriving (Show, Eq)

data CreateArmor = CreateArmor
  { createArmorId :: Maybe String
  , createArmorName :: String
  , createArmorCost :: String
  , createArmorWeight :: String
  , createArmorAc :: Int
  , createArmorStr :: Int
  , createArmorStealthDisadvantage :: Bool
  , createArmorType :: String
  } deriving (Show, Eq)

createArmor :: Parser ArmorAction
createArmor = ArmorCreate <$> (CreateArmor
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of new Armor"))
  <*> strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "Name of new Armor")
  <*> strOption ( long "cost" <> metavar "COST" <> help "Cost")
  <*> strOption ( long "weight" <> metavar "WEIGHT" <> help "Weight")
  <*> option auto ( long "ac" <> metavar "INTEGER" <> help "Armor Class")
  <*> option auto ( long "str" <> metavar "INTEGER" <> help "Strength requirement")
  <*> switch ( long "stealth-disadvantage" <> help "Has stealth disadvantage")
  <*> strOption ( long "type" <> metavar "TYPE" <> help "Armor type"))

data DeleteArmor = DeleteArmor { deleteArmorId :: Maybe String } deriving (Show, Eq)

deleteArmor :: Parser ArmorAction
deleteArmor = ArmorDelete <$> (DeleteArmor
  <$> optional (strOption
    ( long "id"
    <> metavar "ID"
    <> help "ID of Armor to delete")))

data UpdateArmor = UpdateArmor
  { updateArmorId :: Maybe String
  , updateArmorName :: Maybe String
  , updateArmorCost :: Maybe String
  , updateArmorWeight :: Maybe String
  , updateArmorAc :: Maybe Int
  , updateArmorStr :: Maybe Int
  , updateArmorStealthDisadvantage :: Maybe Bool
  , updateArmorType :: Maybe String
  } deriving (Show, Eq)

updateArmor :: Parser ArmorAction
updateArmor = ArmorUpdate <$> (UpdateArmor
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of Armor to update"))
  <*> optional (strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "New name"))
  <*> optional (strOption ( long "cost" <> metavar "COST" <> help "New cost"))
  <*> optional (strOption ( long "weight" <> metavar "WEIGHT" <> help "New weight"))
  <*> optional (option auto ( long "ac" <> metavar "INTEGER" <> help "New AC"))
  <*> optional (option auto ( long "str" <> metavar "INTEGER" <> help "New Strength requirement"))
  <*> optional (option auto ( long "stealth-disadvantage" <> metavar "BOOL" <> help "New stealth disadvantage flag"))
  <*> optional (strOption ( long "type" <> metavar "TYPE" <> help "New armor type")))

armorAction :: Parser ArmorAction
armorAction = hsubparser
  ( command "create" (info (helper <*> createArmor) (progDesc "Create Armor"))
  <> command "delete" (info (helper <*> deleteArmor) (progDesc "Delete Armor"))
  <> command "update" (info (helper <*> updateArmor) (progDesc "Update Armor")))

-- Weapon
data WeaponAction
  = WeaponCreate CreateWeapon
  | WeaponDelete DeleteWeapon
  | WeaponUpdate UpdateWeapon
  deriving (Show, Eq)

data CreateWeapon = CreateWeapon
  { createWeaponId :: Maybe String
  , createWeaponName :: String
  , createWeaponCost :: String
  , createWeaponWeight :: String
  , createWeaponDamage :: String
  , createWeaponProperties :: Maybe String
  , createWeaponWeapon :: Maybe String
  } deriving (Show, Eq)

createWeapon :: Parser WeaponAction
createWeapon = WeaponCreate <$> (CreateWeapon
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of new Weapon"))
  <*> strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "Name of new Weapon")
  <*> strOption ( long "cost" <> metavar "COST" <> help "Cost")
  <*> strOption ( long "weight" <> metavar "WEIGHT" <> help "Weight")
  <*> strOption ( long "damage" <> metavar "DICE" <> help "Damage expression")
  <*> optional (strOption ( long "properties" <> metavar "PROPS" <> help "Properties list (comma separated)"))
  <*> optional (strOption ( long "weapon" <> metavar "WEAPON" <> help "Weapon identifier")))

data DeleteWeapon = DeleteWeapon { deleteWeaponId :: Maybe String } deriving (Show, Eq)

deleteWeapon :: Parser WeaponAction
deleteWeapon = WeaponDelete <$> (DeleteWeapon
  <$> optional (strOption
    ( long "id"
    <> metavar "ID"
    <> help "ID of Weapon to delete")))

data UpdateWeapon = UpdateWeapon
  { updateWeaponId :: Maybe String
  , updateWeaponName :: Maybe String
  , updateWeaponCost :: Maybe String
  , updateWeaponWeight :: Maybe String
  , updateWeaponDamage :: Maybe String
  , updateWeaponProperties :: Maybe String
  , updateWeaponWeapon :: Maybe String
  } deriving (Show, Eq)

updateWeapon :: Parser WeaponAction
updateWeapon = WeaponUpdate <$> (UpdateWeapon
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of Weapon to update"))
  <*> optional (strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "New name"))
  <*> optional (strOption ( long "cost" <> metavar "COST" <> help "New cost"))
  <*> optional (strOption ( long "weight" <> metavar "WEIGHT" <> help "New weight"))
  <*> optional (strOption ( long "damage" <> metavar "DICE" <> help "New damage"))
  <*> optional (strOption ( long "properties" <> metavar "PROPS" <> help "New properties"))
  <*> optional (strOption ( long "weapon" <> metavar "WEAPON" <> help "New weapon identifier")))

weaponAction :: Parser WeaponAction
weaponAction = hsubparser
  ( command "create" (info (helper <*> createWeapon) (progDesc "Create Weapon"))
  <> command "delete" (info (helper <*> deleteWeapon) (progDesc "Delete Weapon"))
  <> command "update" (info (helper <*> updateWeapon) (progDesc "Update Weapon")))

-- Container
data ContainerAction
  = ContainerCreate CreateContainer
  | ContainerDelete DeleteContainer
  | ContainerUpdate UpdateContainer
  deriving (Show, Eq)

data CreateContainer = CreateContainer
  { createContainerId :: Maybe String
  , createContainerName :: String
  , createContainerCost :: String
  , createContainerWeight :: String
  , createContainerCapacity :: String
  } deriving (Show, Eq)

createContainer :: Parser ContainerAction
createContainer = ContainerCreate <$> (CreateContainer
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of new Container"))
  <*> strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "Name of new Container")
  <*> strOption ( long "cost" <> metavar "COST" <> help "Cost")
  <*> strOption ( long "weight" <> metavar "WEIGHT" <> help "Weight")
  <*> strOption ( long "capacity" <> metavar "CAPACITY" <> help "Capacity"))

data DeleteContainer = DeleteContainer { deleteContainerId :: Maybe String } deriving (Show, Eq)

deleteContainer :: Parser ContainerAction
deleteContainer = ContainerDelete <$> (DeleteContainer
  <$> optional (strOption
    ( long "id"
    <> metavar "ID"
    <> help "ID of Container to delete")))

data UpdateContainer = UpdateContainer
  { updateContainerId :: Maybe String
  , updateContainerName :: Maybe String
  , updateContainerCost :: Maybe String
  , updateContainerWeight :: Maybe String
  , updateContainerCapacity :: Maybe String
  } deriving (Show, Eq)

updateContainer :: Parser ContainerAction
updateContainer = ContainerUpdate <$> (UpdateContainer
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of Container to update"))
  <*> optional (strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "New name"))
  <*> optional (strOption ( long "cost" <> metavar "COST" <> help "New cost"))
  <*> optional (strOption ( long "weight" <> metavar "WEIGHT" <> help "New weight"))
  <*> optional (strOption ( long "capacity" <> metavar "CAPACITY" <> help "New capacity")))

containerAction :: Parser ContainerAction
containerAction = hsubparser
  ( command "create" (info (helper <*> createContainer) (progDesc "Create Container"))
  <> command "delete" (info (helper <*> deleteContainer) (progDesc "Delete Container"))
  <> command "update" (info (helper <*> updateContainer) (progDesc "Update Container")))

-- Mount
data MountAction
  = MountCreate CreateMount
  | MountDelete DeleteMount
  | MountUpdate UpdateMount
  deriving (Show, Eq)

data CreateMount = CreateMount
  { createMountId :: Maybe String
  , createMountName :: String
  , createMountSpeed :: Int
  , createMountCarrying :: Int
  } deriving (Show, Eq)

createMount :: Parser MountAction
createMount = MountCreate <$> (CreateMount
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of new Mount"))
  <*> strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "Name of new Mount")
  <*> option auto ( long "speed" <> metavar "INTEGER" <> help "Speed of Mount")
  <*> option auto ( long "carrying" <> metavar "INTEGER" <> help "Carrying capacity"))

data DeleteMount = DeleteMount { deleteMountId :: Maybe String } deriving (Show, Eq)

deleteMount :: Parser MountAction
deleteMount = MountDelete <$> (DeleteMount
  <$> optional (strOption
    ( long "id"
    <> metavar "ID"
    <> help "ID of Mount to delete")))

data UpdateMount = UpdateMount
  { updateMountId :: Maybe String
  , updateMountName :: Maybe String
  , updateMountSpeed :: Maybe Int
  , updateMountCarrying :: Maybe Int
  } deriving (Show, Eq)

updateMount :: Parser MountAction
updateMount = MountUpdate <$> (UpdateMount
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of Mount to update"))
  <*> optional (strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "New name"))
  <*> optional (option auto ( long "speed" <> metavar "INTEGER" <> help "New speed"))
  <*> optional (option auto ( long "carrying" <> metavar "INTEGER" <> help "New carrying capacity")))

mountAction :: Parser MountAction
mountAction = hsubparser
  ( command "create" (info (helper <*> createMount) (progDesc "Create Mount"))
  <> command "delete" (info (helper <*> deleteMount) (progDesc "Delete Mount"))
  <> command "update" (info (helper <*> updateMount) (progDesc "Update Mount")))

-- Spell
data SpellAction
  = SpellCreate CreateSpell
  | SpellDelete DeleteSpell
  | SpellUpdate UpdateSpell
  deriving (Show, Eq)

data CreateSpell = CreateSpell
  { createSpellId :: Maybe String
  , createSpellName :: String
  , createSpellLevel :: Int
  , createSpellRitual :: Bool
  , createSpellAction :: String
  , createSpellRange :: String
  , createSpellComponents :: String
  , createSpellDuration :: String
  , createSpellTargets :: String
  , createSpellAoe :: String
  , createSpellSave :: String
  , createSpellAttack :: String
  } deriving (Show, Eq)

createSpell :: Parser SpellAction
createSpell = SpellCreate <$> (CreateSpell
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of new Spell"))
  <*> strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "Name of new Spell")
  <*> option auto ( long "level" <> metavar "INTEGER" <> help "Spell level")
  <*> switch ( long "ritual" <> help "Is a ritual")
  <*> strOption ( long "action" <> metavar "ACTION" <> help "Action type")
  <*> strOption ( long "range" <> metavar "RANGE" <> help "Range")
  <*> strOption ( long "components" <> metavar "COMPONENTS" <> help "Components")
  <*> strOption ( long "duration" <> metavar "DURATION" <> help "Duration")
  <*> strOption ( long "targets" <> metavar "TARGETS" <> help "Targets")
  <*> strOption ( long "aoe" <> metavar "AOE" <> help "Area of effect")
  <*> strOption ( long "save" <> metavar "SAVE" <> help "Save type")
  <*> strOption ( long "attack" <> metavar "ATTACK" <> help "Attack type"))

data DeleteSpell = DeleteSpell { deleteSpellId :: Maybe String } deriving (Show, Eq)

deleteSpell :: Parser SpellAction
deleteSpell = SpellDelete <$> (DeleteSpell
  <$> optional (strOption
    ( long "id"
    <> metavar "ID"
    <> help "ID of Spell to delete")))

data UpdateSpell = UpdateSpell
  { updateSpellId :: Maybe String
  , updateSpellName :: Maybe String
  , updateSpellLevel :: Maybe Int
  , updateSpellRitual :: Maybe Bool
  , updateSpellAction :: Maybe String
  , updateSpellRange :: Maybe String
  , updateSpellComponents :: Maybe String
  , updateSpellDuration :: Maybe String
  , updateSpellTargets :: Maybe String
  , updateSpellAoe :: Maybe String
  , updateSpellSave :: Maybe String
  , updateSpellAttack :: Maybe String
  } deriving (Show, Eq)

updateSpell :: Parser SpellAction
updateSpell = SpellUpdate <$> (UpdateSpell
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of Spell to update"))
  <*> optional (strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "New name"))
  <*> optional (option auto ( long "level" <> metavar "INTEGER" <> help "New level"))
  <*> optional (option auto ( long "ritual" <> metavar "BOOL" <> help "New ritual flag"))
  <*> optional (strOption ( long "action" <> metavar "ACTION" <> help "New action"))
  <*> optional (strOption ( long "range" <> metavar "RANGE" <> help "New range"))
  <*> optional (strOption ( long "components" <> metavar "COMPONENTS" <> help "New components"))
  <*> optional (strOption ( long "duration" <> metavar "DURATION" <> help "New duration"))
  <*> optional (strOption ( long "targets" <> metavar "TARGETS" <> help "New targets"))
  <*> optional (strOption ( long "aoe" <> metavar "AOE" <> help "New aoe"))
  <*> optional (strOption ( long "save" <> metavar "SAVE" <> help "New save"))
  <*> optional (strOption ( long "attack" <> metavar "ATTACK" <> help "New attack")))

spellAction :: Parser SpellAction
spellAction = hsubparser
  ( command "create" (info (helper <*> createSpell) (progDesc "Create Spell"))
  <> command "delete" (info (helper <*> deleteSpell) (progDesc "Delete Spell"))
  <> command "update" (info (helper <*> updateSpell) (progDesc "Update Spell")))

-- Money
data MoneyAction
  = MoneyCreate CreateMoney
  | MoneyDelete DeleteMoney
  | MoneyUpdate UpdateMoney
  deriving (Show, Eq)

data CreateMoney = CreateMoney
  { createMoneyId :: Maybe String
  , createMoneyName :: String
  , createMoneyAmount :: String
  } deriving (Show, Eq)

createMoney :: Parser MoneyAction
createMoney = MoneyCreate <$> (CreateMoney
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of new Money entity"))
  <*> strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "Name of money entity")
  <*> strOption ( long "amount" <> metavar "AMOUNT" <> help "Amount string"))

data DeleteMoney = DeleteMoney { deleteMoneyId :: Maybe String } deriving (Show, Eq)

deleteMoney :: Parser MoneyAction
deleteMoney = MoneyDelete <$> (DeleteMoney
  <$> optional (strOption
    ( long "id"
    <> metavar "ID"
    <> help "ID of Money to delete")))

data UpdateMoney = UpdateMoney
  { updateMoneyId :: Maybe String
  , updateMoneyName :: Maybe String
  , updateMoneyAmount :: Maybe String
  } deriving (Show, Eq)

updateMoney :: Parser MoneyAction
updateMoney = MoneyUpdate <$> (UpdateMoney
  <$> optional (strOption ( long "id" <> metavar "ID" <> help "ID of Money to update"))
  <*> optional (strOption ( long "name" <> short 'n' <> metavar "NAME" <> help "New name"))
  <*> optional (strOption ( long "amount" <> metavar "AMOUNT" <> help "New amount")))

moneyAction :: Parser MoneyAction
moneyAction = hsubparser
  ( command "create" (info (helper <*> createMoney) (progDesc "Create Money"))
  <> command "delete" (info (helper <*> deleteMoney) (progDesc "Delete Money"))
  <> command "update" (info (helper <*> updateMoney) (progDesc "Update Money")))