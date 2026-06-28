module DC.Opts (
 rootInfo,
 Command(..),
 SceneAction(..),
 CreateScene(..),
 RollOptions(..),
 RootOptions(..),
 SceneOptions(..),
 ActorOptions(..),
 ObjectOptions(..),
 TrapOptions(..),
 ItemOptions(..),
 ArmorOptions(..),
 WeaponOptions(..),
 ContainerOptions(..),
 MountOptions(..),
 SpellOptions(..),
 MoneyOptions(..),
 ActorAction(..),
 CreateActor(..)
) where

import Options.Applicative
import DC.Types
import DC.Error (AppError(AppError))
import DC.Json

data Command
  = RollCommand RollOptions
  | SceneCommand SceneOptions
  | ActorCommand ActorOptions
  | ObjectCommand ObjectOptions
  | TrapCommand TrapOptions
  | ItemCommand ItemOptions
  | ArmorCommand ArmorOptions
  | WeaponCommand WeaponOptions
  | ContainerCommand ContainerOptions
  | MountCommand MountOptions
  | SpellCommand SpellOptions
  | MoneyCommand MoneyOptions
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
  deriving (Show, Eq)

data ActorAction
  = ActorCreate CreateActor
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
  { rootSave :: Bool
  , rootVerbosity :: VerbosityLevel
  , focus :: Bool
  , noOutput :: Bool
  , rootCommand :: Maybe Command
  }

toVerbosity :: Int -> VerbosityLevel
toVerbosity 0 = Name
toVerbosity 1 = Stats
toVerbosity _ = All

rootParser :: Parser RootOptions
rootParser = RootOptions
  <$> switch
    (long "save"
    <> short 's'
    <> help "Use this flag to commit the result to disk")
  <*> (toVerbosity . length <$> many (flag' ()
    (long "verbosity"
    <> short 'v'
    <> help "Use a number of this flag to set the verbosity of printed messages")))
  <*> switch
    (long "focus"
    <> short 'f'
    <> help "Use to save the selected entity/entities as in focus, to use in further commands")
  <*> switch
    (long "no-output"
    <> short 'o'
    <> help "Use this flag to turn off stdout output (for seeing the human-readable output of a command without piping it)")
  <*> optional (hsubparser
    ( command "scene" (info (helper <*> (SceneCommand <$> sceneAction)) 
      (progDesc "Select a Scene, or manage Scenes"))
    <> command "actor" (info (helper <*> (ActorCommand <$> actorAction))
      (progDesc "Select an Actor, or manage Actors"))
    <> command "object" (info (helper <*> (ObjectCommand <$> objectOptions))
      (progDesc "Select an Object, or manage Objects"))
    <> command "trap" (info (helper <*> (TrapCommand <$> trapOptions))
      (progDesc "Select a Trap, or manage Traps"))
    <> command "item" (info (helper <*> (ItemCommand <$> itemOptions))
      (progDesc "Select an Item, or manage Items"))
    <> command "armor" (info (helper <*> (ArmorCommand <$> armorOptions))
      (progDesc "Select Armor, or manage Armor"))
    <> command "weapon" (info (helper <*> (WeaponCommand <$> weaponOptions))
      (progDesc "Select a Weapon, or manage Weapons"))
    <> command "container" (info (helper <*> (ContainerCommand <$> containerOptions))
      (progDesc "Select a Container, or manage Containers"))
    <> command "mount" (info (helper <*> (MountCommand <$> mountOptions))
      (progDesc "Select a Mount, or manage Mounts"))
    <> command "spell" (info (helper <*> (SpellCommand <$> spellOptions))
      (progDesc "Select a Spell, or manage Spells"))
    <> command "money" (info (helper <*> (MoneyCommand <$> moneyOptions))
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

data SceneOptions = SceneOptions
  { sceneIds :: [String]
  , sceneFilterX :: Maybe Int
  , sceneFilterY :: Maybe Int
  , sceneEntities :: Bool
  , sceneActors :: Bool
  , sceneObjects :: Bool
  , sceneCommand :: Maybe SceneAction
  } deriving (Show, Eq)

sceneAction :: Parser SceneOptions
sceneAction = SceneOptions
  <$> many (strOption
    (long "id"
    <> metavar "SCENE"
    <> help "The ID of a Scene on which to perform the action. Can specify multiple with further usage of --id"))
  <*> optional (option auto
    (long "filter-x"
    <> metavar "INTEGER"
    <> help "Filter Scenes by X dimension"))
  <*> optional (option auto
    (long "filter-y"
    <> metavar "INTEGER"
    <> help "Filter Scenes by Y dimension"))
  <*> switch
    (long "entities"
    <> help "Use all the Entities in the Scene for the action")
  <*> switch
    (long "actors"
    <> help "Use the Actors in the Scene for the action")
  <*> switch
    (long "objects"
    <> help "Use the Objects in the Scene for the action")
  <*> optional (hsubparser
    (command "update" (info (helper <*> updateScene) 
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
      (progDesc "Remove an Actor from a Scene"))))


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

data ActorOptions = ActorOptions
  { actorIds :: [String]
  , actorFilterX :: Maybe Int
  , actorFilterY :: Maybe Int
  , actorFilterCurrentHp :: Maybe Int
  , actorFilterMaxHp :: Maybe Int
  , actorFilterCha :: Maybe Int
  , actorFilterInt :: Maybe Int
  , actorFilterCon :: Maybe Int
  , actorFilterStr :: Maybe Int
  , actorFilterDex :: Maybe Int
  , actorFilterWis :: Maybe Int
  , actorFilterHitDice :: Maybe String
  , actorFilterAc :: Maybe Int
  , actorFilterLevel :: Maybe Int
  , actorCarriedItems :: Bool
  , actorHeldItem :: Bool
  , actorKnownSpells :: Bool
  , actorPreparedSpells :: Bool
  , actorDonnedArmor :: Bool
  , actorWieldedWeapon :: Bool
  , actorCommand :: Maybe ActorAction
  } deriving (Show, Eq)

actorAction :: Parser ActorOptions
actorAction = ActorOptions
  <$> many (strOption
    (long "id"
    <> metavar "ACTOR"
    <> help "The ID of an Actor on which to perform the action. Can specify multiple with further usage of --id"))
  <*> optional (option auto
    (long "filter-x"
    <> metavar "INTEGER"
    <> help "Filter Actors by their X coordinate"))
  <*> optional (option auto
    (long "filter-y"
    <> metavar "INTEGER"
    <> help "Filter Actors by their Y coordinate"))
  <*> optional (option auto
    (long "filter-current-hp"
    <> metavar "INTEGER"
    <> help "Filter Actors by their current HP"))
  <*> optional (option auto
    (long "filter-max-hp"
    <> metavar "INTEGER"
    <> help "Filter Actors by their maximum HP"))
  <*> optional (option auto
    (long "filter-cha"
    <> metavar "INTEGER"
    <> help "Filter Actors by their Charisma ability score"))
  <*> optional (option auto
    (long "filter-int"
    <> metavar "INTEGER"
    <> help "Filter Actors by their Intelligence ability score"))
  <*> optional (option auto
    (long "filter-con"
    <> metavar "INTEGER"
    <> help "Filter Actors by their Constitution ability score"))
  <*> optional (option auto
    (long "filter-str"
    <> metavar "INTEGER"
    <> help "Filter Actors by their Strength ability score"))
  <*> optional (option auto
    (long "filter-dex"
    <> metavar "INTEGER"
    <> help "Filter Actors by their Dexterity ability score"))
  <*> optional (option auto
    (long "filter-wis"
    <> metavar "INTEGER"
    <> help "Filter Actors by their Wissdom ability score"))
  <*> optional (strOption
    (long "filter-hd"
    <> metavar "DICE"
    <> help "Pass a Dice Expression to filter Actors by their Hit Dice"))
  <*> optional (option auto
    (long "filter-ac"
    <> metavar "INTEGER"
    <> help "Filter Actors by their Armor Class"))
  <*> optional (option auto
    (long "filter-level"
    <> metavar "INTEGER"
    <> help "Filter Actors by their Character Level"))
  <*> switch
    (long "carried-items"
    <> help "Use the Actor's carried items in the action")
  <*> switch
    (long "held-items"
    <> help "Use the Actor's held items in the action")
  <*> switch
    (long "known-spells"
    <> help "Use the Actor's known spells in the action")
  <*> switch
    (long "prepared-spells"
    <> help "Use the Actor's prepared spells in the action")
  <*> switch
    (long "donned-armor"
    <> help "Use the Actor's donned armor in the action")
  <*> switch
    (long "wielded-weapons"
    <> help "Use the Actor's wielded weapon(s) in the action")
  <*> optional (hsubparser
    (command "update" (info (helper <*> updateActor)
      (progDesc "Directly update values for a particular Actor"))
    <> command "create" (info (helper <*> createActor)
      (progDesc "Directly create a new Actor by providing values"))))

data CreateActor = CreateActor
  { createActorId :: String
  , createActorName :: String
  , createActorX :: Int
  , createActorY :: Int
  , createActorCurrentHp :: Int
  , createActorMaxHp :: Int
  , createActorCha :: Int
  , createActorInt :: Int
  , createActorCon :: Int
  , createActorStr :: Int
  , createActorDex :: Int
  , createActorWis :: Int
  , createActorHitDice :: String
  , createActorAc :: Int
  , createActorLevel  :: Int
  , createActorSaveProficiencies :: [Either AppError Ability]
  , createActorWeaponProficiencies :: [Either AppError WeaponProficiency]
  } deriving (Show, Eq)

createActor :: Parser ActorAction
createActor = ActorCreate <$> (CreateActor
  <$> strOption
    (long "id"
    <> metavar "ACTOR-ID"
    <> help "The ID of the new Actor")
  <*> strOption
    (long "name"
    <> short 'n'
    <> metavar "ACTOR-NAME"
    <> help "The name of the new Actor")
  <*> option auto
    (short 'x'
    <> metavar "INTEGER"
    <> help "The X coordinate of the new Actor")
  <*> option auto
    (short 'y'
    <> metavar "INTEGER"
    <> help "The Y coordinate of the new Actor")
  <*> option auto
    (long "current-hp"
    <> metavar "INTEGER"
    <> help "The current HP of the new Actor")
  <*> option auto
    (long "max-hp"
    <> metavar "INTEGER"
    <> help "The maximum HP of the new Actor")
  <*> option auto
    (long "cha"
    <> metavar "INTEGER"
    <> help "The Charisma ability score of the new Actor")
  <*> option auto
    (long "int"
    <> metavar "INTEGER"
    <> help "The Intelligence ability score of the new Actor")
  <*> option auto
    (long "con"
    <> metavar "INTEGER"
    <> help "The Constitution ability score of the new Actor")
  <*> option auto
    (long "str"
    <> metavar "INTEGER"
    <> help "The Strength ability score of the new Actor")
  <*> option auto
    (long "dex"
    <> metavar "INTEGER"
    <> help "The Dexterity ability score of the new Actor")
  <*> option auto
    (long "wis"
    <> metavar "INTEGER"
    <> help "The Wisdom ability score of the new Actor")
  <*> strOption
    (long "hit-dice"
    <> long "hd"
    <> metavar "DICE"
    <> help "A dice expression specifying the hit dice of the new Actor")
  <*> option auto
    (long "ac"
    <> long "armor-class"
    <> metavar "INTEGER"
    <> help "The armor class of the new Actor")
  <*> option auto
    (long "level"
    <> short 'l'
    <> metavar "INTEGER"
    <> help "The character level of the new Actor")
  <*> many ((\s -> fromValue (JsonString s) :: Either AppError Ability) <$> strOption
    (long "save-proficiency"
    <> long "sp"
    <> metavar "ABILITY"
    <> help "Three-letter ability score identifier (cha, int, wis, dex, str, con). Can be used multiple times"))
  <*> many ((\s -> fromValue (JsonString s) :: Either AppError WeaponProficiency) <$> strOption
    (long "weapon-proficiency"
    <> long "wp"
    <> metavar "WEAPON-PROFICIENCY"
    <> help "Either 'simple', 'martial', or a string consisting of one of the official D&d 5e weapon names from the Basic Rules weapons table. Exact match in quotes, lowercase.")))

data UpdateActor = UpdateActor
  { updateActorId :: String
  , actorName :: Maybe String
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
  , actorHitDice :: Maybe String
  , actorAc :: Maybe Int
  , actorLevel :: Maybe Int
  , updateActorSaveProficiencies :: [Either AppError Ability]
  , updateActorWeaponProficiencies :: [Either AppError WeaponProficiency]
  } deriving (Show, Eq)

updateActor :: Parser ActorAction
updateActor = ActorUpdate <$> (UpdateActor
  <$> strOption
    (long "id"
    <> metavar "ACTOR-ID"
    <> help "The ID of the actor to update")
  <*> optional (strOption
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
    <> help "The new Level of the Actor"))
    <*> many ((\s -> fromValue (JsonString s) :: Either AppError Ability) <$> strOption
    (long "save-proficiency"
    <> long "sp"
    <> metavar "ABILITY"
    <> help "Three-letter ability score identifier (cha, int, wis, dex, str, con). Can be used multiple times"))
  <*> many ((\s -> fromValue (JsonString s) :: Either AppError WeaponProficiency) <$> strOption
    (long "weapon-proficiency"
    <> long "wp"
    <> metavar "WEAPON-PROFICIENCY"
    <> help "Either 'simple', 'martial', or a string consisting of one of the official D&d 5e weapon names from the Basic Rules weapons table. Exact match in quotes, lowercase.")))

-- Object
data ObjectAction
  = ObjectCreate CreateObject
  | ObjectDelete DeleteObject
  | ObjectUpdate UpdateObject
  deriving (Show, Eq)

data CreateObject = CreateObject
  { createObjectId :: String
  , createObjectName :: String
  , createObjectAc :: Int
  , createObjectMaxHp :: Int
  , createObjectX :: Int
  , createObjectY :: Int
  } deriving (Show, Eq)

createObject :: Parser ObjectAction
createObject = ObjectCreate <$> (CreateObject
  <$> strOption
    ( long "id"
    <> metavar "ID"
    <> help "The ID of the new Object")
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

data ObjectOptions = ObjectOptions
  { objectIds :: [String]
  , objectFilterX :: Maybe Int
  , objectFilterY :: Maybe Int
  , objectFilterAc :: Maybe Int
  , objectFilterMaxHp :: Maybe Int
  , objectFilterCurrentHp :: Maybe Int
  , objectCommand :: Maybe ObjectAction
  } deriving (Show, Eq)

objectOptions :: Parser ObjectOptions
objectOptions = ObjectOptions
  <$> many (strOption ( long "id" <> metavar "OBJECT" <> help "The ID of an Object"))
  <*> optional (option auto ( long "filter-x" <> metavar "INTEGER" <> help "Filter Objects by X coordinate"))
  <*> optional (option auto ( long "filter-y" <> metavar "INTEGER" <> help "Filter Objects by Y coordinate"))
  <*> optional (option auto ( long "filter-ac" <> metavar "INTEGER" <> help "Filter Objects by Armor Class"))
  <*> optional (option auto ( long "filter-max-hp" <> metavar "INTEGER" <> help "Filter Objects by maximum HP"))
  <*> optional (option auto ( long "filter-current-hp" <> metavar "INTEGER" <> help "Filter Objects by current HP"))
  <*> optional (hsubparser
    ( command "create" (info (helper <*> createObject) (progDesc "Create an Object"))
    <> command "delete" (info (helper <*> deleteObject) (progDesc "Delete an Object"))
    <> command "update" (info (helper <*> updateObject) (progDesc "Update an Object"))))

-- Trap
data TrapAction
  = TrapCreate CreateTrap
  | TrapDelete DeleteTrap
  | TrapUpdate UpdateTrap
  deriving (Show, Eq)

data CreateTrap = CreateTrap
  { createTrapId :: String
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
  <$> strOption
    ( long "id" <> metavar "ID" <> help "ID of the new Trap")
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

data TrapOptions = TrapOptions
  { trapIds :: [String]
  , trapFilterX :: Maybe Int
  , trapFilterY :: Maybe Int
  , trapFilterDetectDc :: Maybe Int
  , trapFilterAttackBonus :: Maybe Int
  , trapFilterSaveDc :: Maybe Int
  , trapCommand :: Maybe TrapAction
  } deriving (Show, Eq)

trapOptions :: Parser TrapOptions
trapOptions = TrapOptions
  <$> many (strOption ( long "id" <> metavar "TRAP" <> help "The ID of a Trap"))
  <*> optional (option auto ( long "filter-x" <> metavar "INTEGER" <> help "Filter Traps by X coordinate"))
  <*> optional (option auto ( long "filter-y" <> metavar "INTEGER" <> help "Filter Traps by Y coordinate"))
  <*> optional (option auto ( long "filter-detect-dc" <> metavar "INTEGER" <> help "Filter Traps by detect DC"))
  <*> optional (option auto ( long "filter-attack-bonus" <> metavar "INTEGER" <> help "Filter Traps by attack bonus"))
  <*> optional (option auto ( long "filter-save-dc" <> metavar "INTEGER" <> help "Filter Traps by save DC"))
  <*> optional (hsubparser
    ( command "create" (info (helper <*> createTrap) (progDesc "Create a Trap"))
    <> command "delete" (info (helper <*> deleteTrap) (progDesc "Delete a Trap"))
    <> command "update" (info (helper <*> updateTrap) (progDesc "Update a Trap"))))

-- Item
data ItemAction
  = ItemCreate CreateItem
  | ItemDelete DeleteItem
  | ItemUpdate UpdateItem
  deriving (Show, Eq)

data CreateItem = CreateItem
  { createItemId :: String
  , createItemName :: String
  , createItemCost :: String
  , createItemWeight :: String
  } deriving (Show, Eq)

createItem :: Parser ItemAction
createItem = ItemCreate <$> (CreateItem
  <$> strOption ( long "id" <> metavar "ID" <> help "ID of new Item")
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

data ItemOptions = ItemOptions
  { itemIds :: [String]
  , itemFilterCost :: Maybe String
  , itemFilterWeight :: Maybe String
  , itemCommand :: Maybe ItemAction
  } deriving (Show, Eq)

itemOptions :: Parser ItemOptions
itemOptions = ItemOptions
  <$> many (strOption ( long "id" <> metavar "ITEM" <> help "The ID of an Item"))
  <*> optional (strOption ( long "filter-cost" <> metavar "COST" <> help "Filter Items by cost"))
  <*> optional (strOption ( long "filter-weight" <> metavar "WEIGHT" <> help "Filter Items by weight"))
  <*> optional (hsubparser
    ( command "create" (info (helper <*> createItem) (progDesc "Create an Item"))
    <> command "delete" (info (helper <*> deleteItem) (progDesc "Delete an Item"))
    <> command "update" (info (helper <*> updateItem) (progDesc "Update an Item"))))

-- Armor
data ArmorAction
  = ArmorCreate CreateArmor
  | ArmorDelete DeleteArmor
  | ArmorUpdate UpdateArmor
  deriving (Show, Eq)

data CreateArmor = CreateArmor
  { createArmorId :: String
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
  <$> strOption ( long "id" <> metavar "ID" <> help "ID of new Armor")
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

data ArmorOptions = ArmorOptions
  { armorIds :: [String]
  , armorFilterAc :: Maybe Int
  , armorFilterStr :: Maybe Int
  , armorFilterStealth :: Maybe Bool
  , armorFilterType :: Maybe String
  , armorCommand :: Maybe ArmorAction
  } deriving (Show, Eq)

armorOptions :: Parser ArmorOptions
armorOptions = ArmorOptions
  <$> many (strOption ( long "id" <> metavar "ARMOR" <> help "The ID of an Armor"))
  <*> optional (option auto ( long "filter-ac" <> metavar "INTEGER" <> help "Filter Armor by AC"))
  <*> optional (option auto ( long "filter-str" <> metavar "INTEGER" <> help "Filter Armor by Strength"))
  <*> optional (option auto ( long "filter-stealth-disadvantage" <> metavar "BOOL" <> help "Filter Armor by stealth disadvantage"))
  <*> optional (strOption ( long "filter-type" <> metavar "TYPE" <> help "Filter Armor by type"))
  <*> optional (hsubparser
    ( command "create" (info (helper <*> createArmor) (progDesc "Create Armor"))
    <> command "delete" (info (helper <*> deleteArmor) (progDesc "Delete Armor"))
    <> command "update" (info (helper <*> updateArmor) (progDesc "Update Armor"))))

-- Weapon
data WeaponAction
  = WeaponCreate CreateWeapon
  | WeaponDelete DeleteWeapon
  | WeaponUpdate UpdateWeapon
  deriving (Show, Eq)

data CreateWeapon = CreateWeapon
  { createWeaponId :: String
  , createWeaponName :: String
  , createWeaponCost :: String
  , createWeaponWeight :: String
  , createWeaponDamage :: String
  , createWeaponProperties :: Maybe String
  , createWeaponWeapon :: Maybe String
  } deriving (Show, Eq)

createWeapon :: Parser WeaponAction
createWeapon = WeaponCreate <$> (CreateWeapon
  <$> strOption ( long "id" <> metavar "ID" <> help "ID of new Weapon")
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

data WeaponOptions = WeaponOptions
  { weaponIds :: [String]
  , weaponFilterDamage :: Maybe String
  , weaponFilterProperties :: Maybe String
  , weaponFilterWeapon :: Maybe String
  , weaponCommand :: Maybe WeaponAction
  } deriving (Show, Eq)

weaponOptions :: Parser WeaponOptions
weaponOptions = WeaponOptions
  <$> many (strOption ( long "id" <> metavar "WEAPON" <> help "The ID of a Weapon"))
  <*> optional (strOption ( long "filter-damage" <> metavar "DICE" <> help "Filter Weapons by damage"))
  <*> optional (strOption ( long "filter-properties" <> metavar "PROPS" <> help "Filter Weapons by properties"))
  <*> optional (strOption ( long "filter-weapon" <> metavar "WEAPON" <> help "Filter Weapons by weapon identifier"))
  <*> optional (hsubparser
    ( command "create" (info (helper <*> createWeapon) (progDesc "Create Weapon"))
    <> command "delete" (info (helper <*> deleteWeapon) (progDesc "Delete Weapon"))
    <> command "update" (info (helper <*> updateWeapon) (progDesc "Update Weapon"))))

-- Container
data ContainerAction
  = ContainerCreate CreateContainer
  | ContainerDelete DeleteContainer
  | ContainerUpdate UpdateContainer
  deriving (Show, Eq)

data CreateContainer = CreateContainer
  { createContainerId :: String
  , createContainerName :: String
  , createContainerCost :: String
  , createContainerWeight :: String
  , createContainerCapacity :: String
  } deriving (Show, Eq)

createContainer :: Parser ContainerAction
createContainer = ContainerCreate <$> (CreateContainer
  <$> strOption ( long "id" <> metavar "ID" <> help "ID of new Container")
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

data ContainerOptions = ContainerOptions
  { containerIds :: [String]
  , containerFilterCapacity :: Maybe String
  , containerCommand :: Maybe ContainerAction
  } deriving (Show, Eq)

containerOptions :: Parser ContainerOptions
containerOptions = ContainerOptions
  <$> many (strOption ( long "id" <> metavar "CONTAINER" <> help "The ID of a Container"))
  <*> optional (strOption ( long "filter-capacity" <> metavar "CAPACITY" <> help "Filter Containers by capacity"))
  <*> optional (hsubparser
    ( command "create" (info (helper <*> createContainer) (progDesc "Create Container"))
    <> command "delete" (info (helper <*> deleteContainer) (progDesc "Delete Container"))
    <> command "update" (info (helper <*> updateContainer) (progDesc "Update Container"))))

-- Mount
data MountAction
  = MountCreate CreateMount
  | MountDelete DeleteMount
  | MountUpdate UpdateMount
  deriving (Show, Eq)

data CreateMount = CreateMount
  { createMountId :: String
  , createMountName :: String
  , createMountSpeed :: Int
  , createMountCarrying :: Int
  } deriving (Show, Eq)

createMount :: Parser MountAction
createMount = MountCreate <$> (CreateMount
  <$> strOption ( long "id" <> metavar "ID" <> help "ID of new Mount")
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

data MountOptions = MountOptions
  { mountIds :: [String]
  , mountFilterSpeed :: Maybe Int
  , mountFilterCarrying :: Maybe Int
  , mountCommand :: Maybe MountAction
  } deriving (Show, Eq)

mountOptions :: Parser MountOptions
mountOptions = MountOptions
  <$> many (strOption ( long "id" <> metavar "MOUNT" <> help "The ID of a Mount"))
  <*> optional (option auto ( long "filter-speed" <> metavar "INTEGER" <> help "Filter Mounts by speed"))
  <*> optional (option auto ( long "filter-carrying" <> metavar "INTEGER" <> help "Filter Mounts by carrying capacity"))
  <*> optional (hsubparser
    ( command "create" (info (helper <*> createMount) (progDesc "Create Mount"))
    <> command "delete" (info (helper <*> deleteMount) (progDesc "Delete Mount"))
    <> command "update" (info (helper <*> updateMount) (progDesc "Update Mount"))))

-- Spell
data SpellAction
  = SpellCreate CreateSpell
  | SpellDelete DeleteSpell
  | SpellUpdate UpdateSpell
  deriving (Show, Eq)

data CreateSpell = CreateSpell
  { createSpellId :: String
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
  <$> strOption ( long "id" <> metavar "ID" <> help "ID of new Spell")
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

data SpellOptions = SpellOptions
  { spellIds :: [String]
  , spellFilterLevel :: Maybe Int
  , spellFilterRitual :: Maybe Bool
  , spellCommand :: Maybe SpellAction
  } deriving (Show, Eq)

spellOptions :: Parser SpellOptions
spellOptions = SpellOptions
  <$> many (strOption ( long "id" <> metavar "SPELL" <> help "The ID of a Spell"))
  <*> optional (option auto ( long "filter-level" <> metavar "INTEGER" <> help "Filter Spells by level"))
  <*> optional (option auto ( long "filter-ritual" <> metavar "BOOL" <> help "Filter Spells by ritual flag"))
  <*> optional (hsubparser
    ( command "create" (info (helper <*> createSpell) (progDesc "Create Spell"))
    <> command "delete" (info (helper <*> deleteSpell) (progDesc "Delete Spell"))
    <> command "update" (info (helper <*> updateSpell) (progDesc "Update Spell"))))

-- Money
data MoneyAction
  = MoneyCreate CreateMoney
  | MoneyDelete DeleteMoney
  | MoneyUpdate UpdateMoney
  deriving (Show, Eq)

data CreateMoney = CreateMoney
  { createMoneyId :: String
  , createMoneyName :: String
  , createMoneyAmount :: String
  } deriving (Show, Eq)

createMoney :: Parser MoneyAction
createMoney = MoneyCreate <$> (CreateMoney
  <$> strOption ( long "id" <> metavar "ID" <> help "ID of new Money entity")
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

data MoneyOptions = MoneyOptions
  {moneyIds :: [String]
  , moneyFilterAmount :: Maybe String
  , moneyCommand :: Maybe MoneyAction
  } deriving (Show, Eq)

moneyOptions :: Parser MoneyOptions
moneyOptions = MoneyOptions
  <$> many (strOption ( long "id" <> metavar "MONEY" <> help "The ID of a Money entity"))
  <*> optional (strOption ( long "filter-amount" <> metavar "AMOUNT" <> help "Filter Money by amount string"))
  <*> optional (hsubparser
    ( command "create" (info (helper <*> createMoney) (progDesc "Create Money"))
    <> command "delete" (info (helper <*> deleteMoney) (progDesc "Delete Money"))
    <> command "update" (info (helper <*> updateMoney) (progDesc "Update Money"))))