module DC.Opts (

) where

import Options.Applicative

data Command
  = SceneCommand SceneAction
  | ActorCommand ActorAction
  | ObjectCommand 
  | TrapCommand 
  | ItemCommand 
  | ArmorCommand 
  | WeaponCommand 
  | ContainerCommand 
  | MountCommand 
  | SpellCommand 
  | MoneyCommand 

data SceneAction
  = SceneCreate
  | SceneDelete
  | SceneUpdate UpdateScene
  | SceneAdd
  | SceneRemove
  | SceneAddTo
  | SceneRemoveFrom

data ActorAction
  = ActorCreate
  | ActorDelete
  | ActorUpdate UpdateActor
  | ActorAdd
  | ActorRemove
  | ActorAddTo
  | ActorRemoveFrom

rootCommand :: Parser Command
rootCommand = hsubparser
  ( command "scene" (info (helper <*> (SceneCommand <$> sceneAction)) 
    (progDesc "Select a Scene, or manage Scenes"))
  <> command "actor" (info (helper <*> (ActorCommand <$> actorAction))
    (progDesc "Select an Actor, or manage Actors")))

sceneAction :: Parser SceneAction
sceneAction = hsubparser
  ( command "update" (info (helper <*> updateScene) 
    (progDesc "Directly update values for a particular Scene")))

data UpdateScene = UpdateScene
  { sceneName :: Maybe String
  , sceneX :: Maybe Int
  , sceneY :: Maybe Int
  }

updateScene :: Parser SceneAction
updateScene = SceneUpdate <$> (UpdateScene
  <$> optional (strOption
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
  }

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