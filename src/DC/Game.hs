module DC.Game (
  GameState(..),
  Env(..),
  AppM
) where

import DC.Entity (Entity)
import System.Random (StdGen)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.IORef (IORef)

data GameState = GameState 
  { scene :: Entity
  , gen :: StdGen
  , commits :: [Entity]
  }

data Env = Env
  { socketPath :: FilePath
  , dbPath :: FilePath
  , state :: IORef GameState
  }

type AppM a = ExceptT String (ReaderT Env IO) a