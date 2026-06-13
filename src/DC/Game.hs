module DC.Game (
 GameState(..),
 GameAction
) where

import DC.Entity (Entity)
import System.Random (StdGen)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)

data GameState = GameState
  { scene :: Entity
  , gen :: StdGen
  }

type GameAction a = ExceptT String (StateT GameState IO) a