module Main where

import System.Exit
import DC.Parse

main :: IO ()
main = do
  -- add test runners into the array for each module
  good <- and <$> sequence []
  if good
     then exitSuccess
     else exitFailure