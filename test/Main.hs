module Main where

import System.Exit

main :: IO ()
main = do
  -- add test runners into the array for each module
  good <- and <$> sequence []
  if good
     then exitSuccess
     else exitFailure