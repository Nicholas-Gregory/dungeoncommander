module Main where

import Tests.Roll (hello)   
import System.Exit

main :: IO ()
main = do
  -- add test runners into the array for each module
  good <- and <$> sequence [hello]
  if good
     then exitSuccess
     else exitFailure