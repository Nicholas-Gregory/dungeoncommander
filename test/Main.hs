module Main where

import System.Exit
import DC.Dice (expression)
import DC.Parse (runParser)

main :: IO ()
main = do
  -- add test runners into the array for each module
  print $ runParser expression "3d6+2"
  print $ runParser expression "2d10"
  print $ runParser expression "1d20 - 3"
  print $ runParser expression "abcd"
  good <- and <$> sequence []
  if good
     then exitSuccess
     else exitFailure