module Main where

import System.Exit
import Test.QuickCheck
import EntityProps (props)

main :: IO ()
main = do
  results <- mapM quickCheckResult props
  let good = all allSuccess results
  if good then exitSuccess else exitFailure

allSuccess :: Result -> Bool
allSuccess r = case r of
  Success {} -> True
  _ -> False