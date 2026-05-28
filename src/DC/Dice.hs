module DC.Dice (processExpression) where

import DC.Parse ( Parser, char, digit, space, runParser )
import Control.Applicative (Alternative((<|>), some, empty), optional)
import System.Random (StdGen, randomRs)

number :: Parser Int
number = (\s -> read s :: Int) <$> some digit

dice :: Parser (Int, Int)
dice = (\n _ t -> (n, t)) <$> number <*> char 'd' <*> number

modifier :: Parser Int
modifier = do
  operator <- optional space *> (char '-' <|> char '+')
  n <- optional space *> number

  return $ case operator of
    '+' -> n
    '-' -> negate n
    _ -> 0

expression :: Parser ((Int, Int), Maybe Int)
expression = (,) <$> dice <*> optional modifier

rollDice :: StdGen -> Int -> Int -> Int
rollDice gen n t = sum $ take n $ randomRs (1, t) gen

processExpression :: StdGen -> String -> Maybe Int
processExpression gen expr = do
  (((n, t), m), _) <- runParser expression expr

  return $ case m of
    Nothing -> rollDice gen n t
    Just m' -> m' + rollDice gen n t
  

