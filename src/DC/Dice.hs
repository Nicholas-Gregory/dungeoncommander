module DC.Dice (
  processExpression,
  rollDice
  ) where

import DC.Parse ( Parser, char, number, space, runParser )
import Control.Applicative (Alternative((<|>), some), optional)
import System.Random (StdGen, randomRs)
import DC.Error (AppError)

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

processExpression :: StdGen -> String -> Either AppError Int
processExpression gen expr = case runParser expression expr of
    Right (((n, t), m), _) -> case m of
      Nothing -> Right $ rollDice gen n t
      Just m' -> Right $ m' + rollDice gen n t
    Left e -> Left e