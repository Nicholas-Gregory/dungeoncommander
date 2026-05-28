module DC.Dice where

import DC.Parse ( Parser, char, digit, space )
import Control.Applicative (Alternative((<|>), some), optional)

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

expression :: Parser ((Int, Int), Maybe Int)
expression = (,) <$> dice <*> optional modifier
  