{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module DC.Parse (
  item,
  sat
  ) where

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser r) = Parser { runParser = maybe Nothing (\ (v, s) -> Just (f v, s)) . r }

item :: Parser Char
item = Parser $ \case
  "" -> Nothing
  (x:xs) -> Just (x, xs)

sat :: (Char -> Bool) -> Parser Char
sat p = Parser $ \case
  "" -> Nothing
  (x:xs) -> if p x
            then Just (x, xs)
            else Nothing
