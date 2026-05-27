{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module DC.Parse (
  runParser,
  item,
  sat,
  char
  ) where

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser r) = Parser { runParser = maybe Nothing (\ (v, s) -> Just (f v, s)) . r }

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \s -> Just (x, s)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser $ \s -> maybe Nothing (\ (f, s') -> maybe Nothing (\ (v, s'') -> Just (f v, s'')) (runParser p2 s')) (runParser p1 s)

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  m >>= f = Parser $ \s -> maybe Nothing (\ (v, s') -> runParser (f v) s') (runParser m s) 

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

char :: Char -> Parser Char
char c = sat (== c)
