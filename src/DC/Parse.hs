{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module DC.Parse (
  Parser,
  runParser,
  item,
  sat,
  char,
  digit,
  string,
  space,
  whitespace,
  number
  ) where
import Control.Applicative (Alternative(empty, (<|>), some))
import Data.Char (isDigit, isSpace)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser r) = Parser { runParser = maybe Nothing (\ (v, s) -> Just (f v, s)) . r }

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \s -> Just (x, s)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser $ \s -> maybe Nothing (\ (f, s') -> maybe Nothing (\ (v, s'') -> Just (f v, s'')) (runParser p2 s')) (runParser p1 s)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \s -> case runParser p1 s of
    Nothing -> runParser p2 s
    Just result -> Just result

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  m >>= f = Parser $ \s -> maybe Nothing (\ (v, s') -> runParser (f v) s') (runParser m s) 

instance MonadFail Parser where 
  fail :: String -> Parser a
  fail _ = empty

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

digit :: Parser Char
digit = sat isDigit

string :: String -> Parser String
string "" = empty
string [x] = (: []) <$> char x
string (x:xs) = (:) <$> char x <*> string xs

space :: Parser String
space = some $ char ' '

whitespace :: Parser String
whitespace = some $ sat isSpace

number :: Parser Int
number = (\s -> read s :: Int) <$> some digit