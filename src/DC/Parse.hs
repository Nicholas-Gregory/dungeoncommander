{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module DC.Parse (
  Parser(..),
  item,
  sat,
  char,
  digit,
  string,
  space,
  whitespace,
  number,
  empty,
  caseInsensitiveString,
  caseInsensitiveChar
  ) where
import Control.Applicative (Alternative(empty, (<|>), some), optional)
import Data.Char (isDigit, isSpace, toUpper, toLower)
import DC.Error ( AppError(..), newBaseError, ErrorDetail (ParseError), ErrorContextFrame(..), annotateErrorPure )

newtype Parser a = Parser { runParser :: String -> Either AppError (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser r) = Parser $ \input -> case r input of
    Right (a, s) -> Right (f a, s)
    Left e -> Left e

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \s -> Right (x, s)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser f) <*> (Parser a) = Parser $ \input -> case f input of
    Right (f', s) -> case a s of
      Right (v, s') -> Right (f' v, s')
      Left e -> Left e
    Left e -> Left e

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const $ Left $ newBaseError $ ParseError "Parsing error"
  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser a) <|> (Parser b) = Parser $ \input -> case a input of
    Right result -> Right result
    Left _ -> case b input of
      Right r -> Right r
      Left e2 -> annotateErrorPure (ErrorContextFrame 
        { errorAction = "parserBranch"
        , errorData = [("input", input)] 
        }) (Left e2)

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser a) >>= f = Parser $ \input -> case a input of
    Left e -> Left e
    Right (a', s) -> runParser (f a') s

instance MonadFail Parser where 
  fail :: String -> Parser a
  fail s = Parser $ \_ -> Left $ newBaseError $ ParseError s

item :: Parser Char
item = Parser $ \case
  "" -> Left $ newBaseError $ ParseError "Cannot parse empty input"
  (x:xs) -> Right (x, xs)

sat :: String -> (Char -> Bool) -> Parser Char
sat errMsg p = Parser $ \case
  "" -> Left $ newBaseError $ ParseError "Unexpected end of input"
  (x:xs) -> if p x
            then Right (x, xs)
            else Left $ newBaseError $ ParseError (errMsg <> " (found '" <> [x] <> "')")

char :: Char -> Parser Char
char c = sat ("Expected '" <> [c] <> "'") (== c)

digit :: Parser Char
digit = sat "Expected a digit" isDigit

string :: String -> Parser String
string "" = empty
string [x] = (: []) <$> char x
string (x:xs) = (:) <$> char x <*> string xs

space :: Parser String
space = some $ char ' '

whitespace :: Parser String
whitespace = some $ sat "Expected some whitespace" isSpace

number :: Parser Int
number = (\s -> read s :: Int) <$> some digit

caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = sat ("Expected '" <> [c] <> "' or '" <> [toUpper c] <> "'") ((== c) . toLower)

caseInsensitiveString :: String -> Parser String
caseInsensitiveString "" = empty
caseInsensitiveString [x] = (: []) <$> caseInsensitiveChar x
caseInsensitiveString (x:xs) = (:) <$> caseInsensitiveChar x <*> caseInsensitiveString xs