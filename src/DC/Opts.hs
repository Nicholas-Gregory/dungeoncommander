module DC.Opts (
  flag,
  switch,
  arg,
  optArg,
  command,
  cliArg,
  numList,
  numListArg,
  Option(..)
) where

import DC.Parse (
  Parser(..),
  sat,
  char,
  string, space, number)
import Control.Applicative (Alternative((<|>), some, many))
import Data.Char (isLower, isAlphaNum, isNumber)

data Option = 
  Switch Char |
  Flag String |
  Arg String |
  OptArg (String, String) |
  SubCommand String |
  NumList (String, [Int])
  deriving (Show)

alphaNumLower :: Parser Char
alphaNumLower = sat "Expected alphanumeric" (liftA2 (||) isAlphaNum isLower)

flag :: Parser String
flag = do
  _ <- string "--"
  
  some alphaNumLower

switch :: Parser Char
switch = char '-' *> alphaNumLower

numList :: Parser [Int]
numList = some $ number <* char ',' <* space

arg :: Parser String
arg = some alphaNumLower

optArg :: Parser Option
optArg = do
  o <- (: []) <$> switch <|> flag
  _ <- space
  a <- arg

  return $ OptArg (o, a)

numListArg :: Parser Option
numListArg = do
  o <- (: []) <$> switch <|> flag
  _ <- char '='
  l <- numList

  return $ NumList (o, l)

command :: Parser String
command = some $ alphaNumLower <|> char '-'

cliArg :: Parser Option
cliArg = (Flag <$> flag)
  <|> (Switch <$> switch)
  <|> (Arg <$> arg)