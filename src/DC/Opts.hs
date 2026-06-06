module DC.Opts (
  flag,
  switch,
  arg,
  optArg,
  command,
  cliArg,
  Option(..)
) where

import DC.Parse (
  Parser,
  sat,
  char,
  string, space)
import Control.Applicative (Alternative((<|>), some, many))
import Data.Char (isLower, isAlphaNum)

data Option = 
  Switch Char |
  Flag String |
  Arg String |
  OptArg (String, String) |
  SubCommand String
  deriving (Show)

alphaNumLower :: Parser Char
alphaNumLower = sat (liftA2 (||) isAlphaNum isLower)

flag :: Parser String
flag = do
  _ <- string "--"
  
  some alphaNumLower

switch :: Parser Char
switch = char '-' *> alphaNumLower

arg :: Parser String
arg = some alphaNumLower

optArg :: Parser (String, String)
optArg = do
  o <- (: []) <$> switch <|> flag
  _ <- space
  a <- arg

  return (o, a)

command :: Parser String
command = some $ alphaNumLower <|> char '-'

cliArg :: Parser Option
cliArg = (Flag <$> flag)
  <|> (Switch <$> switch)
  <|> (Arg <$> arg)