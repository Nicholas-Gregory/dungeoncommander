module DC.Opts (
  flag,
  switch,
  arg,
  optArg,
  command
) where

import DC.Parse (
  Parser,
  sat,
  char,
  string, space)
import Control.Applicative (Alternative((<|>), some))
import Data.Char (isLower, isAlphaNum)

alphaNumLower :: Parser Char
alphaNumLower = sat (liftA2 (&&) isAlphaNum isLower)

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

  