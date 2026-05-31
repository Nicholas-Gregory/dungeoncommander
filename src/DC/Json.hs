{-# LANGUAGE LambdaCase #-}

module DC.Json (
  jsonString,
  jsonBool,
  jsonArray
) where
import DC.Parse (Parser, item, char, sat, string, whitespace, number)
import Control.Applicative (Alternative((<|>), many, some), optional)
import Data.Map (Map)
import Data.Maybe (isNothing)

-- This is only a subset of JSON for use in the context of this app
-- Not intended to be used as if it were a full implementation of the spec

data JsonValue
  = JsonNumber Int
  | JsonString String
  | JsonBool Bool
  | JsonArray [JsonValue]
  | JsonObject (Map String JsonValue)
  deriving (Show)

jsonEscape :: Parser String
jsonEscape = (\_ c -> '\\' : [c]) <$> char '\\' <*> item

jsonString :: Parser String
jsonString = (\_ s _ -> s) 
  <$> char '"' 
  <*> (concat <$> many (jsonEscape <|> some (sat (liftA2 (&&) (/='"') (/='\\')))))
  <*> char '"'

jsonBool :: Parser Bool
jsonBool = (== "true") <$> (string "true" <|> string "false")

jsonInt :: Parser Int
jsonInt = do
  sign <- optional $ char '-'
  n <- number

  return (if isNothing sign then n else negate n)

jsonArray :: Parser [JsonValue]
jsonArray = do
  _ <- char '['
  inner <- many $ ((JsonString <$> jsonString)
           <|> (JsonBool <$> jsonBool)
           <|> (JsonNumber <$> jsonInt)
           <|> (JsonArray <$> jsonArray))
           <* optional (char ',')
           <* optional whitespace
  _ <- char ']'

  return inner

-- jsonObject :: Parser JsonValue
-- jsonObject = do
--   let map = empty :: Map String JsonValue
--   _ <- '{'
  
  

  