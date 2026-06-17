{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module DC.Json (
  jsonString,
  jsonBool,
  jsonArray,
  jsonObject,
  writeJsonValue,
  isJsonArray,
  isJsonObject,
  getField,
  JsonObjectMap,
  JsonValue(..),
  FromJson(..),
  ToJson(..),
  IsJson(..)
) where
import DC.Parse (Parser (runParser), item, char, sat, string, whitespace, number, space)
import Control.Applicative (Alternative((<|>), many, some), optional)
import qualified Data.Map as M (Map, fromList, toList, lookup)
import Data.Maybe (isNothing)
import Data.Foldable (foldl')


-- This is only a subset of JSON for use in the context of this app
-- Not intended to be used as if it were a full implementation of the spec

type JsonObjectMap = M.Map String JsonValue

data JsonValue
  = JsonNumber Int
  | JsonString String
  | JsonBool Bool
  | JsonArray [JsonValue]
  | JsonObject JsonObjectMap
  deriving (Show, Eq)

writeJsonValue :: JsonValue -> String
writeJsonValue (JsonNumber n) = show n
writeJsonValue (JsonString s) = '\"' : s ++ "\""
writeJsonValue (JsonBool b) = if b then "true" else "false"
writeJsonValue (JsonArray []) = "[]"
writeJsonValue (JsonArray [a]) = '[' : writeJsonValue a ++ "]"
writeJsonValue (JsonArray a) = '[' : writeJsonValue (head a) 
  ++ foldl' (\acc x -> 
    acc 
    ++ "," 
    ++ writeJsonValue x) "" (tail a) 
  ++ "]"
writeJsonValue (JsonObject o) = case M.toList o of
  [] -> "{}"
  [(k, v)] -> '{' : "\"" 
    ++ k 
    ++ "\":" 
    ++ writeJsonValue v 
    ++ "}"
  m ->  let (k, v) = head m 
        in '{' : "\"" 
          ++ k 
          ++ "\":" 
          ++ writeJsonValue v 
          ++ foldl' (\acc (k', v') -> 
            acc 
            ++ "," 
            ++ "\"" 
            ++ k' 
            ++ "\":" 
            ++ writeJsonValue v') "" (tail m) 
          ++ "}"

jsonEscape :: Parser String
jsonEscape = (\_ c -> '\\' : [c]) <$> char '\\' <*> item

jsonString :: Parser String
jsonString = (\_ s _ -> s) 
  <$> char '"' 
  <*> (concat <$> many (jsonEscape <|> some (sat "Unexpected end of string/escape character" (liftA2 (&&) (/='"') (/='\\')))))
  <*> char '"'

jsonBool :: Parser Bool
jsonBool = (== "true") <$> (string "true" <|> string "false")

jsonInt :: Parser Int
jsonInt = do
  sign <- optional $ char '-'
  n <- number

  return (if isNothing sign then n else negate n)

jsonValue :: Parser JsonValue
jsonValue = (JsonString <$> jsonString)
  <|> (JsonBool <$> jsonBool)
  <|> (JsonNumber <$> jsonInt)
  <|> (JsonArray <$> jsonArray)
  <|> (JsonObject <$> jsonObject)

jsonArray :: Parser [JsonValue]
jsonArray = do
  _ <- char '['
  _ <- optional whitespace
  inner <- many $ jsonValue
    <* optional (char ',')
    <* optional whitespace
  _ <- char ']'

  return inner

jsonKvPair :: Parser (String, JsonValue)
jsonKvPair = do
  k <- jsonString
  _ <- optional space
  _ <- char ':'
  _ <- optional space
  v <- jsonValue

  return (k, v)

jsonObject :: Parser JsonObjectMap
jsonObject = do
  _ <- char '{'
  _ <- optional whitespace
  inner <- many $ jsonKvPair
    <* optional whitespace
    <* optional (char ',')
    <* optional whitespace
  _ <- optional whitespace
  _ <- char '}'

  return $ M.fromList inner

isJsonObject :: JsonValue -> Bool
isJsonObject v = case v of
  JsonObject _ -> True
  _ -> False

isJsonArray :: JsonValue -> Bool
isJsonArray v = case v of
  JsonArray _ -> True
  _ -> False


class FromJson a where
  fromJson :: JsonValue -> Either String a

class ToJson a where
  toJson :: a -> JsonValue

instance ToJson String where
  toJson :: String -> JsonValue
  toJson = JsonString

instance ToJson Int where
  toJson :: Int -> JsonValue
  toJson = JsonNumber

instance ToJson Bool where
  toJson :: Bool -> JsonValue
  toJson = JsonBool

instance ToJson [String] where
  toJson :: [String] -> JsonValue
  toJson = JsonArray . map JsonString

instance ToJson (Int, Int) where
  toJson :: (Int, Int) -> JsonValue
  toJson (a, b) = JsonArray [JsonNumber a, JsonNumber b]

instance ToJson (String, String) where
  toJson :: (String, String) -> JsonValue
  toJson (a, b) = JsonArray [JsonString a, JsonString b]

class IsJson a where
  fromValue :: JsonValue -> Either String a

instance IsJson String where
  fromValue :: JsonValue -> Either String String
  fromValue (JsonString s) = Right s
  fromValue _ = Left "Expected JSON String"

instance IsJson Int where
  fromValue :: JsonValue -> Either String Int
  fromValue (JsonNumber n) = Right n
  fromValue _ = Left "Expected JSON Number"

instance IsJson Bool where
  fromValue :: JsonValue -> Either String Bool
  fromValue (JsonBool b) = Right b
  fromValue _ = Left "Expected JSON Bool"

instance IsJson [String] where
  fromValue :: JsonValue -> Either String [String]
  fromValue (JsonArray a) = mapM (\case
    (JsonString s) -> Right s
    _ -> Left "Expected JSON Array of JSON Strings") a
  fromValue _ = Left "Expected JSON Array"

instance IsJson (Int, Int) where
  fromValue :: JsonValue -> Either String (Int, Int)
  fromValue (JsonArray [JsonNumber a, JsonNumber b]) = Right (a, b)
  fromValue _ = Left "Expected JSON Array of two JSON Numbers"

instance IsJson (String, String) where
  fromValue :: JsonValue -> Either String (String, String)
  fromValue (JsonArray [JsonString a, JsonString b]) = Right (a, b)
  fromValue _ = Left "Expected JSON Array of two JSON Strings"

instance IsJson JsonValue where
  fromValue :: JsonValue -> Either String JsonValue
  fromValue = Right

getField :: IsJson a => String -> JsonObjectMap -> Either String a
-- getField k o = fromValue =<< M.lookup k o
getField k o = case M.lookup k o of
  Just v -> case fromValue v of
    Right a -> Right a
    Left e -> Left e
  Nothing -> Left $ "JSON error: Key '" <> k <> "' does not exist in object"
