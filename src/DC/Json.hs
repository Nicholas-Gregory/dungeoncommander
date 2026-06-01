module DC.Json (
  jsonString,
  jsonBool,
  jsonArray,
  jsonObject,
  writeJsonValue,
  JsonValue(..)
) where
import DC.Parse (Parser, item, char, sat, string, whitespace, number, space)
import Control.Applicative (Alternative((<|>), many, some), optional)
import Data.Map (Map, fromList, toList)
import Data.Maybe (isNothing)
import Data.Foldable (foldl')


-- This is only a subset of JSON for use in the context of this app
-- Not intended to be used as if it were a full implementation of the spec

type JsonObjectMap = Map String JsonValue

data JsonValue
  = JsonNumber Int
  | JsonString String
  | JsonBool Bool
  | JsonArray [JsonValue]
  | JsonObject JsonObjectMap
  deriving (Show)

writeJsonValue :: JsonValue -> String
writeJsonValue (JsonNumber n) = show n
writeJsonValue (JsonString s) = '\"' : s ++ "\""
writeJsonValue (JsonBool b) = if b then "true" else "false"
writeJsonValue (JsonArray []) = "[]"
writeJsonValue (JsonArray [a]) = '[' : writeJsonValue a ++ "]"
writeJsonValue (JsonArray a) = '[' : writeJsonValue (head a) ++ foldl' (\acc x -> acc ++ "," ++ writeJsonValue x) "" (tail a) ++ "]"
writeJsonValue (JsonObject o) = case toList o of
  [] -> "{}"
  [(k, v)] -> '{' : "\"" ++ k ++ "\":" ++ writeJsonValue v ++ "}"
  m ->  let (k, v) = head m 
        in '{' : "\"" ++ k ++ "\":" ++ writeJsonValue v ++ foldl' (\acc (k', v') -> acc ++ "," ++ "\"" ++ k' ++ "\":" ++ writeJsonValue v') "" (tail m) ++ "}"

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
    <* optional space
    <* optional (char ',')
    <* optional space
  _ <- optional whitespace
  _ <- char '}'

  return $ fromList inner
  
  

  