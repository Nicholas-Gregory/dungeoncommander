module DC.Actions (
 filterByKey
) where
import DC.Parse (Parser (runParser))
import DC.Json (writeJsonValue, jsonObject)
import Data.Map ((!?))


finalParse :: Parser a -> String -> a
finalParse parser input = case runParser parser input of
  Just (v, "") -> v
  Just (_, rest) -> error ("Leftovers: " ++ rest)
  Nothing -> error "Parsing error!"

filterByKey :: String -> String -> Maybe String
filterByKey json key = do
  let m = finalParse jsonObject json
  let v = m !? key

  writeJsonValue <$> v 
  

  