module DC.Json (
  jsonString
) where
import DC.Parse (Parser, item, char, sat)
import Control.Applicative (Alternative((<|>), many, some))

-- This is only a subset of JSON for use in the context of this app
-- Not intended to be used as if it were a full implementation of the spec

jsonEscape :: Parser String
jsonEscape = (\_ c -> '\\' : [c]) <$> char '\\' <*> item

jsonString :: Parser String
jsonString = (\_ s _ -> s) 
  <$> char '"' 
  <*> (concat <$> many (jsonEscape <|> some (sat (liftA2 (&&) (/='"') (/='\\')))))
  <*> char '"'