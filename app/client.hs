module Client where
import System.Environment (getArgs)
import DC.Parse (Parser(runParser))
import DC.Opts (command)



main :: IO ()
main = do
  args <- getArgs
  