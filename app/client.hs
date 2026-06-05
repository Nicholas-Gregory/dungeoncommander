module Main where

import System.Environment (getArgs)
import DC.Opts (command, cliArg, Option (SubCommand, Arg))
import Network.Socket
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (sendAll, recv)
import Control.Applicative (optional)
import System.IO (hReady, stdin)
import Control.Monad (join)
import DC.Parse (Parser(runParser))
import DC.Json (jsonObject)
import System.Random (getStdGen)
import DC.Dice (processExpression)

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  sock <- socket AF_UNIX Stream defaultProtocol
  ready <- hReady stdin
  gen <- getStdGen
  input <- if not ready 
    then do
      connect sock (SockAddrUnix "/tmp/dc.sock")
      sendAll sock $ C.pack "{ \"action\": \"GET\" }"
      r <- recv sock 4096
      return (C.unpack r)
    else getContents


  let stateParse = runParser jsonObject input
  let optsParse = map fst <$> mapM (runParser cliArg) args

  case optsParse of
    Just ((Arg "roll"):[Arg expr]) -> print $ processExpression gen expr
    _ -> print "Usage: <subcommand> [options]"
                  
