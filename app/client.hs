module Main where

import System.Environment (getArgs)
import DC.Opts (command)
import Network.Socket
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (sendAll, recv)
import Control.Applicative (optional)
import System.IO (hReady, stdin)
import Control.Monad (join)
import DC.Parse (Parser(runParser))
import DC.Json (jsonObject)

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  sock <- socket AF_UNIX Stream defaultProtocol
  ready <- hReady stdin
  input <- if not ready 
    then do
      print "here"
      connect sock (SockAddrUnix "/tmp/dc.sock")
      sendAll sock $ C.pack "REQUEST"
      r <- recv sock 4096
      return (C.unpack r)
    else getContents


  let parse = runParser jsonObject input

  print parse

                  
