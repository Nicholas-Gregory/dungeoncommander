module Main where

import System.Environment (getArgs)
import DC.Opts (command)
import Network.Socket
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (sendAll)
import Control.Applicative (optional)
import System.IO (hReady, stdin)

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  sock <- socket AF_UNIX Stream defaultProtocol
  ready <- hReady stdin

  connect sock (SockAddrUnix "/tmp/dc.sock")

  if ready
    then do
      input <- getContents
      putStrLn ("Working with: " ++ input)
    else do
      putStrLn "Requesting data from server"
      sendAll sock (C.pack "REQUEST")
