module Main where

import System.Environment (getArgs)
import DC.Opts (command)
import Network.Socket
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (sendAll)

main :: IO ()
main = withSocketsDo $ do
  (cmd:_) <- getArgs
  sock <- socket AF_UNIX Stream defaultProtocol

  connect sock (SockAddrUnix "/tmp/dc.sock")

  sendAll sock (C.pack cmd)