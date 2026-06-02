module Main where

import Network.Socket
import Control.Monad (forever)
import Network.Socket.ByteString (recv)

main :: IO a
main = withSocketsDo $ do
  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix "/tmp/dc.sock")
  listen sock 1

  forever $ do
    (conn, _) <- accept sock
    msg <- recv conn 1024
    putStrLn $ "Server recieved: " ++ show msg ++ " from client."