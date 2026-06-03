module Main where

import Network.Socket
import Control.Monad (forever)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import System.IO (readFile)
import Control.Concurrent (forkFinally)

handleClient :: Socket -> IO ()
handleClient conn = do
  msg <- recv conn 4096
  case C.unpack msg of
    "REQUEST" -> do
      contents <- C.readFile "db.json"
      sendAll conn contents
    _ -> return ()

main :: IO ()
main = withSocketsDo $ do
  sock <- socket AF_UNIX Stream 0

  bind sock (SockAddrUnix "/tmp/dc.sock")
  listen sock 1

  
  forever $ do
    (conn, _) <- accept sock
    forkFinally (handleClient conn) (\_ -> close conn)