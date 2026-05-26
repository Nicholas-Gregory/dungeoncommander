module Tests.Roll (hello) where

hello :: IO Bool
hello = do
  putStrLn "Hello from Tests.Roll!"
  return True
