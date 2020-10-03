module Main where

import           Crypto.BruteForce.Internal

main :: IO ()
main = case bruteforce of
  Just x -> putStrLn $ "Found " <> x
  _      -> putStrLn "Not found"
