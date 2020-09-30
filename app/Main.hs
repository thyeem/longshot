module Main where

import           BruteForce
import           Control.Applicative

-- | Solve a given problem
main :: IO ()
main = case found of
  Just x -> putStrLn $ "Found: " <> show x
  _      -> putStrLn "Not found"
  where found = foldl (<|>) empty $ bruteforce <%> [0 .. chunks]
