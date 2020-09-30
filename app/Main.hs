module Main where

import           BruteForce
import           Data.Maybe                     ( catMaybes )

-- | Solve a given problem
main :: IO ()
main | null found = putStrLn "Not found"
     | otherwise  = putStrLn $ "Found: " <> (show . head $ found)
  where found = catMaybes $ bruteforce <%> [0 .. chunks]
