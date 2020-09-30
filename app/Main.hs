module Main where

import           BruteForce
import           Control.Applicative
import qualified Data.ByteString.Char8         as C

-- | Solve a given problem
-- main :: IO ()
-- main = case found of
--   Just x -> putStrLn $ "Found: " <> show x
--   _      -> putStrLn "Not found"
--   where found = foldl (<|>) empty $ bruteforce <%> [0 .. chunks]

main :: IO ()
main | null found = putStrLn "Not found"
     | otherwise  = putStrLn $ "Found: " <> (C.unpack . head $ found)
  where found = foldl (<|>) empty $ bruteforce' <%> byteChars
