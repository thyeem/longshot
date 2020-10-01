module Main where

import qualified Data.ByteString.Char8         as C
import           BruteForce
import           Control.Monad
import           Control.Applicative
import           Language.Haskell.TH
import qualified Crypto.Hash.SHA256            as S
import qualified Data.ByteString.Base16        as H

-- | Solve a given problem
-- main :: IO ()
-- main = case found of
--   Just x -> putStrLn $ "Found: " <> show x
--   _      -> putStrLn "Not found"
--   where found = foldl (<|>) empty $ bruteforce <%> [0 .. chunks]

main :: IO ()
main = case found of
  Just x -> putStrLn $ "Found: " <> C.unpack x
  _      -> putStrLn "Not found"
  where found = $(bruteforceN limit)  -- <%> byteChars
