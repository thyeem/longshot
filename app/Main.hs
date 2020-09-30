module Main where

import           Control.DeepSeq
import           Control.Parallel               ( par
                                                , pseq
                                                )
import           Data.List                      ( find )
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( catMaybes )
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Base16        as H
import qualified Crypto.Hash.SHA256            as S

-- Here assume that only characters below to be used in a preimage
-- ['0' .. '9']
-- Search space will be tremendously increased when using like:
-- ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']

-- | Preimage solution
key :: String
key = "12345678"

-------------------------------------------------------------------
-- | Number of chunks
-- How many search zones would you have? 
chunks :: Integer
chunks = round . sqrt $ 10 ^ length key

-- | Size of each step
unit :: Integer
unit = fromIntegral (10 ^ length key) `div` chunks

-- | Image: hash value to find
image :: C.ByteString
image = S.hash . C.pack $ key

-- | Brute Force attack
bruteforce :: Integer -> Maybe C.ByteString
bruteforce index = find (== image) $ S.hash . C.pack . show <$> domain
  where domain = [unit * index .. unit * succ index]

-- | Solve the problem
main :: IO ()
main | null found = putStrLn "Not Found"
     | otherwise  = putStrLn $ "Found: " <> (C.unpack . H.encode $ head found)
  where found = catMaybes $ bruteforce <%> [0 .. chunks]

-- | Parallel map using deepseq, par and pseq
(<%>) :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
f <%> []       = []
f <%> (x : xs) = y `par` ys `pseq` (y : ys) where
  y  = force $ f x
  ys = f <%> xs
