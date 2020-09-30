module BruteForce where

import           Control.DeepSeq
import           Control.Parallel               ( par
                                                , pseq
                                                )
import           Data.List                      ( find )
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Base16        as H
import qualified Crypto.Hash.SHA256            as S
import qualified Crypto.Hash.BLAKE2.BLAKE2b    as B2b

-- Here assume that only characters below to be used in a preimage
-- ['0' .. '9']
-- Search space will be tremendously increased when using like:
-- ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']

-- | Hex-string image
hex :: String
-- hex = "ef797c8118f02dfb649607dd5d3f8c7623048c9c063d532cc95c5ed7a898a64f" -- 12345678
-- hex = "c5460c4a38f89b6f4cf36b4c85590f25ad6ee25f01f03dca98d43d84da56e8da" -- 50000000
hex = "01c02776d7290e999c60af8413927df1d389690aab8cac12503066cf62e899f6" -- 55555555
-- hex = "3f08d8fadb4b67fb056623565edbbc2c788091d78fd24cbc473fce3043ce3473" -- 99999999

-- | Length of preimage (limit of the search space)
limit :: Int
limit = 8

-------------------------------------------------------------------
-- | Number of chunks
-- How many search zones would you have? 
chunks :: Integer
chunks = round . sqrt $ 10 ^ limit

-- | Chunk size 
unit :: Integer
unit = fromIntegral (10 ^ limit) `div` chunks

-- | Image bytestring: hash value to find
image :: C.ByteString
image = fst . H.decode . C.pack $ hex

-- | Brute-force attack
bruteforce :: Integer -> Maybe Integer
bruteforce index = find ((== image) . S.hash . C.pack . show) domain
  where domain = [unit * index .. unit * succ index]

-- | Parallel map using deepseq, par and pseq
(<%>) :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
f <%> []       = []
f <%> (x : xs) = y `par` ys `pseq` (y : ys) where
  y  = force $ f x
  ys = f <%> xs
