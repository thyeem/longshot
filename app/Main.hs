module Main where

import           Control.Monad
import           Control.DeepSeq
import           Control.Parallel               ( par
                                                , pseq
                                                )
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Base16        as H
import           Data.List.Split                ( chunksOf )
import qualified Crypto.Hash.SHA256            as S


-- | Number of chunks you want
pieces :: Int
pieces = 100000

-- | Preimage solution
key :: String
key = "12345678"

-- | Characters to be used in a preimage
chars :: String
chars = ['0' .. '9']

-- Search space will be tremendously increased when using like:
-- chars = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']

-- | Split the whole search spaces by number of pieces
chunks :: [[Integer]]
chunks = chunksOf pieces [1 .. 10 ^ length key]

-- | Image: hash value to find
image :: C.ByteString
image = S.hash . C.pack $ key

-- | Brute Force attack
bruteforce :: C.ByteString -> [Integer] -> Bool
bruteforce image searchSpace =
  or ((image ==) . S.hash . C.pack . show <$> searchSpace)

-- | Parallel map using deepseq, par and pseq
pmap :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
pmap f []       = []
pmap f (x : xs) = y `par` ys `pseq` (y : ys) where
  y  = force $ f x
  ys = pmap f xs

main :: IO ()
main
  | or $ bruteforce image `pmap` chunks
  = putStrLn $ "Found: " <> (C.unpack . H.encode $ image)
  | otherwise
  = putStrLn "Not Found"
