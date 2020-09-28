module Main where

import           Control.Monad
import           Control.DeepSeq
import           Control.Parallel               ( par
                                                , pseq
                                                )
import           Data.Word                      ( Word8 )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Base16        as H
import           Data.Char                      ( ord )
import           Data.List.Split                ( chunksOf )
import           Crypto.Hash.SHA256             ( hash )


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

-- | Split the whole search spaces by number of processor
chunks :: [[Integer]]
chunks = chunksOf pieces [1 .. 10 ^ length key]

byteChars :: [Word8]
byteChars = fromIntegral . ord <$> chars

-- | Image: hash value to find
image :: C.ByteString
image = hash . C.pack $ key

-- | Brute Force attack
bruteforce :: C.ByteString -> [Integer] -> Bool
bruteforce image searchSpace =
  or ((image ==) . hash . C.pack . show <$> searchSpace)

-- | Parallel map using deepseq, par and pseq
map' :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
map' f []       = []
map' f (x : xs) = y `par` ys `pseq` (y : ys) where
  y  = force $ f x
  ys = map' f xs

infixl 4 <%>
-- | Infix map'
(<%>) :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
(<%>) = map'

main :: IO ()
main
  | or $ bruteforce image <%> chunks
  = putStrLn $ "Found: " <> (C.unpack . H.encode $ image)
  | otherwise
  = putStrLn "Not Found"
