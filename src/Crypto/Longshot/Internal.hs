-- |
-- Module      : Crypto.Longshot.Internal
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
module Crypto.Longshot.Internal
  ( bruteforce
  , bruteforceDeep
  )
where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Parallel
import           Data.Foldable
import qualified Data.ByteString.Char8         as C
import           Language.Haskell.TH
import           Crypto.Longshot
import           Crypto.Longshot.TH
import           Crypto.Longshot.Hasher

-- | Each bruteforceN declaration: generating code through splicing
-- Number of functions declared == 'maxNumBind'
--
$( funcGenerator )

-- | Brute-force search only for a given exact length
--
--     size | preimage length to search
--    chars | given character set like "0123456789"
--      hex | given hex-string like "17da1ae431f965d839ec8eb93087fb2b"
--   hasher | hash functions defined in 'Hasher' module
--  numBind | number of bound variables defined by search length and prefix size
--   runPar | a partially applied function for parallel execution
-- prefixes | all possible combinations of given prefix characters.
--            the search space is equally partioned based on these prefixes.
--            length(prefixes) == number of sparks
--
bruteforce :: Int -> String -> String -> Hasher -> Maybe String
bruteforce size chars hex hasher = found
 where
  found  = foldl' (<|>) empty (runPar <%> prefixes)
  runPar = bruteforcePar numBind (byteChars chars) (image hex) hasher
  numPrefix | size < defNumPrefix = 1
            | otherwise           = defNumPrefix
  numBind  = size - numPrefix
  prefixes = bytePrefixes numPrefix chars

-- | Pick up an appropriate search function
-- Returns a partial application corresponding to the given 'numBind'
--
bruteforcePar
  :: Int
  -> [C.ByteString]
  -> C.ByteString
  -> Hasher
  -> C.ByteString
  -> Maybe String
bruteforcePar n
  | n `elem` [0 .. maxNumBind] = $( funcList ) !! n
  | otherwise = errorWithoutStackTrace "Not available search length"

-- | Deep Brute-force search including less than a given search size
-- See the 'bruteforce' function for the arguments used
--
bruteforceDeep :: Int -> String -> String -> Hasher -> Maybe String
bruteforceDeep size chars hex hasher = foldl' (<|>) empty found
 where
  found = deep chars hex hasher <%> [1 .. size]
  deep a b c d = bruteforce d a b c

-- | Parallel map using deepseq, par and pseq
-- Type of any argument in this map should be instance of 'NFData'
--
(<%>) :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
f <%> []       = []
f <%> (x : xs) = y `par` ys `pseq` (y : ys) where
  y  = force $ f x
  ys = f <%> xs
