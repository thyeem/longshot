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

-- Declaration of bruteforceN: generating code by splicing
$( funcGenerator )

-- | Brute-force search only for a given exact length
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
bruteforcePar
  :: Int
  -> [C.ByteString]
  -> C.ByteString
  -> Hasher
  -> C.ByteString
  -> Maybe String
bruteforcePar n
  | n `elem` [0 .. defNumBind] = $( funcList ) !! n
  | otherwise = errorWithoutStackTrace "Not available search length"

-- | Deep Brute-force search including less than a given search size
bruteforceDeep :: Int -> String -> String -> Hasher -> Maybe String
bruteforceDeep size x y z = foldl' (<|>) empty found
 where
  found = deep x y z <%> [1 .. size]
  deep a b c d = bruteforce d a b c

-- | Parallel map using deepseq, par and pseq
(<%>) :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
f <%> []       = []
f <%> (x : xs) = y `par` ys `pseq` (y : ys) where
  y  = force $ f x
  ys = f <%> xs
