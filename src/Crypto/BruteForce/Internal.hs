module Crypto.BruteForce.Internal where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Parallel               ( par
                                                , pseq
                                                )
import qualified Data.ByteString.Char8         as C
import           Language.Haskell.TH
import           Crypto.BruteForce
import           Crypto.BruteForce.TH

-- Declaration of bruteforceN: generating code by splicing
$( funcGenerator )

-- | Brute-force search
bruteforce
  :: Int
  -> [C.ByteString]
  -> C.ByteString
  -> (C.ByteString -> C.ByteString)
  -> [C.ByteString]
  -> Maybe String
bruteforce numBind chars hex hasher prefixes = found
 where
  found  = foldl (<|>) empty (runPar <%> prefixes)
  runPar = bruteforcePar numBind chars hex hasher

bruteforcePar
  :: Int
  -> [C.ByteString]
  -> C.ByteString
  -> (C.ByteString -> C.ByteString)
  -> C.ByteString
  -> Maybe String
bruteforcePar n
  | n < 0 = head $( funcList )
  | n `elem` [0 .. defNumBind] = $( funcList ) !! n
  | otherwise = errorWithoutStackTrace "Not available search length"

-- | Parallel map using deepseq, par and pseq
(<%>) :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
f <%> []       = []
f <%> (x : xs) = y `par` ys `pseq` (y : ys) where
  y  = force $ f x
  ys = f <%> xs
