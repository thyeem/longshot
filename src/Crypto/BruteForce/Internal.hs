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

-- Declaration of bruteforcePar: generating code by splicing
$( funcGenerator )

-- | Brute-force search
bruteforce :: Maybe String
bruteforce = case found of
  Just x -> Just $ C.unpack x
  _      -> Nothing
  where found = foldl (<|>) empty (bruteforcePar <%> prefixes)

-- | Parallel map using deepseq, par and pseq
(<%>) :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
f <%> []       = []
f <%> (x : xs) = y `par` ys `pseq` (y : ys) where
  y  = force $ f x
  ys = f <%> xs
