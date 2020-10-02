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


$( funcGenerator )

bruteforce :: IO ()
bruteforce = case found of
  Just x -> putStrLn $ "Found: " <> C.unpack x
  _      -> putStrLn "Not found"
  where found = foldl (<|>) empty (id <%> $( funcListPar ))

-- | Parallel map using deepseq, par and pseq
(<%>) :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
f <%> []       = []
f <%> (x : xs) = y `par` ys `pseq` (y : ys) where
  y  = force $ f x
  ys = f <%> xs


