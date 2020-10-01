module BruteForce where

import           Control.Monad
import           Control.DeepSeq
import           Control.Parallel               ( par
                                                , pseq
                                                )
import           Data.List                      ( find )
import Control.Applicative
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Base16        as H
import qualified Crypto.Hash.SHA256            as S
import qualified Crypto.Hash.BLAKE2.BLAKE2b    as B2b
import qualified Data.Map                      as M
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

-- | Hex-string image
hex :: String
-- hex = "ef797c8118f02dfb649607dd5d3f8c7623048c9c063d532cc95c5ed7a898a64f" -- 12345678
hex = "c5460c4a38f89b6f4cf36b4c85590f25ad6ee25f01f03dca98d43d84da56e8da" -- 50000000
-- hex = "01c02776d7290e999c60af8413927df1d389690aab8cac12503066cf62e899f6" -- 55555555
-- hex = "3f08d8fadb4b67fb056623565edbbc2c788091d78fd24cbc473fce3043ce3473" -- 99999999

-- The area (width x length) of the search space 
--   = (the number of characters available) x (preimage size)

-- | Width of the search space: num of characters to be available in a preimage
chars :: String
chars = "0123456789"

-- | Length of the search space: limit of preimage size
limit :: Int
limit = 8

-------------------------------------------------------------------
-- | Image bytestring: hash value to find
image :: C.ByteString
image = fst . H.decode . C.pack $ hex

byteChars :: [C.ByteString]
byteChars = C.pack . (: []) <$> chars

-- | N-ary bruteforce meta program using TH
bruteforceN :: Int -> Q Exp
bruteforceN n = do
  xs <- replicateM n (newName "xs")
  let pts    = varP <$> xs
      stmts  = (`bindS` varE 'byteChars) <$> pts
      bytes' = foldl merge [| mempty |] (varE <$> xs)
      cond'  = condE [| $(hash' bytes') == $(varE 'image) |]
                     [| Just $(bytes') |]
                     [| Nothing |]
      stmts' = stmts <> [noBindS cond']
  [| foldl (<|>) empty $(compE stmts') |]
 where
  merge a b = [| $a <> $b |]
  hash' = appE (varE 'S.hash)

-- | Parallel map using deepseq, par and pseq
(<%>) :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
f <%> []       = []
f <%> (x : xs) = y `par` ys `pseq` (y : ys) where
  y  = force $ f x
  ys = f <%> xs
