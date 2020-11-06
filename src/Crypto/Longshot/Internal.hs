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
  , bruteforcePar
  , (<%>)
  , image
  , byteChars
  , bytePrefixes
  )
where

import           Control.Monad                  ( replicateM )
import           Control.Applicative            ( (<|>)
                                                , empty
                                                )
import           Control.Parallel               ( par
                                                , pseq
                                                )
import           Control.DeepSeq                ( NFData
                                                , force
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Base16        as H
import           Crypto.Longshot.Hasher
import           Crypto.Longshot.TH
import           Crypto.Longshot.Const

-- | Each bruteforceN declaration: generating code through splicing.
-- Number of functions declared == 'maxNumBind'
--
$( funcGenerator )

-- | Brute-force search only for a given exact length
--
-- @
-- +----------+----------------------------------------------------------------------+
-- |     size | Preimage length to search                                            |
-- +----------+----------------------------------------------------------------------+
-- |    chars | Given character set like "0123456789"                                |
-- +----------+----------------------------------------------------------------------+
-- |      hex | Given hex-string like "17da1ae431f965d839ec8eb93087fb2b"             |
-- +----------+----------------------------------------------------------------------+
-- |   hasher | Hash functions in 'Hasher' module. Get it using 'getHasher'          |
-- +----------+----------------------------------------------------------------------+
-- |  numBind | Number of bound variables defined by search length and prefix size   |
-- +----------+----------------------------------------------------------------------+
-- |   runPar | A partially applied function for parallel execution                  |
-- +----------+----------------------------------------------------------------------+
-- | prefixes | All possible combinations of given prefix characters.                |
-- |          | The search space is equally partitioned based on these prefixes.     |
-- |          | length of prefixes == number of sparks                               |
-- +----------+----------------------------------------------------------------------+
-- @
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
--
-- Returns a partial application corresponding to the given numBind
--
bruteforcePar
  :: Int -> [ByteString] -> ByteString -> Hasher -> ByteString -> Maybe String
bruteforcePar numBind
  | numBind `elem` [0 .. maxNumBind] = $( funcList ) !! numBind
  | otherwise = errorWithoutStackTrace "Not available search length"

-- | Incrementally searches without knowing the exact length of search
--
-- See the 'bruteforce' function for the arguments used
--
bruteforceDeep :: String -> String -> Hasher -> Maybe String
bruteforceDeep chars hex hasher = foldl' (<|>) empty found
 where
  found = deep chars hex hasher <$> [1 .. limitSearchLength]
  deep a b c d = bruteforce d a b c

-- | Parallel map using deepseq, par and pseq
--
-- Type of any argument in this map should be an instance of 'NFData'.
--
(<%>) :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
f <%> []       = []
f <%> (x : xs) = y `par` ys `pseq` (y : ys) where
  y  = force $ f x
  ys = f <%> xs

-- | Image bytestring: target hash value to find
image :: String -> ByteString
image = fst . H.decode . C.pack

-- | Bytestring usable for preimage
byteChars :: String -> [ByteString]
byteChars chars = C.pack . (: []) <$> chars

-- | Combination of prefixes possible: size of @(length of chars) ^ (numPrefix)@
bytePrefixes :: Int -> String -> [ByteString]
bytePrefixes numPrefix chars = C.pack <$> replicateM numPrefix chars
