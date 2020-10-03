module Crypto.BruteForce where

import           Control.Monad
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Base16        as H

-- | Setting up search space --------------------------------------
-- How big is the search space?
-- Not just (width x length), rather (length ^ width).
-- -> (preimage size) ^ (the number of characters available)

-- | Width of the search space: num of characters to be available in a preimage
chars :: String
chars = "0123456789"

-- | Length of the search space: limit of preimage size
limit :: Int
limit = 8

prefixN :: Int
prefixN = 2

prefixes :: [String]
prefixes = replicateM prefixN chars
-------------------------------------------------------------------

-- | Bytestring derived from @chars@
byteChars :: [C.ByteString]
byteChars = C.pack . (: []) <$> chars

-- | Image bytestring: hash value to find
image :: C.ByteString
image = fst . H.decode . C.pack $ hex

-- | Hex-string image
hex :: String
-- hex = "ef797c8118f02dfb649607dd5d3f8c7623048c9c063d532cc95c5ed7a898a64f" -- 12345678
-- hex = "c5460c4a38f89b6f4cf36b4c85590f25ad6ee25f01f03dca98d43d84da56e8da" -- 50000000
-- hex = "01c02776d7290e999c60af8413927df1d389690aab8cac12503066cf62e899f6" -- 55555555
hex = "3f08d8fadb4b67fb056623565edbbc2c788091d78fd24cbc473fce3043ce3473" -- 99999999
-- hex = "7f96f92b17b4bf4eca1c4d38ad70f211ed9b05f60415276dcded0ff3d0cf9aea" -- 123$%^78
