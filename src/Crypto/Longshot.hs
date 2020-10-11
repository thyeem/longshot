-- |
-- Module      : Crypto.Longshot
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
-- How big is the search space? The space consists of two axes.
--
-- * X-axis: Number of characters available
-- * Y-axis: Search length of preimage to find
--
-- Note that it's proportional to @(X ^ Y)@ rather than @(X * Y)@
--
-- The values below are defined by default.
--
-- When not provided as options in CUI, the following values are used.
--
module Crypto.Longshot where

import           Control.Monad
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Base16        as H

-- | Default characters available in a preimage
defChars :: String
defChars = "0123456789"

-- | Default search length of preimage
defSearchLength :: Int
defSearchLength = 8

-- | Limit search length of preimage
limitSearchLength :: Int
limitSearchLength = 20

-- | Default value related to the number of sparks
defNumPrefix :: Int
defNumPrefix = 3

-- | Maximum number of actions in bruteforceN
maxNumBind :: Int
maxNumBind = limitSearchLength - defNumPrefix
--------------------------------------------------------------------

-- | Image bytestring: target hash value to find
image :: String -> C.ByteString
image = fst . H.decode . C.pack

-- | Bytestring usable for preimage
byteChars :: String -> [C.ByteString]
byteChars chars = C.pack . (: []) <$> chars

-- | Combination of prefixes possible: size of (length of chars) ^ (numPrefix)
bytePrefixes :: Int -> String -> [C.ByteString]
bytePrefixes numPrefix chars = C.pack <$> replicateM numPrefix chars

-- | Convert preimage found into key string
toKey :: C.ByteString -> String
toKey = C.unpack
