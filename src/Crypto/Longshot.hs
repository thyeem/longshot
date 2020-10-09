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
module Crypto.Longshot
  ( defChars
  , defSearchLength
  , defNumPrefix
  , defNumBind
  , image
  , byteChars
  , bytePrefixes
  , toKey
  )
where

import           Control.Monad
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Base16        as H

-- | Characters available in a preimage
defChars :: String
defChars = "0123456789"

-- | Search length of preimage
defSearchLength :: Int
defSearchLength = 8

-- | Limit search length of preimage
limitSearchLength :: Int
limitSearchLength = 20

-- | Value related to the number of sparks
defNumPrefix :: Int
defNumPrefix = 3

-- | Number of actions in TH bruteforceN
defNumBind :: Int
defNumBind = limitSearchLength - defNumPrefix
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
