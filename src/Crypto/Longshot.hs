-- |
-- Module      : Crypto.Longshot
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
-- Fast Brute-force search using parallelism
--
-- Longshot enables to search for preimages from a given hash value
-- using a brute-force method based on parallelism.
module Crypto.Longshot
  ( module Crypto.Longshot.Const
  , module Crypto.Longshot.Internal
  , module Crypto.Longshot.TH
  , module Crypto.Longshot.Hasher
  ) where

import           Crypto.Longshot.Const
import           Crypto.Longshot.Hasher
import           Crypto.Longshot.Internal
import           Crypto.Longshot.TH
