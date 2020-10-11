-- |
-- Module      : Crypto.Longshot.Hasher
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
module Crypto.Longshot.Hasher
  ( Hasher
  , getHasher
  )
where

import           GHC.TypeLits                   ( Nat )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteArray                as B
import qualified Crypto.Hash                   as X
import qualified Crypto.Hash.SHA256            as S
import qualified Crypto.Hash.BLAKE2.BLAKE2s    as Blake2s
import qualified Crypto.Hash.BLAKE2.BLAKE2b    as Blake2b
import qualified BLAKE3                        as Blake3

-- | Type for hash functions available
type Hasher = ByteString -> ByteString

type Blake3_256 = ByteString -> Blake3.Digest (32 :: Nat)
type Blake3_384 = ByteString -> Blake3.Digest (48 :: Nat)
type Blake3_512 = ByteString -> Blake3.Digest (64 :: Nat)

-- | Select hasher by defined name
getHasher :: String -> Hasher
getHasher name = case name of
  "md5"         -> B.convert . X.hashWith X.MD5
  "sha1"        -> B.convert . X.hashWith X.SHA1
  "ripemd160"   -> B.convert . X.hashWith X.RIPEMD160
  "whirlpool"   -> B.convert . X.hashWith X.Whirlpool
  "sha256"      -> S.hash
  "sha3_256"    -> B.convert . X.hashWith X.SHA3_256
  "sha3_384"    -> B.convert . X.hashWith X.SHA3_384
  "sha3_512"    -> B.convert . X.hashWith X.SHA3_512
  "blake2s_256" -> Blake2s.hash 32 mempty
  "blake2b_256" -> Blake2b.hash 32 mempty
  "blake2b_384" -> Blake2b.hash 48 mempty
  "blake2b_512" -> Blake2b.hash 64 mempty
  "blake3_256"  -> B.convert . (Blake3.hash . (: []) :: Blake3_256)
  "blake3_384"  -> B.convert . (Blake3.hash . (: []) :: Blake3_384)
  "blake3_512"  -> B.convert . (Blake3.hash . (: []) :: Blake3_512)
  "keccak_256"  -> B.convert . X.hashWith X.Keccak_256
  "keccak_384"  -> B.convert . X.hashWith X.Keccak_384
  "keccak_512"  -> B.convert . X.hashWith X.Keccak_512
  "skein_256"   -> B.convert . X.hashWith X.Skein256_256
  "skein_384"   -> B.convert . X.hashWith X.Skein512_384
  "skein_512"   -> B.convert . X.hashWith X.Skein512_512
  a             -> errorWithoutStackTrace $ "Not allowed hash algorithm  " <> a
