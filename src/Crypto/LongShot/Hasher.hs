module Crypto.LongShot.Hasher
  ( Hasher
  , getHasher
  )
where

import           GHC.TypeLits
import           Numeric.Natural
import qualified Crypto.Hash.SHA256            as S
import qualified Data.ByteArray                as BA
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C
import qualified BLAKE3                        as B3
import qualified Crypto.Hash                   as X
import           Crypto.Hash                    ( hashWith
                                                , Digest
                                                , MD5(..)
                                                , SHA1(..)
                                                , RIPEMD160(..)
                                                , SHA256(..)
                                                , SHA3_256(..)
                                                , SHA3_384(..)
                                                , SHA3_512(..)
                                                , Blake2b_256(..)
                                                , Blake2b_384(..)
                                                , Blake2b_512(..)
                                                , Blake2bp_512(..)
                                                , Keccak_256(..)
                                                , Keccak_384(..)
                                                , Keccak_512(..)
                                                , Skein256_256(..)
                                                , Skein512_384(..)
                                                , Skein512_512(..)
                                                , Whirlpool(..)
                                                )

type Hasher = C.ByteString -> C.ByteString

type Blake3_256 = C.ByteString -> B3.Digest (32 :: Nat)
type Blake3_384 = C.ByteString -> B3.Digest (48 :: Nat)
type Blake3_512 = C.ByteString -> B3.Digest (64 :: Nat)

-- | Select hasher by name
getHasher :: String -> Hasher
getHasher name = case name of
  "md5"         -> BA.convert . hashWith MD5
  "sha1"        -> BA.convert . hashWith SHA1
  "ripemd160"   -> BA.convert . hashWith RIPEMD160
  "sha256"      -> S.hash
  "sha3_256"    -> BA.convert . hashWith SHA3_256
  "sha3_384"    -> BA.convert . hashWith SHA3_384
  "sha3_512"    -> BA.convert . hashWith SHA3_512
  "blake2b_256" -> BA.convert . hashWith Blake2b_256
  "blake2b_384" -> BA.convert . hashWith Blake2b_384
  "blake2b_512" -> BA.convert . hashWith Blake2b_512
  "blake2bp"    -> BA.convert . hashWith Blake2bp_512
  "blake3_256"  -> BA.convert . (B3.hash . (: []) :: Blake3_256)
  "blake3_384"  -> BA.convert . (B3.hash . (: []) :: Blake3_384)
  "blake3_512"  -> BA.convert . (B3.hash . (: []) :: Blake3_512)
  "keccak_256"  -> BA.convert . hashWith Keccak_256
  "keccak_384"  -> BA.convert . hashWith Keccak_384
  "keccak_512"  -> BA.convert . hashWith Keccak_512
  "skein_256"   -> BA.convert . hashWith Skein256_256
  "skein_384"   -> BA.convert . hashWith Skein512_384
  "skein_512"   -> BA.convert . hashWith Skein512_512
  "whirlpool"   -> BA.convert . hashWith Whirlpool
  a             -> errorWithoutStackTrace $ "Not allowed hash algorithm  " <> a
