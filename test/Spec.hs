import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit
import           Control.Monad
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Base16        as H
import           Crypto.Longshot.Internal
import           Crypto.Longshot.Hasher

main :: IO ()
main = defaultMain tests

-- | Search Length limit
-- The key length was limited to 4 as it takes a long time to test
limit :: Int
limit = 4

-- | Characters prepared for test
chars :: String
chars = "0123456789abcdef~'*+-%^?[]/"

-- | Hash algorithms available
hashers :: [String]
hashers =
  [ "md5"
  , "sha1"
  , "ripemd160"
  , "whirlpool"
  , "sha256"
  , "sha3_256"
  , "sha3_384"
  , "sha3_512"
  , "blake2s_256"
  , "blake2b_256"
  , "blake2b_384"
  , "blake2b_512"
  , "blake3_256"
  , "blake3_384"
  , "blake3_512"
  , "blake3bp"
  , "keccak_256"
  , "keccak_384"
  , "keccak_512"
  , "skein_256"
  , "skein_384"
  , "skein_512"
  ]

-- | Test for strict mode of Brute-force search
testLongshot :: Hasher -> Gen Bool
testLongshot hasher = do
  size <- choose (1, limit) :: Gen Int
  key  <- replicateM size (elements chars :: Gen Char)
  let hex   = C.unpack . H.encode . hasher . C.pack $ key
  let found = bruteforce size chars hex hasher
  case found of
    Just x | x == key -> return True
    _                 -> return False

-- | Test for deep mode of Brute-force search
testLongshotDeep :: Hasher -> Gen Bool
testLongshotDeep hasher = do
  let size = limit
  size' <- choose (1, limit) :: Gen Int
  key   <- replicateM size' (elements chars :: Gen Char)
  let hex   = C.unpack . H.encode . hasher . C.pack $ key
  let found = bruteforceDeep size chars hex hasher
  case found of
    Just x | x == key -> return True
    _                 -> return False

-- | Property test generator
genTestProp :: (Hasher -> Gen Bool) -> TestName -> TestName -> TestTree
genTestProp f desc algo = testProperty (desc <> algo) (f $ getHasher algo)

-- | Main test of properties
tests :: TestTree
tests = testGroup
  "Longshot Tests"
  [ testGroup "Strict-Longshot search"
              (genTestProp testLongshot "testLongshot with " <$> hashers)
  , testGroup
    "Deep-Longshot search"
    (genTestProp testLongshotDeep "testLongshotDeep with " <$> hashers)
  ]
