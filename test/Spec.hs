import           Control.Monad
import           Crypto.Longshot.Hasher
import           Crypto.Longshot.Internal
import qualified Data.ByteString.Base16        as H
import qualified Data.ByteString.Char8         as C
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

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
  , "keccak_256"
  , "keccak_384"
  , "keccak_512"
  , "skein_256"
  , "skein_384"
  , "skein_512"
  ]

-- | Test for { strict | deep } mode of Brute-force search
testLongshot :: String -> String -> Gen Bool
testLongshot mode algo = do
  size <- choose (1, limit) :: Gen Int
  key  <- replicateM size (elements chars :: Gen Char)
  let hasher = getHasher algo
  let hex    = C.unpack . H.encode . hasher . C.pack $ key
  let solver | mode == "strict" = bruteforce size
             | otherwise        = bruteforceDeep
  let found = solver chars hex hasher
  case found of
    Just x | x == key -> return True
    _                 -> return False

-- | Property test generator
genTestProp
  :: (String -> String -> Gen Bool) -> String -> String -> String -> TestTree
genTestProp f mode desc algo = testProperty (desc <> algo) (f mode algo)

-- | Main test of properties
tests :: TestTree
tests = testGroup
  "Longshot Tests"
  [ testGroup
    "Strict-Longshot search"
    (genTestProp testLongshot "strict" "Strict longshot with " <$> hashers)
  , testGroup
    "Deep-Longshot search"
    (genTestProp testLongshot "deep" "Deep longshot with " <$> hashers)
  ]
