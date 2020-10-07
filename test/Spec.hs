import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit
import           Control.Monad
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Base16        as H
import qualified Crypto.Hash.SHA256            as S
import qualified Crypto.Hash.BLAKE2.BLAKE2b    as B
import qualified Crypto.Hash.Keccak            as K
import           Crypto.LongShot.Internal

main :: IO ()
main = defaultMain tests

-- | Test for strict mode of Brute-force search 
-- The key length was limited to 4 as it takes a long time to test
testLongshot :: (C.ByteString -> C.ByteString) -> Gen Bool
testLongshot hasher = do
  let chars = "0123456789abcdef~!@#$%*()<>;:?/"
  size <- choose (1, 4) :: Gen Int
  key  <- replicateM size (elements chars :: Gen Char)
  let hex   = C.unpack . H.encode . hasher . C.pack $ key
  let found = bruteforce size chars hex hasher
  case found of
    Just x | x == key -> return True
    _                 -> return False

-- | Test for deep mode of Brute-force search 
-- The key length was limited to 4 as it takes a long time to test
testLongshotDeep :: (C.ByteString -> C.ByteString) -> Gen Bool
testLongshotDeep hasher = do
  let chars = "0123456789abcdef~!@#$%*)[]}>?;:"
  let size  = 4
  size' <- choose (1, 4) :: Gen Int
  key   <- replicateM size' (elements chars :: Gen Char)
  let hex   = C.unpack . H.encode . hasher . C.pack $ key
  let found = bruteforceDeep size chars hex hasher
  case found of
    Just x | x == key -> return True
    _                 -> return False


tests :: TestTree
tests = testGroup
  "LongShot Tests"
  [ testProperty "testLongShot with SHA256" $ testLongshot S.hash
  , testProperty "testLongShot with Blake2b" $ testLongshot (B.hash 32 mempty)
--   , testProperty "testLongShot with Keccak256" $ testLongshot K.keccak256
  ]
