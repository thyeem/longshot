{-# LANGUAGE QuasiQuotes #-}
module Main where

import           Control.Monad
import           System.Environment
import           System.Console.Docopt
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Base16        as H
import qualified Crypto.Hash.SHA256            as S
import qualified Crypto.Hash.BLAKE2.BLAKE2b    as B
import qualified Crypto.Hash.Keccak            as K
import           Crypto.BruteForce
import           Crypto.BruteForce.Internal

patterns :: Docopt
patterns = [docopt|
bruteforce - Fast and concise Brute-force search

Usage:
  bruteforce run        [-n SIZE] [-c CHARS] [-a HASHER] [--deep] HEX
  bruteforce image      [-a HASHER] KEY

Commands:
  run                   Brute-force search with given hexstring and options
  image                 Generate image from given key string and hash algorithm

Arguments:
  HEX                   Specify target hexstring to search
  KEY                   Specify key string as a preimage

Options:
  -h --help             Show this
  -n SIZE               Specify search length  [default: 8]   
  -c CHARS              Specify characters in preimage  [default: 0123456789]
  -a HASHER             Specify hash algorithm  [default: sha256]
                        Available HASHER: sha256 | blake2b | keccak256
  --deep                Search deeply including less than a given search length
|]

-- | Defines args-ops frequently used
(><) = isPresent
(<->|!) = getArgOrExitWith patterns
(<->) = getArg

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  when (args >< command "image") $ genImage args
  when (args >< command "run") $ run args

-- | Command: genImage
genImage :: Arguments -> IO ()
genImage args = do
  key    <- C.pack <$> (args <->|! argument "KEY")
  hasher <- getHasher <$> (args <->|! shortOption 'a')
  putStrLn . C.unpack . H.encode . hasher $ key

-- | Command: run
run :: Arguments -> IO ()
run args = do
  hex    <- args <->|! argument "HEX"
  chars  <- args <->|! shortOption 'c'
  size   <- (read <$> (args <->|! shortOption 'n')) :: IO Int
  hasher <- getHasher <$> (args <->|! shortOption 'a')
  let solver | args >< longOption "deep" = bruteforceDeep
             | otherwise                 = bruteforce
  let found = solver size chars hex hasher
  case found of
    Just key -> putStrLn $ "Found  " <> key
    _        -> putStrLn "Not found"

-- | Select hasher by name
getHasher :: String -> (C.ByteString -> C.ByteString)
getHasher label = case label of
  "sha256"    -> S.hash
  "blake2b"   -> B.hash 32 mempty
  "keccak256" -> K.keccak256
  a           -> errorWithoutStackTrace $ "Not allowed hash algorithm: " <> a
