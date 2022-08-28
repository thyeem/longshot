{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad                  ( when )
import           Control.Monad.Extra            ( unlessM )
import           Crypto.Longshot
import qualified Data.ByteString.Base16        as H
import qualified Data.ByteString.Char8         as C
import           Data.Version                   ( showVersion )
import           Paths_longshot                 ( version )
import           System.Console.CmdArgs  hiding ( args )
import           System.Console.CmdArgs.Explicit
                                                ( HelpFormat(..)
                                                , helpText
                                                )
import           System.Environment             ( getArgs
                                                , withArgs
                                                )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO                      ( hPutStrLn
                                                , hSetEncoding
                                                , stderr
                                                , stdout
                                                , utf8
                                                )

----------------
-- command line
----------------

data Command
  = Search
      { input :: String,
        hasher :: String,
        size :: Int,
        charset :: String
      }
  | Hash
      { input :: String,
        hasher :: String
      }
  | Test
  deriving (Show, Data, Typeable)

-- |
search :: Command
search =
  Search
      { input   = def &= typ "HEX_STRING" &= argPos 0
      , hasher  = def &= name "a" &= typ "HASHER" &= help
                    "Specify hash algorithm  (default: sha256)"
      , size    = def &= name "n" &= typ "LENGTH" &= help
                    "Specify a search length  (default: 0)"
      , charset = def &= name "c" &= typ "CHARSET" &= help
        "Specify characters used in preimages\n(default: \"A-Za-z0-9\")"
      }
    &= help "Brute-force search with a given hex-string image"
    &= auto

-- |
hash :: Command
hash =
  Hash
      { input  = def &= typ "KEY_STRING" &= argPos 0
      , hasher = def &= name "a" &= typ "HASHER" &= help
                   "Specify hash algorithm  (default: sha256)"
      }
    &= help "Generate a hex-string image from a key string"

-- |
test :: Command
test = Test &= help "Run the test suite"

----------------
-- main
----------------

-- | main fn: entry-point
main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs
  if null args then help_ else longshot args

-- | main program
longshot :: [String] -> IO ()
longshot args = do
  cmd <- withArgs args (cmdArgsRun mode_)
  case cmd of
    Search{} -> do'search cmd
    Hash{}   -> do'hash cmd
    Test     -> do'test

-- | No default mode specified, then exit with help text
help_ :: IO a
help_ = do
  putStr . show $ helpText [summary_] HelpFormatOne mode_
  exitSuccess

-- | Program summary
summary_ :: String
summary_ =
  "longshot "
    ++ showVersion version
    ++ ", fast brute-force search in parallelism"

-- | Kind of commands
mode_ :: Mode (CmdArgs Command)
mode_ =
  cmdArgsMode
    $  modes [search, hash, test]
    &= helpArg [explicit, name "h", name "help"]
    &= verbosity
    &= program "longshot"
    &= summary summary_

-- | do
do'search :: Command -> IO ()
do'search Search {..} = undefined
do'search _           = err "unreachable"
-- "!\"#$%&'()*+,-./:;<=>?@\^_`{|}~[]"
-- "!()-.?[]_`~;:@#$%^&*+="

-- | do
do'hash :: Command -> IO ()
do'hash Hash {..} = undefined
do'hash _         = err "unreachable"

-- | do
do'test :: IO ()
do'test = err "impls"

-- main :: IO ()
-- main = do
-- args <- parseArgsOrExit patterns =<< getArgs
-- when (args >< command "image") $ genImage args
-- when (args >< command "run") $ run args

-- | Command: image
-- genImage :: Arguments -> IO ()
-- genImage args = do
-- key    <- C.pack <$> (args <->|! argument "KEY")
-- hasher <- getHasher <$> (args <->|! shortOption 'a')
-- putStrLn . C.unpack . H.encode . hasher $ key

-- | Command: run
-- run :: Arguments -> IO ()
-- run args = do
-- hex    <- args <->|! argument "HEX"
-- chars  <- args <->|! shortOption 'c'
-- size   <- (read <$> (args <->|! shortOption 'n')) :: IO Int
-- hasher <- getHasher <$> (args <->|! shortOption 'a')
-- let solver | args >< longOption "deep" = bruteforceDeep
-- | otherwise                 = bruteforce size
-- let found = solver chars hex hasher
-- case found of
-- Just key -> putStrLn $ "Found  " <> key
-- _        -> putStrLn "Not found"
err :: String -> IO ()
err msg = do
  hPutStrLn stderr msg
  exitFailure
