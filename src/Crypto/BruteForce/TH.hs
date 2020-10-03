module Crypto.BruteForce.TH where

import           Control.Monad
import           Language.Haskell.TH
import qualified Data.ByteString.Char8         as C
import qualified Crypto.Hash.SHA256            as S
import qualified Crypto.Hash.BLAKE2.BLAKE2b    as B
import           Crypto.BruteForce

-- | Brute-force with N-search-length using TH
bruteforceN :: Q Exp -> Q Exp
bruteforceN prefix = do
  xs <- replicateM (maxSearchLength - numPrefix) (newName "xs")
  let pts   = varP <$> xs
  let bytes = [| C.pack $(prefix) <> 
                 $( foldl merge [| mempty |] (varE <$> xs) ) |]
  let cond  = condE [| $( hashE bytes ) == $( varE 'image ) |]
                    [| Just $( bytes ) |]
                    [| Nothing |]
  let stmts  = ((`bindS` varE 'byteChars) <$> pts) <> 
               [noBindS cond]
  [| foldl (<|>) empty $( compE stmts ) |]
 where
  merge a b = [| $a <> $b |]
  hashE = appE (varE 'S.hash)

-- | Declare function to run in parallel for search 
funcGenerator :: Q [Dec]
funcGenerator = do
   let name = mkName "bruteforcePar" 
   prefix <- newName "prefix"
   sequence [ funD name 
              [ clause [varP prefix] 
                (normalB (bruteforceN (varE prefix))) 
                [] ] ]
