module Crypto.BruteForce.TH where

import           Control.Monad
import           Language.Haskell.TH
import qualified Data.ByteString.Char8         as C
import qualified Crypto.Hash.SHA256            as S
import qualified Crypto.Hash.BLAKE2.BLAKE2b    as B
import           Crypto.BruteForce

-- | Brute-force with string length of N using TH
bruteforceN :: String -> Q Exp
bruteforceN prefix = do
  xs <- replicateM (limit - prefixN) (newName "xs")
  let pts   = varP <$> xs
  let bytes = [| C.pack prefix 
                 <> 
                 $( foldl merge [| mempty |] (varE <$> xs) ) 
              |]
  let cond  = condE [| $( hashE bytes ) == $( varE 'image ) |]
                    [| Just $( bytes ) |]
                    [| Nothing |]
  let stmts  = ((`bindS` varE 'byteChars) <$> pts) 
               <> 
               [noBindS cond]
  [| foldl (<|>) empty $( compE stmts ) |]
 where
  merge a b = [| $a <> $b |]
  hashE = appE (varE 'S.hash)

-- | Declare functions to run in parallel for search 
funcGenerator :: Q [Dec]
funcGenerator = forM (zip [1..] prefixes) bfDec where 
 bfDec (i, prefix) = do
   let name = mkName $ "bf" <> show i
   funD name [clause [] (normalB (bruteforceN prefix)) []]

-- | Get list of functions to run in parallel for search
funcListPar :: Q Exp
funcListPar = listE ( varE . mkName . ("bf" <> ) . show <$> [1..length chars ^ prefixN])
