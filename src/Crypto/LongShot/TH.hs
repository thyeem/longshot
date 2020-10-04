module Crypto.LongShot.TH where

import           Control.Monad
import           Language.Haskell.TH
import           Crypto.LongShot

-- | Brute-force with N-search-length using TH
bruteforceN :: Int -> Q Exp -> Q Exp -> Q Exp -> Q Exp -> Q Exp
bruteforceN numBind chars hex hasher prefix = do
  names <- replicateM numBind (newName "names")
  let pats  = varP <$> names
  let bytes = [| $( prefix ) <>
                 $( foldl (\a b -> [| $a <> $b |])
                    [| mempty |]
                    (varE <$> names)
                  )
              |]
  let cond  = condE [| $( appE hasher bytes ) == $(hex) |]
                    [| Just (toKey $( bytes )) |]
                    [| Nothing |]
  let stmts = ((`bindS` chars) <$> pats) <> [noBindS cond]
  [| foldl (<|>) empty $( compE stmts ) |]

-- | Declare functions to run in parallel for search
funcGenerator :: Q [Dec]
funcGenerator = forM [0 .. defNumBind] funcG where 
  funcG numBind = do
    let name = mkName $ "bruteforce" <> show numBind
    chars <- newName "chars"
    hex <- newName "hex"
    hasher <- newName "hasher"
    prefix <- newName "prefix"
    funD name 
      [ clause [varP chars, varP hex, varP hasher, varP prefix] 
        (normalB (bruteforceN numBind (varE chars) (varE hex) (varE hasher) (varE prefix))) 
        []
      ]

-- | Get list of functions to run in parallel for search
funcList :: Q Exp
funcList = listE (varE . mkName . ("bruteforce" <>) . show <$> [0 .. defNumBind])