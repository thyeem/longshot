{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Crypto.Longshot.TH
-- License     : MIT
-- Maintainer  : Francis Lim <thyeem@gmail.com>
-- Stability   : experimental
-- Portability : unknown
module Crypto.Longshot.TH where

import Control.Monad (replicateM)
import Crypto.Longshot.Const
import Data.ByteString.Char8 (unpack)
import Data.Foldable (foldl')
import Language.Haskell.TH

-- | Brute-force with N-search-length using TH
bruteforceN :: Int -> Q Exp -> Q Exp -> Q Exp -> Q Exp -> Q Exp
bruteforceN numBind chars hex hasher prefix = do
  names <- replicateM numBind (newName "names")
  let pats = varP <$> names
  let bytes = prefix : (varE <$> names)
  let preimage = [|$(foldl' (\a b -> [|$a <> $b|]) [|mempty|] bytes)|]
  let cond =
        condE
          [|$(appE hasher preimage) == $(hex)|]
          [|Just (unpack $(preimage))|]
          [|Nothing|]
  let stmts = ((`bindS` chars) <$> pats) <> [noBindS cond]
  [|foldl' (<|>) empty $(compE stmts)|]

-- | Declare functions to run in parallel for search
funcGenerator :: Q [Dec]
funcGenerator = mapM funcG [0 .. maxNumBind]
  where
    funcG numBind = do
      let name = mkName $ "bruteforce" <> show numBind
      chars <- newName "chars"
      hex <- newName "hex"
      hasher <- newName "hasher"
      prefix <- newName "prefix"
      funD
        name
        [ clause
            [varP chars, varP hex, varP hasher, varP prefix]
            (normalB (bruteforceN numBind (varE chars) (varE hex) (varE hasher) (varE prefix)))
            []
        ]

-- | Get list of functions to run in parallel for search
funcList :: Q Exp
funcList = listE (varE . mkName . ("bruteforce" ++) . show <$> [0 .. maxNumBind])
