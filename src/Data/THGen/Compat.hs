{-# LANGUAGE CPP #-}

module Data.THGen.Compat
  ( dataD
  , strictType
  , varStrictType
  ) where

import qualified Language.Haskell.TH as TH

dataD :: TH.Name -> [TH.ConQ] -> [TH.Name] -> TH.DecQ
#if MIN_VERSION_template_haskell(2,12,0)
dataD name cons derivs = TH.dataD (return []) name [] Nothing cons [derivCls]
  where
    derivCls = TH.derivClause Nothing $ fmap TH.conT derivs
#elif MIN_VERSION_template_haskell(2,11,0)
dataD name cons derivs = TH.dataD (return []) name [] Nothing cons derivCls
  where
    derivCls = traverse TH.conT derivs
#else
dataD name = TH.dataD (return []) name []
#endif

#if MIN_VERSION_template_haskell(2,11,0)
strictType :: TH.TypeQ -> TH.BangTypeQ
strictType = TH.bangType (TH.bang TH.noSourceUnpackedness TH.sourceStrict)
#else
strictType :: TH.TypeQ -> TH.StrictTypeQ
strictType = TH.strictType TH.isStrict
#endif

#if MIN_VERSION_template_haskell(2,11,0)
varStrictType :: TH.Name -> TH.BangTypeQ -> TH.VarBangTypeQ
varStrictType = TH.varBangType
#else
varStrictType :: TH.Name -> TH.StrictTypeQ -> TH.VarStrictTypeQ
varStrictType = TH.varStrictType
#endif
