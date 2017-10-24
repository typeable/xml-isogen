{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -ddump-splices #-}

module TestDefs where

import Data.List.NonEmpty
import Data.THGen.XML
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances ()

"Bar" =:= enum
  & "baroque"
  & "bartender"

"Quux" Exhaustive =:= enum
  & "ALL"
  & "YOUR"
  & "BASE"
  & "ARE"
  & "BELONG"
  & "TO"
  & "US"

"Foo" =:= record
  + "Bar"
  ? "Baz" [t|Text|]
  !% "Quux"
  ?% "Muux" [t|XmlQuux|]

"Root" =:= record
  ! "Foo"

deriving instance Generic XmlRoot

instance Arbitrary XmlRoot where
  arbitrary = genericArbitrary
  shrink = genericShrink

deriving instance Generic XmlFoo

instance Arbitrary XmlFoo where
  arbitrary = genericArbitrary
  shrink = genericShrink

#if __GLASGOW_HASKELL__ < 800
instance Arbitrary (NonEmpty XmlBar) where
  arbitrary = genericArbitrary
  shrink = genericShrink
#endif
