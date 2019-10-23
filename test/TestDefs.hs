{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -ddump-splices #-}

module TestDefs where

-- import Data.List.NonEmpty
import Data.THGen.XML
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances ()

"Bar" =:= enum Both
  & "baroque"
  & "bartender"

"Quux" Exhaustive =:= enum Both
  & "ALL"
  & "YOUR"
  & "BASE"
  & "ARE"
  & "BELONG"
  & "TO"
  & "US"

"Foo" =:= record Both NoLens
  ^ "shmuux" [t|XmlQuux|]
  + "bar"
  ? "baz" [t|Text|]
  !% "quux"
  ?% "muux" [t|XmlQuux|]

"Root" =:= record Both LensRenaming
  ! "{http://example.com/ns/my-namespace}Foo"

instance Arbitrary XmlRoot where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary XmlFoo where
  arbitrary = genericArbitrary
  shrink = genericShrink

#if __GLASGOW_HASKELL__ < 800
instance Arbitrary (NonEmpty XmlBar) where
  arbitrary = genericArbitrary
  shrink = genericShrink
#endif
