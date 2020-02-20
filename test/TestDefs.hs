{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -ddump-splices #-}

module TestDefs where

-- import Data.List.NonEmpty
import Data.THGen.XML
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances ()

"Bar" =:= enum ParserAndGenerator
  & "baroque"
  & "bartender"

"Quux" Exhaustive =:= enum ParserAndGenerator
  & "ALL"
  & "YOUR"
  & "BASE"
  & "ARE"
  & "BELONG"
  & "TO"
  & "US"

"Foo" =:= record ParserAndGenerator
  ^ "Shmuux" [t|XmlQuux|]
  + "Bar"
  ? "Baz" [t|Text|]
  !% "Quux"
  ?% "Muux" [t|XmlQuux|]

"Root" =:= record ParserAndGenerator
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
