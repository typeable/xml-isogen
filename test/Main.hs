{-# OPTIONS -fno-warn-unused-imports #-}
module Main where

import Control.Exception
import Text.XML
import Data.Default
import System.IO.Unsafe
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TestDefs
import Text.XML.DOM.Parser
import Text.XML.Writer

isomorphicFoo :: XmlRoot -> Property
isomorphicFoo a = a === isogen
  where
    isogen :: XmlRoot
    isogen =
      let
        doc = document "Root" $ toXML a
        res = case runDomParser doc fromDom of
          Left e -> unsafePerformIO $ do
            pprint doc
            throwIO e
          Right a' -> a'
      in res

main :: IO ()
main = hspec $ do
  prop "isomorphic" isomorphicFoo
