{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.THGen.Orphans
where

import           Data.Scientific
import qualified Data.Text       as Text
import           Text.XML.Writer

instance ToXML Scientific where
  toXML = content . Text.pack . formatScientific Fixed Nothing
