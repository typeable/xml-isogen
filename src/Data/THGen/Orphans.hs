module Data.THGen.Orphans
where

import qualified Data.Text as Text
import           Data.Scientific
import           Text.XML.Writer

instance ToXML Scientific where
  toXML = content . Text.pack . show
