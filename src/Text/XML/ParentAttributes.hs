module Text.XML.ParentAttributes
  ( ToXmlParentAttributes(..)
  , ToXmlAttribute(..)
  , toXmlAttributeIntegral
  ) where

import Data.Text as T.S
import Data.Text.Lazy as T.L
import Data.Text.Lazy.Builder as T.L
import Data.Text.Lazy.Builder.Int as T.L
import Numeric.Natural
import Text.XML

class ToXmlParentAttributes a where
  toXmlParentAttributes :: a -> [(Name, T.S.Text)]

-- | A catch-all instance: by default objects don't have parent attributes.
-- The alternative solution is to write a default implementation in the class,
-- but it has a drawback that it requires numerous (boilerplate) empty
-- instances - one for each type that doesn't have parent attributes (most
-- types). This overlappable instance gets overriden for XML records
-- (described in "Data.THGen.XML") because they can store parent attributes
-- added via the @!%@ and @?%@ operators.
-- For example, `toXmlParentAttributes` applied to a `Text` or `Integer' value
-- returns @[]@ because of this instance. However, an XML record defined as
-- @"Foo" =:= record !% "bar"@ will have its own TH-generated
-- `ToXmlParentAttributes` instance that inspects the value of its @bar@ field
-- and returns it as an attribute.
-- This way the TH-generators can safely apply `toXmlParentAttributes` to any
-- value without worrying about missing instances.
instance {-# OVERLAPPABLE #-} ToXmlParentAttributes a where
  toXmlParentAttributes _ = []

class ToXmlAttribute a where
  toXmlAttribute :: a -> T.S.Text

instance ToXmlAttribute T.S.Text where
  toXmlAttribute = id

toXmlAttributeIntegral :: Integral a => a -> T.S.Text
toXmlAttributeIntegral = T.L.toStrict . T.L.toLazyText . T.L.decimal

instance ToXmlAttribute Int where
  toXmlAttribute = toXmlAttributeIntegral

instance ToXmlAttribute Word where
  toXmlAttribute = toXmlAttributeIntegral

instance ToXmlAttribute Integer where
  toXmlAttribute = toXmlAttributeIntegral

instance ToXmlAttribute Natural where
  toXmlAttribute = toXmlAttributeIntegral
