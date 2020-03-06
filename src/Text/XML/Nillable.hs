{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.XML.Nillable where

import           Control.DeepSeq
import           Control.Lens
import           Text.XML.DOM.Parser
import           Text.XML.ParentAttributes
import           Text.XML.Writer

-- | Type that can have explicit null value
--
-- Null value is indicated by "nil"="true" attribute.
newtype Nillable a = Nillable (Maybe a)
  deriving
    ( Eq, Show, Ord, Read, NFData, ToXML
    , Functor, Applicative, Monad, Foldable, Traversable)

_Nillable :: Prism' (Nillable a) a
_Nillable = coerced . _Just

nNill :: Nillable a
nNill = Nillable Nothing

nJust :: a -> Nillable a
nJust = Nillable . Just

instance ToXmlParentAttributes a => ToXmlParentAttributes (Nillable a) where
  toXmlParentAttributes (Nillable Nothing) =
    [("{http://www.w3.org/2001/XMLSchema-instance}nil", "true")]
  toXmlParentAttributes (Nillable (Just a)) = toXmlParentAttributes a

instance FromDom a => FromDom (Nillable a) where
  fromDom = do
    nil <- parseAttributeMaybe
      (matchName "{http://www.w3.org/2001/XMLSchema-instance}nil")
      Right
    case nil of
      Just "true" -> return (Nillable Nothing)
      _           -> Nillable . Just <$> fromDom
