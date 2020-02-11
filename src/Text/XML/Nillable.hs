{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.XML.Nillable
where

import Control.DeepSeq
import Text.XML.DOM.Parser
import Text.XML.Writer
import Text.XML.ParentAttributes

-- | Type that can have explicit null value
--
-- Null value is indicated by "nil"="true" attribute.
newtype Nillable a = Nillable (Maybe a)
  deriving (Eq, Show, Ord, Read, NFData, ToXML)

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
      _ -> Nillable . Just <$> fromDom
