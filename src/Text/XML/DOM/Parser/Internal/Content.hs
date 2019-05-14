{-# LANGUAGE OverloadedStrings #-}
module Text.XML.DOM.Parser.Internal.Content where
import Control.Lens
import Control.Monad.Reader
import Data.Text as T
import Text.XML
import Text.XML.DOM.Parser hiding (getCurrentContent, parseContent)

-- Get current content. If there are more than one content
-- they are intercalate with space like in xpath `text()`
--
getCurrentContent :: (Monad m) => DomParserT Identity m Text
getCurrentContent
  = T.intercalate " "
  . (\e -> [T.strip cs| (NodeContent cs) <- e])
  . elementNodes
  . runIdentity
  . _pdElements
  <$> ask

parseContent
  :: (Monad m)
  => (Text -> Either Text a)
     -- ^ Content parser, return error msg if value is not parsed
  -> DomParserT Identity m a
parseContent parse
  = getCurrentContent
  >>= either (throwParserError . PEContentWrongFormat) return . parse
