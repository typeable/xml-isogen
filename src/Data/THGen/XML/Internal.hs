module Data.THGen.XML.Internal where

import Control.Lens
import Data.String
import Data.THGen.Compat
import Data.THGen.Enum
import Language.Haskell.TH as TH


data XmlFieldPlural
  = XmlFieldPluralMandatory  -- Occurs exactly 1 time (Identity)
  | XmlFieldPluralOptional   -- Occurs 0 or 1 times (Maybe)
  | XmlFieldPluralRepeated   -- Occurs 0 or more times (List)
  | XmlFieldPluralMultiplied -- Occurs 1 or more times (NonEmpty)

data XmlAttributePlural
  = XmlAttributePluralMandatory -- Occurs exactly 1 time (Identity)
  | XmlAttributePluralOptional  -- Occurs 0 or 1 times (Maybe)

data PrefixName = PrefixName String String

data IsoXmlDescPreField = IsoXmlDescPreField String TH.TypeQ

data IsoXmlDescPreAttribute = IsoXmlDescPreAttribute String TH.TypeQ

data IsoXmlDescPreContent = IsoXmlDescPreContent String TH.TypeQ

data IsoXmlDescField = IsoXmlDescField XmlFieldPlural String TH.TypeQ

data IsoXmlDescAttribute = IsoXmlDescAttribute XmlAttributePlural String TH.TypeQ

data IsoXmlDescContent = IsoXmlDescContent String TH.TypeQ

data IsoXmlDescRecordPart
  = IsoXmlDescRecordField IsoXmlDescField
  | IsoXmlDescRecordAttribute IsoXmlDescAttribute
  | IsoXmlDescRecordContent IsoXmlDescContent

newtype IsoXmlDescRecord = IsoXmlDescRecord [IsoXmlDescRecordPart]

makePrisms ''IsoXmlDescRecord

data ExhaustivenessName = ExhaustivenessName String Exhaustiveness

newtype IsoXmlDescEnumCon
  = IsoXmlDescEnumCon { unIsoXmlDescEnumCon :: String }

instance IsString IsoXmlDescEnumCon where
  fromString = IsoXmlDescEnumCon

data IsoXmlDescEnum = IsoXmlDescEnum [IsoXmlDescEnumCon]

makePrisms ''IsoXmlDescEnum
