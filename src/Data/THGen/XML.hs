{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

{- |

Generate XML-isomorphic types from declarative descriptions.

There are two kinds of XML-isomorphic types: enumerations and records.
Enumerations are simple enum-types generated via "Data.THGen.Enum"
plus a `FromContent` instance and a `ToXML` instance which are derived
from `Read` and `Show`. Records are a bit more complicated: to define
a record you need to supply its name, a prefix for fields, and a list
of field descriptions. A field description contains the XML tag name
and repetition kind (mandatory, optional, repeated or multiplied).

The repetition kind determines both the parsing strategy and the
wrapper around the field type:

* @a@ for mandatory fields
* @Maybe a@ for optional fields
* @[a]@ for repeated fields
* @NonEmpty a@ for multiplied fields

Example 1.

> "Color" =:= enum
>   & "R"
>   & "G"
>   & "B"

produces

> data XmlColor
>   = XmlColorR
>   | XmlColorG
>   | XmlColorB
>   | UnknownXmlColor String

with a `FromContent` instance that expects the current element content
to be either @R@, @G@ or @B@.

Example 2.

> "Message" =:= record
>   ! "author"
>   + "recipient"
>   ? "message" [t|Text|]
>   * "attachement"

produces

> data Message = Message
>   { _mAuthor      :: Author
>   , _mRecipient   :: NonEmpty Recipient
>   , _mMessage     :: Maybe Text
>   , _mAttachement :: [Attachement]
>   } deriving (...)

with a corresponding `FromDom` instance. Lenses are generated
automatically as well.

The examples above also demonstrate that to define the declarative
descriptions of data types we provide a terse and convenient EDSL.

To define an enumeration, use the `enum` function followed by the name
of the data type to be generated. You can optionally specify if the
enumeration is exhaustive (contains only the listed constructors) or
non-exhaustive (also contains a constructor for unknown values; this
is the default):

> "Enum1" Exhaustive =:= enum
>   ...

> "Enum2" NonExhaustive =:= enum
>   ...

To define a record, use the `record` function followed by the name of
the data type to be generated. The prefix for the record fields is
inferred automatically by taking all of the uppercase letters in the
name. You can override it manually like so:

> "Reference" "ref" =:= record
>    ...

To describe a record field you must supply its name as it appears
in the XML tag, prefixed by its repetition kind:

* @!@ for mandatory fields
* @?@ for optional fields
* @*@ for repeated fields
* @+@ for multiplied fields

The type of the field is inferred automatically from its name, so
if the field is called @"author"@ its type will be @Author@. You can
override the type by specifying it in quasiquotes like so:

> "Message" =:= record
>   ! "author" [t|Person|]
>   ...

-}


module Data.THGen.XML
  ( Exhaustiveness(..)
  , PrefixName(..)
  , ExhaustivenessName(..)
  , record
  , enum
  , (!)
  , (?)
  , (*)
  , (+)
  , (!%)
  , (?%)
  , (&)
  , (=:=)
    -- Re-exports
  , T.Text
  , Int
  , Integer
  ) where

import           Control.Lens hiding (repeated, enum, (&))
import           Control.Lens.Internal.FieldTH (makeFieldOpticsForDec)
import qualified Data.Char as C
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe (maybeToList, mapMaybe)
import           Data.String
import           Data.THGen.Enum
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH
import           Prelude hiding ((+), (*))
import           Text.XML.DOM.Parser
import           Text.XML.ParentAttributes
import qualified Text.XML.Writer as XW
import qualified Text.XML as X

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

data IsoXmlDescField = IsoXmlDescField XmlFieldPlural String TH.TypeQ

data IsoXmlDescAttribute = IsoXmlDescAttribute XmlAttributePlural String TH.TypeQ

data IsoXmlDescRecordPart
  = IsoXmlDescRecordField IsoXmlDescField
  | IsoXmlDescRecordAttribute IsoXmlDescAttribute

newtype IsoXmlDescRecord = IsoXmlDescRecord [IsoXmlDescRecordPart]

makePrisms ''IsoXmlDescRecord

data ExhaustivenessName = ExhaustivenessName String Exhaustiveness

newtype IsoXmlDescEnumCon
  = IsoXmlDescEnumCon { unIsoXmlDescEnumCon :: String }

instance IsString IsoXmlDescEnumCon where
  fromString = IsoXmlDescEnumCon

data IsoXmlDescEnum = IsoXmlDescEnum [IsoXmlDescEnumCon]

makePrisms ''IsoXmlDescEnum

appendField
  :: XmlFieldPlural
  -> IsoXmlDescRecord
  -> IsoXmlDescPreField
  -> IsoXmlDescRecord
appendField plural xrec (IsoXmlDescPreField name ty) =
  let xfield = IsoXmlDescRecordField $ IsoXmlDescField plural name ty
  in over _IsoXmlDescRecord (xfield:) xrec

appendAttribute
  :: XmlAttributePlural
  -> IsoXmlDescRecord
  -> IsoXmlDescPreAttribute
  -> IsoXmlDescRecord
appendAttribute plural xrec (IsoXmlDescPreAttribute name ty) =
  let xattribute = IsoXmlDescRecordAttribute $ IsoXmlDescAttribute plural name ty
  in over _IsoXmlDescRecord (xattribute:) xrec

(!), (?), (*), (+) :: IsoXmlDescRecord -> IsoXmlDescPreField -> IsoXmlDescRecord
(!) = appendField XmlFieldPluralMandatory
(?) = appendField XmlFieldPluralOptional
(*) = appendField XmlFieldPluralRepeated
(+) = appendField XmlFieldPluralMultiplied

(!%), (?%) :: IsoXmlDescRecord -> IsoXmlDescPreAttribute -> IsoXmlDescRecord
(!%) = appendAttribute XmlAttributePluralMandatory
(?%) = appendAttribute XmlAttributePluralOptional

infixl 2 !
infixl 2 ?
infixl 2 *
infixl 2 +
infixl 2 !%
infixl 2 ?%

appendEnumCon :: IsoXmlDescEnum -> IsoXmlDescEnumCon -> IsoXmlDescEnum
appendEnumCon xenum xenumcon =
  over _IsoXmlDescEnum (xenumcon:) xenum

(&) :: IsoXmlDescEnum -> IsoXmlDescEnumCon -> IsoXmlDescEnum
(&) = appendEnumCon

infixl 2 &

class Description name desc | desc -> name where
  (=:=) :: name -> desc -> TH.DecsQ

infix 0 =:=

instance Description PrefixName IsoXmlDescRecord where
  prefixName =:= descRecord =
    let descRecordParts = descRecord ^. _IsoXmlDescRecord
    in isoXmlGenerateRecord prefixName (reverse descRecordParts)

record :: IsoXmlDescRecord
record = IsoXmlDescRecord []

enum :: IsoXmlDescEnum
enum = IsoXmlDescEnum []

instance Description ExhaustivenessName IsoXmlDescEnum where
  exhaustivenessName =:= descEnum =
    let descEnumCons = descEnum ^. _IsoXmlDescEnum
    in isoXmlGenerateEnum exhaustivenessName (reverse descEnumCons)

instance IsString (TH.TypeQ -> IsoXmlDescPreField) where
  fromString = IsoXmlDescPreField

instance IsString IsoXmlDescPreField where
  fromString name = IsoXmlDescPreField name ty
    where
      ty = (TH.conT . TH.mkName) ("Xml" ++ over _head C.toUpper name)

instance IsString (TH.TypeQ -> IsoXmlDescPreAttribute) where
  fromString = IsoXmlDescPreAttribute

instance IsString IsoXmlDescPreAttribute where
  fromString name = IsoXmlDescPreAttribute name ty
    where
      ty = (TH.conT . TH.mkName) ("Xml" ++ over _head C.toUpper name)

instance s ~ String => IsString (s -> PrefixName) where
  fromString = PrefixName

instance IsString PrefixName where
  fromString strName = PrefixName strName (makeNamePrefix strName)

instance e ~ Exhaustiveness => IsString (e -> ExhaustivenessName) where
  fromString = ExhaustivenessName

instance IsString ExhaustivenessName where
  fromString strName = ExhaustivenessName strName NonExhaustive

makeNamePrefix :: String -> String
makeNamePrefix = map C.toLower . filter C.isUpper

funSimple :: TH.Name -> TH.ExpQ -> TH.DecQ
funSimple name body = TH.funD name [ TH.clause [] (TH.normalB body) [] ]

isoXmlGenerateEnum
  :: ExhaustivenessName -> [IsoXmlDescEnumCon] -> TH.DecsQ
isoXmlGenerateEnum (ExhaustivenessName strName' exh) enumCons = do
  let
    strName  = "Xml" ++ strName'
    strVals  = map unIsoXmlDescEnumCon enumCons
    enumDesc = EnumDesc exh strName strVals
    name     = TH.mkName strName
  enumDecls <- enumGenerate enumDesc
  toXmlInst <- do
    TH.instanceD
      (return [])
      [t|XW.ToXML $(TH.conT name)|]
      [funSimple 'XW.toXML [e|XW.toXML . T.pack . show|]]
  toXmlAttributeInst <- do
    TH.instanceD
      (return [])
      [t|ToXmlAttribute $(TH.conT name)|]
      [funSimple 'toXmlAttribute [e|T.pack . show|]]
  fromDomInst <- do
    TH.instanceD
      (return [])
      [t|FromDom $(TH.conT name)|]
      [funSimple 'fromDom [e|parseContent readContent|]]
  fromAttributeInst <- do
    TH.instanceD
      (return [])
      [t|FromAttribute $(TH.conT name)|]
      [funSimple 'fromAttribute [e|readContent|]]
  return $ enumDecls ++ [toXmlInst, toXmlAttributeInst,
    fromDomInst, fromAttributeInst]

isoXmlGenerateRecord :: PrefixName -> [IsoXmlDescRecordPart] -> TH.DecsQ
isoXmlGenerateRecord (PrefixName strName' strPrefix') descRecordParts = do
  let
    strName       = "Xml" ++ strName'
    strPrefix     = "x" ++ strPrefix'
    name          = TH.mkName strName
    fieldName str = "_" ++ strPrefix ++ over _head C.toUpper str
  dataDecl <- do
    let
      fields = do
        descRecordPart <- descRecordParts
        return $ case descRecordPart of
          IsoXmlDescRecordField descField ->
            let
              IsoXmlDescField fieldPlural fieldStrName fieldType = descField
              fName = TH.mkName (fieldName fieldStrName)
              fType = case fieldPlural of
                XmlFieldPluralMandatory  -> fieldType
                XmlFieldPluralOptional   -> [t| Maybe $fieldType |]
                XmlFieldPluralRepeated   -> [t| [$fieldType] |]
                XmlFieldPluralMultiplied -> [t| NonEmpty $fieldType |]
            in
              TH.varStrictType fName (TH.strictType TH.isStrict fType)
          IsoXmlDescRecordAttribute descAttribute ->
            let
              IsoXmlDescAttribute
                attributePlural attributeStrName attributeType = descAttribute
              fName = TH.mkName (fieldName attributeStrName)
              fType = case attributePlural of
                XmlAttributePluralMandatory -> attributeType
                XmlAttributePluralOptional  -> [t| Maybe $attributeType |]
            in
              TH.varStrictType fName (TH.strictType TH.isStrict fType)
    TH.dataD
      (return [])
      name
      []
      [TH.recC name fields]
      [''Eq, ''Show]
  lensDecls <- makeFieldOpticsForDec lensRules dataDecl
  fromDomInst <- do
    let
      exprHeader      = [e|pure $(TH.conE name)|]
      exprRecordParts = do
        descRecordPart <- descRecordParts
        return $ case descRecordPart of
          IsoXmlDescRecordField descField ->
            let
              IsoXmlDescField fieldPlural fieldStrName _ = descField
              exprFieldStrName = TH.litE (TH.stringL fieldStrName)
              fieldParse       = case fieldPlural of
                XmlFieldPluralMandatory  -> [e|inElem|]
                _                        -> [e|inElemTrav|]
            in
              [e|$fieldParse $exprFieldStrName fromDom|]
          IsoXmlDescRecordAttribute descAttribute ->
            let
              IsoXmlDescAttribute attributePlural attributeStrName _ = descAttribute
              exprAttributeStrName = TH.litE (TH.stringL attributeStrName)
              attributeParse       = case attributePlural of
                XmlAttributePluralMandatory -> [e|parseAttribute|]
                XmlAttributePluralOptional  -> [e|parseAttributeMaybe|]
            in
              [e|$attributeParse $exprAttributeStrName fromAttribute|]
      fromDomExpr = foldl (\e fe -> [e| $e <*> $fe |]) exprHeader exprRecordParts
    TH.instanceD
      (return [])
      [t|FromDom $(TH.conT name)|]
      [ funSimple 'fromDom fromDomExpr ]

  toXmlInst <- do
    objName <- TH.newName strPrefix
    let
      exprFields = do
        descRecordPart <- descRecordParts
        IsoXmlDescField fieldPlural fieldStrName _ <-
          maybeToList $ case descRecordPart of
            IsoXmlDescRecordField descField -> Just descField
            _                               -> Nothing
        let
          fName            = TH.mkName (fieldName fieldStrName)
          exprFieldStrName = TH.litE (TH.stringL fieldStrName)
          exprForField     = case fieldPlural of
            XmlFieldPluralMandatory  -> [e|id|]
            _                        -> [e|traverse|]
          exprFieldValue   = [e|$(TH.varE fName) $(TH.varE objName)|]
          exprFieldRender  = [e|mkElement $exprFieldStrName|]
        return [e|$exprForField $exprFieldRender $exprFieldValue|]
      toXmlExpr
        = TH.lamE [if null exprFields then TH.wildP else TH.varP objName]
        $ foldr (\fe e -> [e|$fe *> $e|]) [e|return ()|] exprFields
    TH.instanceD
      (return [])
      [t|XW.ToXML $(TH.conT name)|]
      [funSimple 'XW.toXML toXmlExpr]

  toXmlParentAttributesInst <- do
    objName <- TH.newName strPrefix
    let
      exprAttributes            = do
        descRecordPart <- descRecordParts
        IsoXmlDescAttribute attributePlural attributeStrName _ <-
          maybeToList $ case descRecordPart of
            IsoXmlDescRecordAttribute descAttribute -> Just descAttribute
            _                                       -> Nothing
        let
          fName           = TH.mkName (fieldName attributeStrName)
          exprAttrStrName = TH.litE (TH.stringL attributeStrName)
          exprAttrValue   = [e|$(TH.varE fName) $(TH.varE objName)|]
          exprAttrWrap    = case attributePlural of
            XmlAttributePluralMandatory -> [e|Just . toXmlAttribute|]
            XmlAttributePluralOptional  -> [e|fmap toXmlAttribute|]
        return [e|($exprAttrStrName, $exprAttrWrap $exprAttrValue)|]
      toXmlParentAttributesExpr
        = TH.lamE [if null exprAttributes then TH.wildP else TH.varP objName]
        $ [e|mapMaybe distribPair $(TH.listE exprAttributes)|]
    TH.instanceD
      (return [])
      [t|ToXmlParentAttributes $(TH.conT name)|]
      [funSimple 'toXmlParentAttributes toXmlParentAttributesExpr]

  return $ [dataDecl] ++ lensDecls ++
    [fromDomInst, toXmlInst, toXmlParentAttributesInst]

mkElement :: (XW.ToXML a, ToXmlParentAttributes a) => X.Name -> a -> XW.XML
mkElement name a = XW.elementA name (toXmlParentAttributes a) a

distribPair :: Functor f => (a, f b) -> f (a, b)
distribPair (a, fb) = (a,) <$> fb
