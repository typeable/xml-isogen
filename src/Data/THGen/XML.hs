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
import           Data.String
import           Data.THGen.Enum
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH
import           Prelude hiding ((+), (*))
import           Text.XML.DOM.Parser
import qualified Text.XML.Writer as XW

data XmlFieldPlural
  = XmlFieldPluralMandatory  -- Occurs exactly 1 time (Identity)
  | XmlFieldPluralOptional   -- Occurs 0 or 1 times (Maybe)
  | XmlFieldPluralRepeated   -- Occurs 0 or more times (List)
  | XmlFieldPluralMultiplied -- Occurs 1 or more times (NonEmpty)

data PrefixName = PrefixName String String

data IsoXmlDescPreField = IsoXmlDescPreField String TH.TypeQ

data IsoXmlDescField = IsoXmlDescField XmlFieldPlural String TH.TypeQ

data IsoXmlDescRecord = IsoXmlDescRecord [IsoXmlDescField]

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
  let xfield = IsoXmlDescField plural name ty
  in over _IsoXmlDescRecord (xfield:) xrec

(!) :: IsoXmlDescRecord -> IsoXmlDescPreField -> IsoXmlDescRecord
(!) = appendField XmlFieldPluralMandatory

(?) :: IsoXmlDescRecord -> IsoXmlDescPreField -> IsoXmlDescRecord
(?) = appendField XmlFieldPluralOptional

(*) :: IsoXmlDescRecord -> IsoXmlDescPreField -> IsoXmlDescRecord
(*) = appendField XmlFieldPluralRepeated

(+) :: IsoXmlDescRecord -> IsoXmlDescPreField -> IsoXmlDescRecord
(+) = appendField XmlFieldPluralMultiplied

infixl 2 !
infixl 2 ?
infixl 2 *
infixl 2 +

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
    let descFields = descRecord ^. _IsoXmlDescRecord
    in isoXmlGenerateRecord prefixName (reverse descFields)

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
    let toXmlExpr = [e|XW.toXML . T.pack . show|]
    TH.instanceD
      (return [])
      [t|XW.ToXML $(TH.conT name)|]
      [funSimple 'XW.toXML toXmlExpr]
  fromDomInst <- do
    TH.instanceD
      (return [])
      [t|FromDom $(TH.conT name)|]
      [ funSimple 'fromDom [e|parseContent readContent|] ]
  return (enumDecls ++ [toXmlInst, fromDomInst])

isoXmlGenerateRecord :: PrefixName -> [IsoXmlDescField] -> TH.DecsQ
isoXmlGenerateRecord (PrefixName strName' strPrefix') descFields = do
  let
    strName       = "Xml" ++ strName'
    strPrefix     = "x" ++ strPrefix'
    name          = TH.mkName strName
    fieldName str = "_" ++ strPrefix ++ over _head C.toUpper str
  dataDecl <- do
    let
      constructors = do
        IsoXmlDescField fieldPlural fieldStrName fieldType <- descFields
        let
          fName = TH.mkName (fieldName fieldStrName)
          fType = case fieldPlural of
            XmlFieldPluralMandatory  -> fieldType
            XmlFieldPluralOptional   -> [t| Maybe $fieldType |]
            XmlFieldPluralRepeated   -> [t| [$fieldType] |]
            XmlFieldPluralMultiplied -> [t| NonEmpty $fieldType |]
        return $ TH.varStrictType fName (TH.strictType TH.isStrict fType)
    TH.dataD
      (return [])
      name
      []
      [TH.recC name constructors]
      [''Eq, ''Show]
  lensDecls <- makeFieldOpticsForDec lensRules dataDecl
  fromDomInst <- do
    let
      exprHeader = [e|pure $(TH.conE name)|]
      exprFields = do
        IsoXmlDescField fieldPlural fieldStrName _ <- descFields
        let
          exprFieldStrName = TH.litE (TH.stringL fieldStrName)
          fieldParse       = case fieldPlural of
            XmlFieldPluralMandatory  -> [e|inElem|]
            _                        -> [e|inElemTrav|]
        return [e|$fieldParse $exprFieldStrName fromDom|]
      fromDomExpr = foldl (\e fe -> [e| $e <*> $fe |]) exprHeader exprFields
    TH.instanceD
      (return [])
      [t|FromDom $(TH.conT name)|]
      [ funSimple 'fromDom fromDomExpr ]

  toXmlInst <- do
    objName <- TH.newName strPrefix
    let
      exprFooter = [e|return ()|]
      exprFields = do
        IsoXmlDescField fieldPlural fieldStrName _ <- descFields
        let
          fieldRender      = [e|XW.toXML|]
          fName            = TH.mkName (fieldName fieldStrName)
          exprFieldStrName = TH.litE (TH.stringL fieldStrName)
          exprForField     = case fieldPlural of
            XmlFieldPluralMandatory  -> [e|id|]
            _                        -> [e|traverse|]
          exprFieldValue   = [e|$(TH.varE fName) $(TH.varE objName)|]
          exprFieldRender  = [e|XW.element $exprFieldStrName . $fieldRender|]
        return [e|$exprForField $exprFieldRender $exprFieldValue|]
      toXmlExpr
        = TH.lamE [if null exprFields then TH.wildP else TH.varP objName]
        $ foldr (\fe e -> [e|$fe *> $e|]) exprFooter exprFields
    TH.instanceD
      (return [])
      [t|XW.ToXML $(TH.conT name)|]
      [funSimple 'XW.toXML toXmlExpr]
  return $ [dataDecl] ++ lensDecls ++ [fromDomInst, toXmlInst]
