{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.THGen.XML.Internal where

import           Control.DeepSeq
import           Control.Lens hiding (repeated, enum, (&))
import           Control.Lens.Internal.FieldTH (makeFieldOpticsForDec)
import qualified Data.Char as C
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe (maybeToList, mapMaybe)
import           Data.String
import           Data.THGen.Compat as THC
import           Data.THGen.Enum
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Language.Haskell.TH as TH
import           Prelude hiding ((+), (*), (^))
import qualified Text.XML as X
import           Text.XML.DOM.Parser hiding (parseContent)
import           Text.XML.DOM.Parser.Internal.Content
import           Text.XML.ParentAttributes
import qualified Text.XML.Writer as XW


data GenType = Parser | Generator | Both

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

data IsoXmlDescRecord = IsoXmlDescRecord GenType [IsoXmlDescRecordPart]

makePrisms ''IsoXmlDescRecord

data ExhaustivenessName = ExhaustivenessName String Exhaustiveness

newtype IsoXmlDescEnumCon
  = IsoXmlDescEnumCon { unIsoXmlDescEnumCon :: String }

instance IsString IsoXmlDescEnumCon where
  fromString = IsoXmlDescEnumCon

data IsoXmlDescEnum = IsoXmlDescEnum GenType [IsoXmlDescEnumCon]

makePrisms ''IsoXmlDescEnum

appendField
  :: XmlFieldPlural
  -> IsoXmlDescRecord
  -> IsoXmlDescPreField
  -> IsoXmlDescRecord
appendField plural (IsoXmlDescRecord genType fields) (IsoXmlDescPreField name ty) =
  let xfield = IsoXmlDescRecordField $ IsoXmlDescField plural name ty
  in IsoXmlDescRecord genType (xfield:fields)

appendAttribute
  :: XmlAttributePlural
  -> IsoXmlDescRecord
  -> IsoXmlDescPreAttribute
  -> IsoXmlDescRecord
appendAttribute plural (IsoXmlDescRecord genType fields) (IsoXmlDescPreAttribute name ty) =
  let xattribute = IsoXmlDescRecordAttribute $ IsoXmlDescAttribute plural name ty
  in IsoXmlDescRecord genType (xattribute:fields)

appendContent
  :: IsoXmlDescRecord
  -> IsoXmlDescPreContent
  -> IsoXmlDescRecord
appendContent (IsoXmlDescRecord genType fields) (IsoXmlDescPreContent name ty) =
  let xcontent = IsoXmlDescRecordContent $ IsoXmlDescContent name ty
  in IsoXmlDescRecord genType (xcontent:fields)

(!), (?), (*), (+) :: IsoXmlDescRecord -> IsoXmlDescPreField -> IsoXmlDescRecord
(!) = appendField XmlFieldPluralMandatory
(?) = appendField XmlFieldPluralOptional
(*) = appendField XmlFieldPluralRepeated
(+) = appendField XmlFieldPluralMultiplied

(!%), (?%) :: IsoXmlDescRecord -> IsoXmlDescPreAttribute -> IsoXmlDescRecord
(!%) = appendAttribute XmlAttributePluralMandatory
(?%) = appendAttribute XmlAttributePluralOptional

(^) :: IsoXmlDescRecord -> IsoXmlDescPreContent -> IsoXmlDescRecord
(^) = appendContent

infixl 2 !
infixl 2 ?
infixl 2 *
infixl 2 +
infixl 2 !%
infixl 2 ?%
infixl 2 ^

appendEnumCon :: IsoXmlDescEnum -> IsoXmlDescEnumCon -> IsoXmlDescEnum
appendEnumCon (IsoXmlDescEnum genType enumCons) xenumcon =
  IsoXmlDescEnum genType (xenumcon:enumCons)

(&) :: IsoXmlDescEnum -> IsoXmlDescEnumCon -> IsoXmlDescEnum
(&) = appendEnumCon

infixl 2 &

class Description name desc | desc -> name where
  (=:=) :: name -> desc -> TH.DecsQ

infix 0 =:=

instance Description PrefixName IsoXmlDescRecord where
  prefixName =:= (IsoXmlDescRecord genType descRecordParts) =
    isoXmlGenerateDatatype genType prefixName (reverse descRecordParts)

record :: GenType -> IsoXmlDescRecord
record gt = IsoXmlDescRecord gt []

enum :: GenType -> IsoXmlDescEnum
enum gt = IsoXmlDescEnum gt []

instance Description ExhaustivenessName IsoXmlDescEnum where
  exhaustivenessName =:= (IsoXmlDescEnum genType descEnumCons) =
    isoXmlGenerateEnum genType exhaustivenessName (reverse descEnumCons)

instance IsString (TH.TypeQ -> IsoXmlDescPreField) where
  fromString = IsoXmlDescPreField

instance IsString IsoXmlDescPreField where
  fromString name = IsoXmlDescPreField name ty
    where
      ty = (TH.conT . TH.mkName) ("Xml" ++ over _head C.toUpper (xmlLocalName name))

instance IsString (TH.TypeQ -> IsoXmlDescPreAttribute) where
  fromString = IsoXmlDescPreAttribute

instance IsString (TH.TypeQ -> IsoXmlDescPreContent) where
  fromString = IsoXmlDescPreContent

instance IsString IsoXmlDescPreAttribute where
  fromString name = IsoXmlDescPreAttribute name ty
    where
      ty = (TH.conT . TH.mkName) ("Xml" ++ over _head C.toUpper name)

instance IsString IsoXmlDescPreContent where
  fromString name = IsoXmlDescPreContent name ty
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
  :: GenType
  -> ExhaustivenessName
  -> [IsoXmlDescEnumCon]
  -> TH.DecsQ
isoXmlGenerateEnum genType (ExhaustivenessName strName' exh) enumCons = do
  let
    strName  = "Xml" ++ strName'
    strVals  = map unIsoXmlDescEnumCon enumCons
    enumDesc = EnumDesc exh strName strVals
    name     = TH.mkName strName
  enumDecls <- enumGenerate enumDesc
  let
    genToXmlInst = do
      TH.instanceD
        (return [])
        [t|XW.ToXML $(TH.conT name)|]
        [funSimple 'XW.toXML [e|XW.toXML . T.pack . show|]]
    genToXmlAttributeInst = do
      TH.instanceD
        (return [])
        [t|ToXmlAttribute $(TH.conT name)|]
        [funSimple 'toXmlAttribute [e|T.pack . show|]]
    genFromDomInst = do
      TH.instanceD
        (return [])
        [t|FromDom $(TH.conT name)|]
        [funSimple 'fromDom [e|parseContent readContent|]]
    genFromAttributeInst = do
      TH.instanceD
        (return [])
        [t|FromAttribute $(TH.conT name)|]
        [funSimple 'fromAttribute [e|readContent|]]
  case genType of
    Generator -> do
      toXmlInst <- genToXmlInst
      toXmlAttributeInst <- genToXmlAttributeInst
      return $ enumDecls ++ [toXmlInst, toXmlAttributeInst]
    Parser -> do
      fromDomInst <- genFromDomInst
      fromAttributeInst <- genFromAttributeInst
      return $ enumDecls ++ [fromDomInst, fromAttributeInst]
    Both -> do
      toXmlInst <- genToXmlInst
      toXmlAttributeInst <- genToXmlAttributeInst
      fromDomInst <- genFromDomInst
      fromAttributeInst <- genFromAttributeInst
      return $ enumDecls ++ [toXmlInst, toXmlAttributeInst,
        fromDomInst, fromAttributeInst]

isoXmlGenerateDatatype :: GenType -> PrefixName -> [IsoXmlDescRecordPart] -> TH.DecsQ
isoXmlGenerateDatatype genType (PrefixName strName' strPrefix') descRecordParts = do
  let
    isNewtype     = length descRecordParts == 1
    strName       = "Xml" ++ strName'
    strPrefix     = "x" ++ strPrefix'
    name          = TH.mkName strName
    fieldName str = "_" ++ strPrefix ++ over _head C.toUpper str
  termDecl <- do
    let
      fields = do
        descRecordPart <- descRecordParts
        return $ case descRecordPart of
          IsoXmlDescRecordField descField ->
            let
              IsoXmlDescField fieldPlural rawName fieldType = descField
              fieldStrName = xmlLocalName rawName
              fName = TH.mkName (fieldName fieldStrName)
              fType = case fieldPlural of
                XmlFieldPluralMandatory  -> fieldType
                XmlFieldPluralOptional   -> [t| Maybe $fieldType |]
                XmlFieldPluralRepeated   -> [t| [$fieldType] |]
                XmlFieldPluralMultiplied -> [t| NonEmpty $fieldType |]
            in if isNewtype
              then THC.varStrictType fName (THC.nonStrictType fType)
              else THC.varStrictType fName (THC.strictType fType)
          IsoXmlDescRecordAttribute descAttribute ->
            let
              IsoXmlDescAttribute
                attributePlural attributeStrName attributeType = descAttribute
              fName = TH.mkName (fieldName attributeStrName)
              fType = case attributePlural of
                XmlAttributePluralMandatory -> attributeType
                XmlAttributePluralOptional  -> [t| Maybe $attributeType |]
            in if isNewtype
              then THC.varStrictType fName (THC.nonStrictType fType)
              else THC.varStrictType fName (THC.strictType fType)
          IsoXmlDescRecordContent descContent ->
            let
              IsoXmlDescContent contentStrName contentType = descContent
              fName = TH.mkName (fieldName contentStrName)
              fType = contentType
            in if isNewtype
              then THC.varStrictType fName (THC.nonStrictType fType)
              else THC.varStrictType fName (THC.strictType fType)
    if isNewtype
    -- generate a newtype instead to do less allocations later
    then THC.newtypeD name (TH.recC name fields) [''Eq, ''Show, ''Generic]
    else THC.dataD name [TH.recC name fields] [''Eq, ''Show, ''Generic]
  lensDecls <- makeFieldOpticsForDec lensRules termDecl
  nfDataInst <- do
    TH.instanceD
      (return [])
      [t|NFData $(TH.conT name)|]
      [ ]

  let
    genFromDomInst = do
      let
        exprHeader      = [e|pure $(TH.conE name)|]
        exprRecordParts = do
          descRecordPart <- descRecordParts
          return $ case descRecordPart of
            IsoXmlDescRecordField descField ->
              let
                IsoXmlDescField fieldPlural rawName _ = descField
                fieldStrName     = xmlLocalName rawName
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
            IsoXmlDescRecordContent _ -> [e|parseContent fromAttribute|]
        fromDomExpr = foldl (\e fe -> [e| $e <*> $fe |]) exprHeader exprRecordParts
      TH.instanceD
        (return [])
        [t|FromDom $(TH.conT name)|]
        [ funSimple 'fromDom fromDomExpr ]

    genToXmlInst = do
      objName <- TH.newName strPrefix
      let
        exprFields = do
          descRecordPart <- descRecordParts
          case descRecordPart of
            IsoXmlDescRecordField (IsoXmlDescField fieldPlural rawName _) -> do
              let
                fieldStrName     = xmlLocalName rawName
                fName            = TH.mkName (fieldName fieldStrName)
                exprFieldStrName = TH.litE (TH.stringL rawName)
                exprForField     = case fieldPlural of
                  XmlFieldPluralMandatory  -> [e|id|]
                  _                        -> [e|traverse|]
                exprFieldValue   = [e|$(TH.varE fName) $(TH.varE objName)|]
                exprFieldRender  =
                  [e|(\a ->
                    XW.elementA $exprFieldStrName (toXmlParentAttributes a) a)|]
              return [e|$exprForField $exprFieldRender $exprFieldValue|]
            IsoXmlDescRecordContent (IsoXmlDescContent rawName _)     -> do
              let
                fieldStrName     = xmlLocalName rawName
                fName            = TH.mkName (fieldName fieldStrName)
                exprFieldValue   = [e|$(TH.varE fName) $(TH.varE objName)|]
              return [e|XW.content . toXmlAttribute $ $exprFieldValue|]
            _                         -> []
        toXmlExpr
          = TH.lamE [if null exprFields then TH.wildP else TH.varP objName]
          $ foldr (\fe e -> [e|$fe *> $e|]) [e|return ()|] exprFields
      TH.instanceD
        (return [])
        [t|XW.ToXML $(TH.conT name)|]
        [funSimple 'XW.toXML toXmlExpr]
    genToXmlParentAttributeInst = do
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
  #if __GLASGOW_HASKELL__ < 800
      TH.instanceD
  #else
      TH.instanceWithOverlapD (Just TH.Overlapping)
  #endif
        (return [])
        [t|ToXmlParentAttributes $(TH.conT name)|]
        [funSimple 'toXmlParentAttributes toXmlParentAttributesExpr]

  case genType of
    Generator -> do
      toXmlInst <- genToXmlInst
      toXmlParentAttributesInst <- genToXmlParentAttributeInst
      return $ [termDecl] ++ lensDecls ++
        [toXmlInst, toXmlParentAttributesInst, nfDataInst]
    Parser -> do
      fromDomInst <- genFromDomInst
      return $ [termDecl] ++ lensDecls ++ [fromDomInst, nfDataInst]
    Both -> do
      toXmlInst <- genToXmlInst
      toXmlParentAttributesInst <- genToXmlParentAttributeInst
      fromDomInst <- genFromDomInst
      return $ [termDecl] ++ lensDecls ++
        [fromDomInst, toXmlInst, toXmlParentAttributesInst, nfDataInst]

distribPair :: Functor f => (a, f b) -> f (a, b)
distribPair (a, fb) = (a,) <$> fb

-- | Get a local part of (possibly) fully qualified 'X.Name':
--
-- >>> xmlLocalName "{http://example.com/ns/my-namespace}my-name"
-- "my-name"
xmlLocalName :: String -> String
xmlLocalName = T.unpack . X.nameLocalName . fromString
