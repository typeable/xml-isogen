{-# LANGUAGE TemplateHaskell #-}

{- |

Generate enumeration data types with prefixed constructors but unprefixed
`Show` and `Read` instances.

> enumGenerate $ EnumDesc "Animal" ["Cat", "Dog", "Gopher"]

produces

> data Animal = AnimalCat | AnimalDog | AnimalGopher

yet `Read` and `Show` parse and print regular values:

> show AnimalDog == "Dog"
> (read "Cat" :: Animal) == AnimalCat

-}

module Data.THGen.Enum
  ( Exhaustiveness(..)
  , EnumDesc(..)
  , enumGenerate
  ) where

import           Control.Applicative
import           Control.Lens (over, _head, (<&>))
import           Control.Monad
import qualified Data.Char as C
import           Data.THGen.Compat
import qualified Language.Haskell.TH as TH
import qualified Test.QuickCheck as QC
import qualified Text.Read as R

data Exhaustiveness = Exhaustive | NonExhaustive
  deriving (Eq, Ord, Show)

data EnumDesc = EnumDesc Exhaustiveness String [String]

funSimple :: TH.Name -> TH.ExpQ -> TH.DecQ
funSimple name body = TH.funD name [ TH.clause [] (TH.normalB body) [] ]

getC :: Char -> R.ReadPrec Char
getC c = mfilter (==c) R.get

skipSpaces :: R.ReadPrec String
skipSpaces = do
  n <- length . takeWhile C.isSpace <$> R.look
  replicateM n R.get

readRemaining :: R.ReadPrec String
readRemaining = many R.get

done :: R.ReadPrec String
done = mfilter null R.look

mangleEnumConName :: String -> String
mangleEnumConName
  = filter C.isAlphaNum
  . unwords
  . map (over _head C.toUpper)
  . words

enumGenerate :: EnumDesc -> TH.DecsQ
enumGenerate (EnumDesc exh strName strVals) = do
  let
    name       = TH.mkName strName
    vals       = strVals <&> \strVal ->
      TH.mkName (strName ++ mangleEnumConName strVal)
    unknownVal = TH.mkName ("Unknown" ++ strName)
  dataDecl <- do
    let
      constrs       = map (\val -> TH.normalC val []) vals
      unknownConstr = case exh of
        Exhaustive    -> []
        NonExhaustive ->
          [TH.normalC unknownVal [strictType [t|String|]]]
    dataD
      name
      (constrs ++ unknownConstr)
      ([''Eq, ''Ord] ++ if (exh == Exhaustive) then [''Enum, ''Bounded] else [])
  showInstDecl <- do
    unknownMatch <- case exh of
      Exhaustive    -> return []
      NonExhaustive -> do
        v <- TH.newName "str"
        return
          [ TH.match
              (TH.conP unknownVal [TH.varP v])
              (TH.normalB (TH.varE v))
              []
          ]
    let
      matches  = do
        (strVal, val) <- zip strVals vals
        return $ TH.match
          (TH.conP val [])
          (TH.normalB (TH.litE (TH.stringL strVal)))
          []
      showExpr = TH.lamCaseE (matches ++ unknownMatch)
    TH.instanceD
      (return [])
      [t|Show $(TH.conT name)|]
      [funSimple 'show showExpr]
  readInstDecl <- do
    let
      matches      = do
        (strVal, val) <- zip strVals vals
        let
          valE    = TH.conE val
          strValE = TH.litE (TH.stringL strVal)
        return $ [e|$valE <$ traverse getC ($strValE :: String) <* done|]
      unknownMatch = case exh of
        Exhaustive    -> [e|R.pfail|]
        NonExhaustive -> [e|$(TH.conE unknownVal) <$> readRemaining|]
      readPrecExpr =
        [e|skipSpaces >> (R.choice $(TH.listE matches) R.<++ $unknownMatch)|]
    TH.instanceD
      (return [])
      [t|Read $(TH.conT name)|]
      [funSimple 'R.readPrec readPrecExpr]
  arbInstance <- do
    let arbExpr = [e|QC.elements $(TH.listE (map TH.conE vals))|]
    TH.instanceD
      (return [])
      [t|QC.Arbitrary $(TH.conT name)|]
      [funSimple 'QC.arbitrary arbExpr]
  return [dataDecl, readInstDecl, showInstDecl, arbInstance]
