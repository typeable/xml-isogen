{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -ddump-splices #-}

module TestDefs where

import Data.THGen.XML

"Bar" =:= enum
  & "baroque"
  & "bartender"

"Quux" Exhaustive =:= enum
  & "ALL"
  & "YOUR"
  & "BASE"
  & "ARE"
  & "BELONG"
  & "TO"
  & "US"

"Foo" =:= record
  + "Bar"
  ? "Baz" [t|Text|]
  !% "Quux"
