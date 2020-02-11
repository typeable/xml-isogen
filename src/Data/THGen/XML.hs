{-# LANGUAGE NoImplicitPrelude #-}

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

> "Color" =:= enum Both
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

> "Message" =:= record Both
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

> "Enum1" Exhaustive =:= enum Generator
>   ...

> "Enum2" NonExhaustive =:= enum Generator
>   ...

To define a record, use the `record` function followed by the name of
the data type to be generated. The prefix for the record fields is
inferred automatically by taking all of the uppercase letters in the
name. You can override it manually like so:

> "Reference" "ref" =:= record Parser
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

> "Message" =:= record Both
>   ! "author" [t|Person|].
>   ...

Both @record@ and @enum@ take a @GenType@ argument which tells it to generate
generator, parser or both. It's being used to optimize compilation time by
skipping the unnecessary TH instance generation.

-}


module Data.THGen.XML
  ( Exhaustiveness(..)
  , PrefixName(..)
  , ExhaustivenessName(..)
  , GenType(..)
  , record
  , enum
  , (!)
  , (?)
  , (*)
  , (+)
  , (!%)
  , (?%)
  , (&)
  , (^)
  , (=:=)
    -- Re-exports
  , Text
  , P.Int
  , P.Integer
  , Nillable(..)
  ) where

import           Data.THGen.Enum
import           Data.THGen.XML.Internal
import           Data.Text
import           Text.XML.Nillable
import qualified Prelude as P
