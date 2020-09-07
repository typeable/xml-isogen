# xml-isogen

[![Build Status](https://travis-ci.org/typeable/xml-isogen.svg?branch=master)](https://travis-ci.org/typeable/xml-isogen)

TemplateHaskell generators for XML-isomorphic data types, including
instances for parsing and rendering. A convenient DSL to define those
types.

Essentially it's a haskell DSL which allows its users to generate XML parsers and generators for haskell datatypes.
See also [xsd-isogen](https://github.com/typeable/xsd-isogen)

## Tutorial

Lets go through series of examples. First things first:

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Tutorial
where

import Prelude hiding ((^))
import Control.DeepSeq      -- from deepseq
import Data.Text
import Data.THGen.XML       -- from xml-isogen
import Text.XML.Writer      -- from xml-conduit-writer
import Text.XML.DOM.Parser  -- from dom-parser
```

### Records

Lets say we want to parse and/or generate an XML file of the following form:

```XML
<?xml version="1.0" encoding="UTF-8"?>
<person>
    <name>
        John
    </name>
    <email>
        john@example.com
    </email>
</person>
```

Person has a name and an email, and email could be omitted. With `xml-isogen` it's
enough to write the following definition:

```haskell
"Person" =:= record ParserAndGenerator
  ! "name" [t|Text|]
  ? "email" [t|Text|]
```

At this point you can load the module into `ghci` and check what was generated for us
so far:

```
*Tutorial> :browse
data XmlPerson
  = XmlPerson {_xpName :: !Text, _xpEmail :: !(Maybe Text)}
xpEmail ::
  lens-4.18.1:Control.Lens.Type.Lens' XmlPerson (Maybe Text)
xpName :: lens-4.18.1:Control.Lens.Type.Lens' XmlPerson Text
```

We have a data type `XmlPerson` with two fields and two lenses. Note that the
fields have a prefix built of an underscore and all upper case characters
and digits of the type name.


Lets take a closer look at `XmlPerson`:

```
*Tutorial> :i XmlPerson
data XmlPerson
  = XmlPerson {_xpName :: !Text, _xpEmail :: !(Maybe Text)}
      -- Defined at Tutorial.hs:13:1
instance Show XmlPerson -- Defined at Tutorial.hs:13:1
instance Eq XmlPerson -- Defined at Tutorial.hs:13:1
instance FromDom XmlPerson -- Defined at Tutorial.hs:13:1
instance ToXML XmlPerson -- Defined at Tutorial.hs:13:1

```

We have
[FromDom](https://hackage.haskell.org/package/dom-parser-3.1.0/docs/Text-XML-DOM-Parser-FromDom.html#t:FromDom)
and
[ToXML](https://hackage.haskell.org/package/xml-conduit-writer-0.1.1.2/docs/Text-XML-Writer.html#t:ToXML)
instance generated for us. That's because
we instructed `xml-isogen` to generate them using the `ParserAndGenerator` noun. You can specify
also `Parser` or `Generator` if you want only one of them.

The `_xpEmail` field is optional; that's because we prefixed it with `?` modifier.
Here is the list of possible modifiers that affect types:

Modifier | Description | Generated Type
--- | --- | ---
`!` | required | `a`
`?` | optional | `Maybe a`
`*` | repeated | `[a]`
`+` | nonempty | `NonEmpty a`

### Supported types

Fields in a record may have any types as long as they are instances of `Eq`, `Show`,
`NFData`, `FromDom` (for the parser) and `ToXml` (for the generator). Remember
though that TemplateHaskell requires types to be available before they are used
in a splice.

You can omit field types altogether, in that case the type will be assumed to be a
capitalized field name with an `Xml` prefix.
It's your responsibility to make sure that type exists.
Example:

```haskell
newtype XmlEmail = XmlEmail Text
  deriving (Eq, Show, NFData, ToXML)

instance FromDom XmlEmail where
  fromDom = XmlEmail <$> fromDom

"Example1" =:= record Parser
  ! "email"  -- will have type XmlEmail
```

### Enumerations

Often XML element can contain only limited number of possible values. Lets define
a type `Status` that can have only values `Active`, `Pending` or `Deleted`:

```haskell
"Status" =:= enum ParserAndGenerator
  & "Active"
  & "Pending"
  & "Deleted"
```

This definition will generate the following type for us:

```haskell
data XmlStatus
  = XmlStatusActive
  | XmlStatusPending
  | XmlStatusDeleted
  | UnknownXmlStatus !String
```

It has all the necessary instances, so you can use it as a type for a field.

### Append content

Sometimes the XML you are dealing with contains a mix of elements and immediate
content. Something like the following:

```XML
<?xml version="1.0" encoding="UTF-8"?>
<example>
    <field1>
        I am
    </field1>
    totally
    <field2>
        weird
    </field2>
</example>
```

You can model this with an "append content" modifier `^`. It will instruct
`xml-isogen` to append content of the field as it is, without wrapping it
into an XML element. For our case it may look like this:

```haskell
"Example2" =:= record ParserAndGenerator
  ! "field1" [t|Text|]
  ^ "mixed" [t|Text|]
  ! "field2" [t|Text|]
```

After parsing the XML above, we'll get the following:

```haskell
XmlExample2 {_xe2Field1 = "I am", _xe2Mixed = "totally", _xe2Field2 = "weird"}
```

### Attributes

`xml-isogen` also supports XML attributes using `!%` and `?%` modifiers:

```haskell
"Example3" =:= record ParserAndGenerator
  ! "field1" [t|Text|]
  !% "attribute1" [t|Text|]
  ?% "attribute2" [t|Text|]

"Body" =:= record ParserAndGenerator
  ! "root" [t|XmlExample3|]
```

The following two types will be generated:

```haskell
data XmlExample3
  = XmlExample3 {_xe3Field1 :: !Text,
                 _xe3Attribute1 :: !Text,
                 _xe3Attribute2 :: !(Maybe Text)}

newtype XmlBody = XmlBody {_xbRoot :: XmlExample3}
```

The `_xe3Attribute2` is optional because we used `?%` modifier. Attributes will be
attached to parent XML element. Here is an example of the generated XML file:

```XML
<root attribute1="world">
    <field1>
	hello
    </field1>
</root>
```

Note that attributes are attached to the parent XML element, that's why we needed
`XmlBody` type here.

And here is what you get after parsing the XML:

```haskell
XmlExample3 {_xe3Field1 = "hello", _xe3Attribute1 = "world", _xe3Attribute2 = Nothing}
```

### Namespaces

Often XSD schema requires XML elements to be qualified with a namespace. To instruct
`xml-isogen` to qualify fields, specify namespace is a curly brackets:

```haskell
"Example4" =:= record ParserAndGenerator
  ! "field1" [t|Text|]
  ! "{http://example.com/1}field2" [t|Text|]
  ! "{http://example.com/2}field3" [t|Text|]
```

Here is the generated XML:

```XML
<field1>
    hello
</field1>
<field2 xmlns="http://example.com/1">
    world
</field2>
<field3 xmlns="http://example.com/2">
    !
</field3>
```

### Nillable types

Sometimes optional element in XML are encoded using `nil="true"` attribute instead of
omitting the element. (The `nil` attribute comes from `http://www.w3.org/2001/XMLSchema-instance` namespace). With `xml-isogen` you handle it using the `Nillable` type:

```haskell
"Example5" =:= record ParserAndGenerator
  ! "field" [t|Nillable Text|]
```

If the field contains the value `Nothing`, like this

```haskell
XmlExample5 { _xe5Field = Nillable Nothing}
```

then the following XML will be generated:

```XML
<field
  xmlns:ns="http://www.w3.org/2001/XMLSchema-instance"
  ns:nil="true"
/>
```

## Development

To start working with `xml-isogen` using nix use:

```
nix-shell --packages cabal2nix --run "cabal2nix ." > default.nix
nix-shell
cabal v2-build
```
