<!--
  Serialization utility functions

  Copyright (C) 2017, 2018 R-T Specialty, LLC.

    This file is part of the Liza Program UI Compiler.

    liza-proguic is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see
    <http://www.gnu.org/licenses/>.
-->

<stylesheet version="2.0"
            xmlns="http://www.w3.org/1999/XSL/Transform"
            xmlns:xs="http://www.w3.org/2001/XMLSchema"
            xmlns:struct="http://www.lovullo.com/liza/proguic/util/struct"
            xmlns:f="http://mikegerwitz.com/hoxsl/apply"
            xmlns:_struct="http://www.lovullo.com/liza/proguic/util/struct/_priv">

<import href="../hoxsl/src/apply.xsl" />

<!--
@node Serialization
@section Serialization
@cindex Serialization

@luic{} uses a primitive API for representing and serializing objects,
  most notably JSON (@pxref{JSON Transformation}).
This avoids having to handle string generation
  (and couple with an implementation) in various systems.
-->

<variable name="struct:error-qname" as="xs:QName"
          select="QName(
                    'http://www.lovullo.com/liza/proguic/util/struct/error',
                    'err:BADSTRUCT' )" />

<!--
@cindex Array
An @dfn{array} is an untyped list of items.
Usually,
  this provides @math{O(n)} lookups.
It is ideal for linear processing of data.

The term ``array'' is abused in certain languages;
  if you are looking for a key/value store, use
    @ref{struct:dict#1,,@code{struct-dict}}.
-->


<!--
  Generate an empty array.
-->
<function name="struct:array" as="element( struct:array )">
  <struct:array />
</function>


<!--
  Generate an untyped array of values.

  Arrays must contain only @xmlnode{struct:item} elements,
    but unlike dictionaries,
    they @emph{must not} contain a@tie{}@xmlattr{key}.
-->
<function name="struct:array" as="element( struct:array )">
  <param name="values" as="element( struct:item )*" />

  <struct:array>
    <sequence select="$values" />
  </struct:array>
</function>


<!--
@cindex Dictionary
A @dfn{dictionary} is a key/value store.
Like arrays,
  dictionaries contain items,
  but they are indexed by keys.
Usually,
  languages implement this as a hash table,
  providing @math{O(1)} lookups.
-->


<!--
  Create an empty dictionary.
-->
<function name="struct:dict" as="element( struct:dict )">
  <struct:dict />
</function>


<!--
  Create a dictionary of values.

  This is a key-value store containing only @xmlnode{struct:item}
    elements with @xmlattr{key} attributes (@pxref{struct:item#2}).
-->
<function name="struct:dict" as="element( struct:dict )">
  <param name="values" as="element( struct:item )*" />

  <struct:dict>
    <sequence select="$values" />
  </struct:dict>
</function>


<!--
@cindex Item
An @dfn{item} can be either @dfn{keyed} or @dfn{unkeyed}:
  the former is suitable only for dictionaries,
    while the latter is suitable only for arrays.

@devnotice{Item type metadata should be added;
             otherwise, we can only serialize as a string.}
-->


<!--
  Associate a value with a key in a dictionary.

  A key value may be a primitive value or another structure.
  Keyed items must be children of a@tie{}dictionary
    (@pxref{struct:dict#1,,@code{struct:dict}}).

  Attribute values are converted into strings.
-->
<function name="struct:item" as="element( struct:item )">
  <param name="value" />
  <param name="id" as="xs:string" />

  <struct:item key="{$id}">
    <sequence select="if ( $value instance of attribute() ) then
                          string( $value )
                        else
                          $value" />
  </struct:item>
</function>


<!--
  Create a keyless item.

  A key value may be a primitive value or another structure.
  Keyless items must be children of a@tie{}array
    (@pxref{struct:array#1,,@code{struct:array}}).

  Attribute values are converted into strings.
-->
<function name="struct:item" as="element( struct:item )">
  <param name="value" />

  <struct:item>
    <sequence select="if ( $value instance of attribute() ) then
                          string( $value )
                        else
                          $value" />
  </struct:item>
</function>

<!--
Since deriving item values from attributes is common,
  they will automatically be convered into strings.@footnote{
    Really, it makes no sense to permit attributes,
      since that will result in the attribute being assigned to the
      @xmlnode{struct:item} itself,
      which does not make any sense
        (and could corrupt internal state depending on what attribute
          was set).}
-->


<!--
@subsection Auto-Generating Structures

It's common (and natural) to want to serialize key/value pairs from
  attributes.
Two functions provide this convenience:
-->


<!--
  Generate keys from attributes.

  A key/value pair will be created for each attribute in @var{attrs}
  using the attribute's local name as the@tie{}key.  Whitespace in attribute
  values will be normalized.
-->
<function name="struct:items-from-attrs" as="element( struct:item )*">
  <param name="attrs" as="attribute()*" />

  <sequence select="for $attr in $attrs
                      return struct:item( normalize-space( $attr ),
                                          $attr/local-name() )" />
</function>


<!--
  Convert an element into a dictinary using its attributes as
    key/value pairs.

  The name of the element is not used.
  The attributes of the node are passed to
    @ref{struct:item#1,,@code{struct:item}}.
-->
<function name="struct:dict-from-attrs" as="element( struct:dict )">
  <param name="element" as="element()" />

  <sequence select="struct:dict(
                      struct:items-from-attrs( $element/@* ) )" />
</function>


<!--
  Convert a sequence of elements into an array of dictionaries using element
    attributes as dictionary key/value pairs.

  Each element is processed using
    @ref{struct:dict-from-attrs#1,,@code{struct:dict-from-attrs}}.
-->
<function name="struct:dict-array-from-elements" as="element( struct:array )">
  <param name="elements" as="element()*" />

  <sequence select="struct:array(
                      for $element in $elements
                        return struct:item(
                                 struct:dict-from-attrs( $element ) ) )" />
</function>


<!--
Another function allows allows using one of the attibutes as
  a@tie{}key to recursively generate a dictionary of multiple
  elements,
    provided that those elements have unquie keys.
-->

<!--
  Recurisvely generate dictionary using an attribute as a key.

  This generates a new dictionary with @code{n}@tie{}entires where the
    value of the key is another dictionary containing the key/value
    representation of the remaining attributes,
      where @code{n = count($element)}.

  If @var{$recf} is non-empty,
    it will be applied to each element that generates an item in the
    parent dictionary;
      this allows for recursive processing.
  The function is applied within the context of the dictionary and
    should therefore return one or more @xmlnode{struct:item}s.

  Beware: the given key @var{$key} is compared only by @code{local-name}.

  @emph{No check is performed to ensure they all keys are unique in
    the toplevel dictionary.}
  Conflicts result in undefined behavior dependent on the serializer.
  For example,
    when serialized to JSON,
    the latter key takes precedence and the former keys are overwritten.

  @devnotice{Should probably handle more gracefully a situation where the
               key attribute does not exist on one of the elements.}
-->
<function name="struct:dict-from-keyed-elements" as="element( struct:dict )">
  <param name="key"      as="xs:string" />
  <param name="elements" as="element()*" />
  <param name="recf"     as="item()*" />

  <sequence select="
    struct:dict(
      for $element in $elements
        return struct:item(
                 struct:dict(
                   ( struct:items-from-attrs(
                       $element/@*[ not( local-name() = $key ) ] ),
                     if ( $recf ) then
                         f:apply( $recf, $element )
                       else
                         () ) ),
                 $element/@*[ local-name() = $key ] ) )" />
</function>


<!--
  Recurisvely generate dictionary using an attribute as a key.

  This two-argument version simply invokes
    @coderef{struct:dict-from-keyed-elements#3} without a child function.
-->
<function name="struct:dict-from-keyed-elements" as="element( struct:dict )">
  <param name="key"      as="xs:string" />
  <param name="elements" as="element()*" />

  <sequence select="struct:dict-from-keyed-elements( $key, $elements, () )" />
</function>


<!--
An example usage of this function is provided in
  @ref{f:dict-from-keyed-elements}.

@float Figure, f:dict-from-keyed-elements
  Given some document:

  @example
    <meta>
      <field id="foo" desc="Has nested" type="string">
        <nested name="n1" />
      </field>
      <field id="bar" desc="No nested" type="boolean" />
    </meta>
  @end example

  With function:

  @example
  <function name="nestedf" as="element( struct:item )+">
    <param name="field" as="element( field )" />
    <sequence select="struct:item( $field/nested/@@name, 'nestedf' )" />
  </function>
  @end example

  Transformed with:

  @example
  <sequence select="struct:dict-from-keyed-elements( 'id', meta, nestedf() )" />
  @end example

  Results in:

  @example
  <struct:dict>
    <struct:item key="foo">
      <struct:dict>
        <struct:item key="desc">Has nested</struct:item>
        <struct:item key="type">string</struct:item>
        <struct:item key="nestedf">n1</struct:item>
      </struct:dict>
    </struct:item>
    <struct:item key="bar">
      <struct:dict>
        <struct:item key="desc">No nested</struct:item>
        <struct:item key="type">boolean</struct:item>
        <struct:item key="nestedf"></struct:item>
      </struct:dict>
    </struct:item>
  </struct:dict>
  @end example
@caption{Generating a dictionary from keyed elements.}
@end float


Extracting key/value pairs from element attributes is also a common
  operation:
-->


<!--
  Generate keyed items for each element in @var{$elements} using one
  attribute @var{$key}@tie{}as the key and another attribute
  @var{$value}@tie{}as the value.

  Beware: the given key @var{$key} is compared only by @code{local-name}.

  The arguments are ordered such that this is useful as a partially
  applied function for processing lists of elements with lambdas.
-->
<function name="struct:items-from-keyed-elements" as="element( struct:item )*">
  <param name="key"      as="xs:string" />
  <param name="value"    as="xs:string" />
  <param name="elements" as="element()*" />

  <sequence select="for $element in $elements
                      return struct:item(
                        $element/@*[ local-name() = $value ],
                        $element/@*[ local-name() = $key ] )" />
</function>


<!--
When generating dictionary items in a loop from numerous elements,
  it can be inconvenient keeping track of unique keys.
If the goal is to create an array of items grouped by unique keys,
  you're in luck:
-->


<!--
  Group keyed items into arrays indexed by their respective keys.

  Every unique key@tie{}@code{k} will result in an array@mdash{
    }indexed by@tie{}@code{k}@mdash{
    }containing each respective item.

  @emph{Items without keys will not be retained!}
-->
<function name="struct:group-items-by-key" as="element( struct:item )*">
  <param name="items" as="element( struct:item )*" />

  <for-each-group select="$items" group-by="@key">
    <struct:item key="{current-grouping-key()}">
      <struct:array>
        <sequence select="for $item in current-group()
                            return struct:item( $item/node() )" />
      </struct:array>
    </struct:item>
  </for-each-group>
</function>


<!--
@menu
* JSON Transformation:: Serializing to JSON.
@end menu
-->



<!--
@node JSON Transformation
@subsection JSON Transformation
@cindex JSON

The recommended way to serialize a structure as JSON is to apply
  @ref{struct:to-neo4j-attrs#1,,@code{struct:to-neo4j-attrs}}.
-->

<!--
  Transform structure into JSON.
-->
<function name="struct:to-neo4j-attrs" as="xs:string">
  <param name="struct" as="element()" />

  <variable name="result" as="xs:string*">
    <apply-templates mode="struct:to-neo4j-attrs"
                    select="$struct" />
  </variable>

  <!-- force to a single string rather than a sequence of them -->
  <sequence select="string-join( $result, '' )" />
</function>


<!--
We assume that the structure is already well-formed;@footnote{
    That might not necessarily be assured by this implementation,
      but validations belong there (@pxref{Serialization}), not here.}
  this makes serialization a trivial task.

We proceed by recursive descent.
Let's start with arrays.

@subsubsection Array Serialization

An array simply encapsulates items in square brackets:
-->

<!--
  Transform array into JSON.
-->
<template mode="struct:to-neo4j-attrs" priority="5"
          match="struct:array">
  <text>[</text>
    <apply-templates mode="struct:to-neo4j-attrs"
                     select="node()" />
  <text>]</text>
</template>

<!--
Items are simple too,
  since we don't have to deal with keys.
If the item contains an element,
  we consider it to be a nested structure and recurse:
-->

<!--
  Transform nested structure into JSON.
-->
<template mode="struct:to-neo4j-attrs" priority="5"
          match="struct:item[ element() ]">
  <sequence select="struct:to-neo4j-attrs( ./element() )" />

  <if test="following-sibling::struct:item">
    <sequence select="','" />
  </if>
</template>

<!--
  Otherwise, we consider it to be a primitive.
  At this point,
    items are untyped,
    so we have no choice but to serialize as a string:
-->

<!--
  Transform primitive data into JSON.

  Until items are typed, we have no choice but to serialize all items
    as strings.
-->
<template mode="struct:to-neo4j-attrs" priority="4"
          match="struct:item">
  <sequence select="concat(
                      '&quot;',
                      _struct:json-escape-str( . ),
                      '&quot;' )" />

  <if test="following-sibling::struct:item">
    <sequence select="','" />
  </if>
</template>

<!--
Note that we took care to escape the provided value so that double
  quotes do not break out of the serialized string.
-->

<!--
  Escape double quotes within a string.
-->
<function name="_struct:json-escape-str" as="xs:string">
  <param name="str" as="xs:string" />

  <sequence select="replace(
                      replace( $str, '\\', '\\\\' ), '&quot;', '\\&quot;' )" />
</function>


<!--
@subsubsection Dictionary Serialization

Dictionaries are serialized similarly.
In JSON,
  we represent them as objects:
-->


<!--
  Transform dictionary into JSON object.
-->
<template mode="struct:to-neo4j-attrs" priority="5"
          match="struct:dict">
  <text>{</text>
    <apply-templates mode="struct:to-neo4j-attrs-dict"
                     select="node()" />
  <text>}</text>
</template>

<!--
Since dictionaries are key/value,
  every item needs to be assigned to a field on the object,
  where field name is specified by @xmlattr{key}.
Otherwise,
  serialization proceeds the same way as arrays:
-->

<!--
  Transform dictionary key into JSON field on an object.

  The field name is specified by @xmlnode{struct:item/@@key}.
  Until items are typed, we have no choice but to serialize all items
    as strings.
-->
<template mode="struct:to-neo4j-attrs-dict" priority="5"
          match="struct:item[ @key ]">
  <sequence select="concat(_struct:neo4j-translate-prop( @key ),
                            ':' )" />

  <apply-templates mode="struct:to-neo4j-attrs"
                   select="." />
</template>


<function name="_struct:neo4j-translate-prop" as="xs:string">
  <param name="name" as="xs:string" />

  <sequence select="replace( $name, '-', '_' )" />
</function>


<!--
Note that we escape the field.

At a lower priority,
  we have a catch-all that will fail if it encounters a non-keyed
  structure:
-->

<!--
  Error on unrecognized structures during dictionary JSON
    transformation.
-->
<template mode="struct:to-neo4j-attrs-dict" priority="1"
          match="node()">
  <sequence select="error( $struct:error-qname,
                           concat( 'Unexpected non-key structure: ',
                                   name( . ) ),
                           . )"/>
</template>


<!--
@subsubsection Miscellaneous

We use comments in test cases to annotate structures.
It's unlikely that they will be used in practice,
  but since they are nodes too,
  we need to make sure we don't consider them to be errors:
-->

<!--
  Ignore comments during processing.
-->
<template mode="struct:to-neo4j-attrs" priority="2"
          match="comment()">
</template>

<!--
  Everything else we don't know about during processing results in an
    error:
-->

<!--
  Error on unrecognized structures during JSON transformation.
-->
<template mode="struct:to-neo4j-attrs" priority="1"
          match="node()">
  <sequence select="error( $struct:error-qname,
                           concat( 'Unexpected structure: ',
                                   string( . ) ),
                           . )"/>
</template>

<!--
And we're done.
-->

</stylesheet>
