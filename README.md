codf
====
[![Build Status](https://travis-ci.org/nilium/codf.svg?branch=master)](https://travis-ci.org/nilium/codf)
[![codecov](https://codecov.io/gh/nilium/codf/branch/master/graph/badge.svg)](https://codecov.io/gh/nilium/codf)
[![GoDoc](https://godoc.org/go.spiff.io/codf?status.svg)](https://godoc.org/go.spiff.io/codf)


    $ go get -u go.spiff.io/codf

codf is a personal config language for declaring structured data with
support for a somewhat-wide range of built-in types.

The root codf package only covers lexing, parsing, and the AST right
now.



Language
--------

A codf document is a UTF-8 text document containing statements and
sections. Sections, in turn, contain further statements and sections.
Documents may also contain comments, but are not part of a document's
structure or AST.


### Statements

A statement is a name followed by optional values and terminated by
a semicolon (;):

    enable-gophers;
    enable-gophers yes;
    enable-gophers yes with-butter;
    ; ' This line doesn't have a statement -- a semicolon on its own is
      ' an empty statement and does nothing.


### Sections

A section is a name followed by optional values and braces, enclosing
further sections or statements:

    outer-section {
        inner-section /some/path {
            ' ...
        }

        enable-gophers yes;
    }


### Parameters

Both sections and statements may have an optional set of values
following their name, as above. These are called parameters for lack of
a better term.

Parameters These must be one of the types described below.


### Comments

A comment begins with an apostrophe and extends to the nearest line
ending (a linefeed / 0x0A byte):

    ' This is a comment

Comments are not included in the parsed AST and may not be used to
influence configuration.


### Types

Supported value types are integers, floats, rationals, durations,
strings, booleans, regular expressions, arrays, and maps.


#### Integers

Integers can be written in base 10, 16, 8, and 2, as well as arbitrary
bases in the range of 2-36:

    base-10 12345;
    base-16 0x70f; ' 1807
    base-8  0712;  ' 458
    base-2  0b101; ' 5
    base-3  3#210; ' 21
    base-36 36#zz; ' 1295

Integers are arbitarily long and represented as a `*big.Int`.

#### Floats / Decimals

Floats are written either as integers with exponents or decimal numbers
(with optional exponent):

    float-decimal   1.23456;    ' positive
    float-exponent  -123456e-5; ' negative
    float-big       1.23456789e200;

Floats are represented using a `*big.Float`. Precision can be adjusted
by changing the Parser's Precision field -- if 0, it defaults to
DefaultPrecision.

#### Rationals

Rationals are expressed as numerator/denominator, similar to lisps. It
is illegal to use a denominator of zero (0):

    rational -5/40; ' -1/8
    rational 0/40;  ' 0/1

Rationals are represented using a `*big.Rat`.

#### Durations

Durations are expressed as a sequence of integers or decimal numbers
followed by an interval unit (ns, us, ms, s, m, or h). This is
compatible with the Go stdlib's durations, but does not allow decimals
beginning with a period as Go does (e.g., ".5s" -- this has to be
written as "0.5s" in codf). As with Go, it's valid to use "µs" or "us"
for microseconds.

    durations 0s -1s 1h 500ms;  ' 0s -1s 1h0m0s 500ms
    decimals  0.5us 0.5s 0.5ms; ' 500ns 500ms 500µs

Durations are represented using `time.Duration`.

#### Strings

Strings take three forms: double-quoted sequences of characters, raw
strings, and barewords.

##### Double-quoted strings
Double-quoted strings are surrounded by double quotes ("...") and permit
all Go string escape codes (such as \n or \Uhhhhhhhh). In addition, in
contrast to Go, newlines in double-quoted strings are permitted without
escaping them.

    simple-string "foobar";
    escapes       "foo\nbar"; ' "foo\nbar"
    newline       "foo
    bar";                     ' "foo\nbar"

##### Raw strings 
Raw strings are surrounded by backquotes (or backticks -- the "`"
character). Like Go raw string literals, raw strings can contain almost
anything. Unlike Go raw string literals, a backquote can be escaped
inside of a raw string by writing two of them: "``". For example:

    empty           ``;           ' ""
    with-quotes     `"foobar"`;   ' "\"foobar\""
    with-backquotes ```foobar```; ' "`foobar`"

##### Barewords
Barewords are unquoted strings and usually more convenient than other
strings.

A bareword is any text that begins with a Unicode graphical character
minus syntactically-important characters: decimal numbers, quotes,
semicolons, braces, pound, and plus/minus. The rest of a bareword may
contain decimal numbers, pound, and plus/minus -- semicolons, braces,
and quotes are still reserved.

    leading-dot .dot;           ' ".dot"
    symbols     $^--;           ' "$^--"
    slashes     /foo/bar;       ' "/foo/bar"
    commas      Hello, World;   ' "Hello," "World" -- two strings
    unicode     こんにちは世界; ' "こんにちは世界"

It is not possible to write a bareword that uses a boolean keyword
except as a statement name (described below).

Barewords are represented as `string`.

#### Booleans

Booleans can be represented using the following values:

|             | True    | False   |
| ----------- | ------- | ------- |
| **Keyword** | `TRUE`  | `FALSE` |
|             | `True`  | `False` |
|             | `true`  | `false` |
|             | `YES`   | `NO`    |
|             | `Yes`   | `No`    |
|             | `yes`   | `no`    |

All keywords can be written in lowercase, uppercase, or titlecase. For
example:

    t-values YES True true; ' true true true
    f-values FALSE No no;   ' false false false

Other case combinations are not valid (i.e., booleans keywords
case-sensitive).

Booleans can only occur in parameters to statements and sections. For
example, the bareword "true" as a statement name is just the string
"true". The bareword "true" in an array or as a map key or value is the
boolean `true` (and not permitted in map keys).

Booleans are represented as `bool`.

#### Regular Expressions

Regular expressions are written as #/regex/, where internal /s can be
escaped using \/. These are treated as re2 regular expressions and
parsed using the stdlib regexp package.

    empty-regex  #//;
    simple-regex #/foo/;
    slash-regex  #/foo\/bar/;

Regular expressions are represented as `*regexp.Regexp`.

#### Arrays

Arrays are ordered lists of values between square brackets (`[]`).
Values are delimited by whitespace or other sentinel tokens (such as
brackets and comments):

    empty-array [];
    numbers     [1 2 3];
    nested      [[1 2] [3 4]];

Any of the above value types can be held by an array.

An array in the AST is represented as an `*Array`, which contains
a sequence of `[]ExprNode`.

#### Maps

Maps are unordered sets of space-delimited key-value pairs between curly
braces, prefixed by a pound (`#{}`). Key-value pairs in a map are
written as `KEY VALUE` (minus quotes), where each KEY must be followed
by a VALUE (separated by a space). For example:

    empty-map #{};
    normal-map #{
        ' Key    Value
        foo      1234      ' "foo" => 1234
        "bar"    #/baz/    ' "bar"  => #/baz/
    };

Map keys must be strings, either bare or quoted. If a key occurs more
than once in a map, only the last value is kept.

Maps are represented as a `*Map`, which contains a map of strings to
`*MapEntry`. Each `*MapEntry` contains the original key, value, and the
order that it was parsed in -- as above, codf maps are unordered, so
ordering is intended only to be kept for reformatting and other tools
right now.



License
-------

codf is licensed under the BSD two-clause license.
The license can be read in the COPYING file.



% vim: set tw=72 sw=4 et :
