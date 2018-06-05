                                  codf
                                  ====

    $ go get -u go.spiff.io/codf

codf is a personal config language for declaring structured data.

The parser is generated from a PEG to get a quick and dirty
implementation and semi-formal specification of the language. The code
itself is untested and mostly run through the demo package to prod the
parser.

The parser is generated using the https://github.com/pointlander/peg
tool. You can regenerate it by running one of the following:

    # If the .peg.go exists:
    $ go generate
    # If the .peg.go is gone:
    $ peg codf.peg



                                Language
                                --------

               *** THE FOLLOWING IS SUBJECT TO CHANGE ***

The language supports two forms of structure: statements and sections.

A statement is a name followed by optional values and terminated by
a semicolon (;):

    enable-gophers;
    enable-gophers yes;
    enable-gophers yes with-butter;
    ; ' This line doesn't have a statement -- a semicolon on its own is
      ' an empty statement and does nothing.

A section is a name followed by optional values and braces, enclosing
further sections or statements:

    outer-section {
        inner-section /some/path {
            ' ...
        }

        enable-gophers yes;
    }

In addition, a comment begins with an apostrophe and extends to the
nearest line ending (CRLF, LF, or CR):

    ' This is a comment

Supported value types are integers, floats, rationals, durations,
strings, booleans, regular expressions, arrays, and maps.

Integers
--------

Integers can be written in base 10, 16, 8, and 2, as well as arbitrary
bases in the range of 2-36:

    base-10 12345;
    base-16 0x70f; ' 1807
    base-8  0712;  ' 458
    base-2  0b101; ' 5
    base-3  3#210; ' 21
    base-36 36#zz; ' 1295

Integers are arbitarily long and represented as a *big.Int.

Floats
------

Floats are written either as integers with exponents or decimal numbers
(with optional exponent):

    float-decimal   1.23456;    ' positive
    float-exponent  -123456e-5; ' negative
    float-big       1.23456789e200;

Floats are represented using a *big.Float. Precision can be adjusted by
changing the Parser's Precision field -- if 0, it defaults to
DefaultPrecision.

Rationals
---------

Rationals are expressed as numerator/denominator, similar to lisps. It
is illegal to use a denominator of zero (0):

    rational -5/40; ' -1/8
    rational 0/40;  ' 0/1

Durations
---------

Durations are expressed as a sequence of integers or decimal numbers
followed by an interval unit (ns, us, ms, s, m, or h). This is
compatible with the Go stdlib's durations, but does not allow decimals
beginning with a period as Go does (e.g., ".5s" -- this has to be
written as "0.5s" in codf). As with Go, it's valid to use "µs" or "us"
for microseconds.

    durations 0s -1s 1h 500ms;  ' 0s -1s 1h0m0s 500ms
    decimals  0.5us 0.5s 0.5ms; ' 500ns 500ms 500µs

Strings
-------

Strings take two forms: double-quoted sequences of characters and
barewords.

Double-quoted strings are surrounded by double quotes ("...") and permit
all Go string escape codes (such as \n or \Uhhhhhhhh). In addition, in
contrast to Go, newlines in double-quoted strings are permitted without
escaping them.

    simple-string "foobar";
    escapes       "foo\nbar"; ' "foo\nbar"
    newline       "foo
    bar";                     ' "foo\nbar"

Barewords, on the other hand, are unquoted strings. A bareword is any
text that begins with a Unicode graphical character minus
syntactically-important characters: decimal numbers, quotes, semicolons,
braces, pound, and plus/minus. The rest of a bareword may contain
decimal numbers, pound, and plus/minus -- semicolons, braces, and quotes
are still reserved.

    leading-dot .dot;           ' ".dot"
    symbols     $^--;           ' "$^--"
    slashes     /foo/bar;       ' "/foo/bar"
    unicode     こんにちは世界; ' "こんにちは世界"
    commas      Hello, World;   ' "Hello," "World" -- two strings

It is not possibl to write a bareword that uses a boolean keyword
(described below).

Booleans
--------

Booleans can be represented using the following values:

            | True  | False |
    --------+-------+-------+
    Keyword | true  | false |
            | yes   | no    |

All keywords can be written in lowercase, uppercase, or titlecase. For
example:

    t-values YES True true; ' true true true
    f-values FALSE No no;   ' false false false

Regular Expressions
-------------------

Regular expressions are written as #/regex/, where internal /s can be
escaped using \/. These are treated as re2 regular expressions and
parsed using the stdlib regexp package.

    empty-regex  #//;
    simple-regex #/foo/;
    slash-regex  #/foo\/bar/;

Arrays
------

Arrays are ordered lists of space-delimited values between square
brackets ([]):

    empty-array [];
    numbers     [1 2 3];
    nested      [[1 2] [3 4]];

Any of the above value types can be held by an array.

Maps
----

Maps are unordered sets of space-delimited key-value pairs between curly
braces, prefixed by a pound. Key-value pairs in a map are written as
`KEY VALUE` (minus quotes), where each KEY must be followed by a VALUE
(separated by a space). For example:

    empty-map #{};
    normal-map #{
        ' Key    Value
        foo      1234      ' "foo" => 1234
        "bar"    #/baz/    ' "bar"  => #/baz/
    };

Map keys must be strings. If a key occurs more than once in a map, only
the last value is kept.


---
% vim: set tw=72 sw=4 et :
