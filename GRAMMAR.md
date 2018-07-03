codf Grammar
===

This document describes the codf grammar.

In the case of parsing rules (non-lexical), all cases implicitly allow
any run of whitespace and comments between significant tokens.
Whitespace and comment tokens are non-significant except for the purpose
of distinguishing the start and end of some tokens.

Any token that is ended with whitespace may also end at EOF without
whitespace.

Any token that becomes invalid due to a non-sentinel character being
introduced (i.e., characters that are not punctuation or whitespace)
should be treated as a Word token by the lexer.


Lexical Rules
---

### Punctuation

    CurlOpen     : '{' ;
    CurlClose    : '}' ;
    BracketOpen  : '[' ;
    BracketClose : ']' ;
    Semicolon    : ';' ;
    Newline      : '\n' ;

Punctuation is used to break up tokens and signify the start and end of
statements and sections.


### Whitespace

    Whitespace
        : ' '
        | '\t'
        | '\n'
        | '\v'
        | '\f'
        | '\r'
        | '\u0085'
        | '\u00a0'
        | \p{White_Space}
        ;

Whitespace is any run of characters considered whitespace by Go's
unicode.IsSpace function:

> IsSpace reports whether the rune is a space character as defined by
> Unicode's White Space property; in the Latin-1 space this is
>
> `'\t', '\n', '\v', '\f', '\r', ' ', U+0085 (NEL), U+00A0 (NBSP).`
>
> Other definitions of spacing characters are set by category Z and
> property Pattern_White_Space.

Each run of whitespace produces a whitespace token.


### Comments

    Comment
        : "'" { ~Newline } Newline
        ;

A comment is a token beginning with an apostrophe, continuing through
zero or more non-newline characters, and ending with a newline (0xA or
'\n'). Carriage returns (0xD or '\r') are not considered a newline and
are part of the comment's text.

Comments may end at EOF without a terminating newline.


### Numbers

Numbers are lexed according to the type of Number they match.
The Number token below is for convenience in lexing.

    Number
        : [ Sign ]
        (
            Integer
            | Binary
            | Hex
            | Octal
            | BaseInteger
            | Float
            | Rational
        )
        ;

    Sign     : '-' | '+' ;
    PosDigit : '1'..'9' ;
    DecDigit : '0'..'9' ;

    Integer    : '0' | PosInt ;
    PosInt     : DecDigit { DecDigit } ;

Leading zeroes are not permitted in ordinary base-10 integers, floats,
or rationals. The same rule applies to durations, since a lexeme that
begins as an integer or float can become a duration.

    BaseInt    : Base '#' BaseDigit { BaseDigit } ;
    Base       : Integer ;

BaseDigit is any digit permitted for bases 2 through 36, depending on
the integer value of the Base. Leading zeroes are permitted in base
numbers, including base-10 (e.g., `10#0001`).

    Hex        : '0' ( 'X' | 'x' ) HexDigit { HexDigit } ;
    HexDigit   : '0'..'9' | 'a'..'f' | 'A'..'F' ;

    Octal      : '0' OctalDigit { OctalDigit } ;
    OctalDigit : '0'..'7' ;

    Binary     : '0' ( 'b' | 'B' ) BinDigit { BinDigit } ;
    BinDigit   : '0' | '1' ;

The prefix base-numbers for hex, octal, and binary allow leading zeroes,
unlike normal integers, floats, and so on.
    


<!-- vim: set tw=72 et -->
