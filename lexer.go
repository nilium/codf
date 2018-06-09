package codf

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"math/big"
	"regexp"
	"strconv"
	"strings"
	"time"
	"unicode"
)

// DefaultPrecision is the default precision of TDecimal tokens produced by Lexer.
// This can be overridden in the Lexer by setting its Precision field to a non-zero value.
const DefaultPrecision = 80

// ErrUnexpectedEOF is returned by the Lexer when EOF is encountered mid-token where a valid token
// cannot be cut off.
var ErrUnexpectedEOF = errors.New("unexpected EOF")

const eof rune = -1

// TokenKind is an enumeration of the kinds of tokens produced by a Lexer and consumed by a Parser.
type TokenKind uint

func (t TokenKind) String() string {
	i := int(t)
	if i < 0 || len(tokenNames) <= i {
		return "invalid"
	}
	return tokenNames[t]
}

// Lex-able Token kinds encountered in codf.
const (
	tEmpty = TokenKind(iota)

	TEOF // !.

	TWhitespace // [ \n\r\t]+
	TComment    // '\'' ( !EOL . )* ( EOL | EOF )

	TWord // BarewordInitial BarewordTail*

	TSemicolon    // ';'
	TCurlOpen     // '{'
	TCurlClose    // '}'
	TBracketOpen  // '['
	TBracketClose // ']'

	TMapOpen // '#{'
	TRegexp  // '#/' ( '\\/' | [^/] )* '/'
	TString  // '"' ( Escape | [^"] )* '"'

	// TBoolean is produced by the parser transforming a boolean TWord into a TBoolean with
	// a corresponding bool value.
	TBoolean // Title/lower/UPPER of: 'true' | 'false' | 'yes' | 'no

	// All numbers may begin with '-' (negative) or '+' (positive).
	// Numbers without signs are positive.

	// Leading zeroes are only permitted on octal numbers or following the 'b', 'x', or '#' of
	// a base number. For example, 10#00001 is the integer 1.

	TInteger  // '0' | [1-9] [0-9]*
	TDecimal  // Integer '.' Integer Exponent? | Integer Exponent
	THex      // '0x' [a-fA-F0-9]+
	TOctal    // '0' [0-7]+
	TBinary   // '0' [bB] [01]+
	TBaseInt  // 2-36 '#' [a-zA-Z0-9]+ (corresponding to base)
	TDuration // 1m1s1h...
	TRational // Integer '/' Integer
)

var tokenNames = []string{
	tEmpty: "empty",

	TEOF: "EOF",

	TWhitespace: "whitespace",
	TComment:    "comment",

	TWord: "word",

	TSemicolon:    ";",
	TCurlOpen:     "{",
	TCurlClose:    "}",
	TBracketOpen:  "[",
	TBracketClose: "]",

	TMapOpen: "#{",
	TRegexp:  "regexp",
	TString:  "string",

	TBoolean: "bool",

	TInteger:  "integer",
	TDecimal:  "decimal",
	THex:      "hex",
	TOctal:    "octal",
	TBinary:   "binary",
	TBaseInt:  "baseint",
	TDuration: "duration",
	TRational: "rational",
}

// Token is a token with a kind and a start and end location.
// Start, end, and raw fields are considered metadata and should not be used by a parser except to
// provide information to the user.
//
// Kind is a TokenKind, such as TWord, and Value is the corresponding value of that TokenKind.
// Depending on the Kind, the Token must have a Value of the types described below. For all other
// TokenKinds not in the table below, a Value is not expected.
//
//      | Kind      | Value Type     |
//      |-----------+----------------|
//      | TWord     | string         |
//      | TString   | string         |
//      | TRegexp   | *regexp.Regexp |
//      | TBoolean  | bool           |
//      | TDecimal  | *big.Float     |
//      | TRational | *big.Rat       |
//      | TInteger  | *big.Int       |
//      | THex      | *big.Int       |
//      | TOctal    | *big.Int       |
//      | TBinary   | *big.Int       |
//      | TBaseInt  | *big.Int       |
//      | TDuration | time.Duration  |
//
type Token struct {
	Start, End Location
	Kind       TokenKind
	Raw        []byte
	Value      interface{}
}

// Location describes a location in an input byte sequence.
type Location struct {
	Offset int // A byte offset into an input sequence. Starts at 0.
	Line   int // A line number, delimited by '\n'. Starts at 1.
	Column int // A column number. Starts at 1.
}

func (l Location) String() string {
	return strconv.Itoa(l.Line) +
		":" + strconv.Itoa(l.Column) +
		"@" + strconv.Itoa(l.Offset)
}

func (l Location) add(r rune, size int) Location {
	if size < 1 {
		return l
	}
	l.Offset += size
	l.Column++
	if r == '\n' {
		l.Line++
		l.Column = 1
	}
	return l
}

type scanResult struct {
	r    rune
	size int
	err  error
}

var noToken Token

// Lexer takes an input sequence of runes and constructs Tokens from it.
type Lexer struct {
	Precision uint

	scanner io.RuneScanner

	pending  bool
	lastScan scanResult
	lastPos  Location

	startPos Location
	pos      Location

	next consumerFunc

	buf    bytes.Buffer
	strbuf bytes.Buffer
}

// NewLexer allocates a new Lexer that reads runes from r.
func NewLexer(r io.RuneScanner) *Lexer {
	le := &Lexer{
		scanner: r,
		pos:     Location{Line: 1, Column: 1},
	}
	return le
}

// ReadToken returns a token or an error. If EOF occurs, a TEOF token is returned without an error,
// and will be returned by all subsequent calls to ReadToken.
func (l *Lexer) ReadToken() (tok Token, err error) {
	l.reset()
	if l.next == nil {
		l.next = l.lexSegment
	}

	l.startPos = l.scanPos()

	var r rune
	for {
		r, err = l.readRune()
		if err != nil {
			return tok, err
		}

		tok, l.next, err = l.next(r)
		if err != nil || tok.Kind != tEmpty {
			return tok, err
		}
	}
}

type convertFunc func(Token) (Token, error)

func (l *Lexer) valueToken(kind TokenKind, convert convertFunc) (tok Token, err error) {
	tok = l.token(kind, true)
	if convert != nil {
		tok, err = convert(tok)
	}
	return tok, err
}

func (l *Lexer) token(kind TokenKind, takeBuffer bool) Token {
	var txt []byte
	if buflen := l.buf.Len(); buflen > 0 && takeBuffer {
		txt = make([]byte, buflen)
		copy(txt, l.buf.Bytes())
	} else if takeBuffer {
		txt = []byte{}
	}
	l.buf.Reset()
	tok := Token{
		Start: l.startPos,
		End:   l.scanPos(),
		Kind:  kind,
		Raw:   txt,
	}
	if takeBuffer {
		tok.Value = l.strbuf.String()
		l.strbuf.Reset()
	}
	return tok
}

func (l *Lexer) readRune() (r rune, err error) {
	if l.pending {
		l.pending = false
		return l.lastScan.r, l.lastScan.err
	}

	var size int
	r, size, err = l.scanner.ReadRune()
	if err == io.EOF {
		r, size, err = eof, 0, nil
	}
	res := scanResult{r: r, size: size, err: err}
	l.lastScan, l.lastPos = res, l.pos
	if size > 0 {
		l.pos = l.pos.add(r, size)
	}
	return
}

// unread takes the last-scanned rune and tells the lexer to return it on the next call to readRune.
// This can be used to walk back a single readRune call.
func (l *Lexer) unread() {
	if l.pending {
		panic("unread() called with pending rune")
	}
	l.pending = true
}

func (l *Lexer) reset() {
	l.buf.Reset()
	l.strbuf.Reset()
}

func (l *Lexer) buffer(raw, str rune) {
	if raw >= 0 {
		l.buf.WriteRune(raw)
	}
	if str >= 0 {
		l.strbuf.WriteRune(str)
	}
}

func (l *Lexer) scanPos() Location {
	if l.pending {
		return l.lastPos
	}
	return l.pos
}

// Rune cases

func isBarewordInitial(r rune) bool {
	return unicode.IsGraphic(r) &&
		!isBarewordReserved(r) &&
		!isBarewordForbidden(r)
}

func isBarewordForbidden(r rune) bool {
	return r == '"' || isStatementSep(r) || unicode.IsControl(r)
}

func isBarewordReserved(r rune) bool {
	return r == '#' || r == '-' || r == '+' || isDecimal(r)
}

func isBarewordTail(r rune) bool {
	return unicode.IsGraphic(r) &&
		!isBarewordForbidden(r)
}

func isStatementSep(r rune) bool {
	return unicode.IsSpace(r) ||
		r == ';' ||
		r == '{' ||
		r == '}' ||
		r == '[' ||
		r == ']' ||
		r == '\''
}

func isLongIntervalInitial(r rune) bool {
	return r == 'n' || // 'ns'
		r == 'u' || // 'us'
		r == 'μ' // 'μs'
}

func isIntervalInitial(r rune) bool {
	return r == 's' || // 's'
		r == 'n' || // 'ns'
		r == 'h' || // 'h'
		r == 'm' || // 'ms' | 'm'
		r == 'u' || // 'us'
		r == 'μ' // 'μs'
}

func isMaybeLongIntervalInitial(r rune) bool {
	return r == 'm' // 'ms' | 'm'
}

func isSign(r rune) bool {
	return r == '-' || r == '+'
}

func isDecimal(r rune) bool {
	return '0' <= r && r <= '9'
}

func isBinary(r rune) bool {
	return r == '0' || r == '1'
}

func isOctal(r rune) bool {
	return '0' <= r && r <= '7'
}

func isHex(r rune) bool {
	return ('0' <= r && r <= '9') ||
		('a' <= r && r <= 'f') ||
		('A' <= r && r <= 'F')
}

// Branches

type consumerFunc func(rune) (Token, consumerFunc, error)

func (l *Lexer) lexSpace(next consumerFunc) consumerFunc {
	var spaceConsumer consumerFunc
	spaceConsumer = func(r rune) (Token, consumerFunc, error) {
		if !unicode.IsSpace(r) {
			l.unread()
			return l.token(TWhitespace, false), next, nil
		}
		return noToken, spaceConsumer, nil
	}
	return spaceConsumer
}

func (l *Lexer) lexComment(next consumerFunc) consumerFunc {
	var commentConsumer consumerFunc
	commentConsumer = func(r rune) (Token, consumerFunc, error) {
		if r == '\n' || r == eof {
			if r == eof {
				l.unread()
			}
			return l.token(TComment, true), next, nil
		}
		l.buffer(r, -1)
		return noToken, commentConsumer, nil
	}
	return commentConsumer
}

func (l *Lexer) lexSegment(r rune) (Token, consumerFunc, error) {
	switch {
	// EOF
	case r == eof:
		return l.token(TEOF, false), l.lexSegment, nil

	// Whitespace
	case unicode.IsSpace(r):
		return noToken, l.lexSpace(l.lexSegment), nil

	// Semicolon
	case r == ';':
		return l.token(TSemicolon, false), l.lexSegment, nil

	// Braces
	case r == '{':
		return l.token(TCurlOpen, false), l.lexSegment, nil
	case r == '}':
		return l.token(TCurlClose, false), l.lexSegment, nil

	// Brackets
	case r == '[':
		return l.token(TBracketOpen, false), l.lexSegment, nil
	case r == ']':
		return l.token(TBracketClose, false), l.lexSegment, nil

	// Comment
	case r == '\'':
		return noToken, l.lexComment(l.lexSegment), nil

	// Map / regexp (#// | #{})
	case r == '#':
		return noToken, l.lexSpecial, nil

	// Word
	case isBarewordInitial(r):
		l.buffer(r, r)
		return noToken, l.lexWordTail(l.lexSegmentTail), nil

	// Numerics (integer, decimal, rational, duration)
	case isSign(r):
		l.buffer(r, r)
		return noToken, l.lexSignedNumber, nil
	case r == '0':
		l.buffer(r, r)
		return noToken, l.lexZero, nil
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexNonZero, nil

	// String
	case r == '"':
		l.buffer(r, -1)
		return noToken, l.lexString, nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q at %v", r, l.pos)
}

func (l *Lexer) lexWordTail(next consumerFunc) consumerFunc {
	var wordConsumer consumerFunc
	wordConsumer = func(r rune) (Token, consumerFunc, error) {
		if isBarewordTail(r) {
			l.buffer(r, r)
			return noToken, wordConsumer, nil
		}
		l.unread()
		tok := l.token(TWord, true)
		tok.Value = string(tok.Raw)
		return tok, next, nil
	}
	return wordConsumer
}

func (l *Lexer) lexSegmentTail(r rune) (Token, consumerFunc, error) {
	l.unread()
	switch {
	case r == eof:
		return l.token(TEOF, false), nil, nil
	case isStatementSep(r):
		return noToken, l.lexSegment, nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected a name character", r)
}

func (l *Lexer) lexSignedNumber(r rune) (Token, consumerFunc, error) {
	if !isDecimal(r) {
		return noToken, nil, fmt.Errorf("unexpected character %q: expected number after sign", r)
	}
	l.buffer(r, r)
	if r == '0' {
		return noToken, l.lexZero, nil
	}
	return noToken, l.lexNonZero, nil
}

func parseBaseInt(base int) convertFunc {
	return func(t Token) (Token, error) {
		var x big.Int
		if _, ok := x.SetString(t.Value.(string), base); !ok {
			return t, fmt.Errorf("malformed base-%d integer: %q", base, t.Value)
		}
		t.Value = &x
		return t, nil
	}
}

func (l *Lexer) lexOctalNumber(r rune) (Token, consumerFunc, error) {
	switch {
	case isStatementSep(r):
		l.unread()
		tok, err := l.valueToken(TOctal, parseBaseInt(8))
		return tok, l.lexSegment, err
	case isOctal(r):
		l.buffer(r, r)
		return noToken, l.lexOctalNumber, nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected octal digit or separator", r)
}

func (l *Lexer) lexNoTerminate(next consumerFunc, expect string) consumerFunc {
	return func(r rune) (Token, consumerFunc, error) {
		switch {
		case r == eof:
			return noToken, l.lexNoTerminate(next, expect), ErrUnexpectedEOF
		case isStatementSep(r):
			return noToken, nil, fmt.Errorf("unexpected character %q: expect %s", r, expect)
		}
		return next(r)
	}
}

func (l *Lexer) lexHexNum(r rune) (Token, consumerFunc, error) {
	switch {
	case isStatementSep(r), r == eof:
		l.unread()
		tok, err := l.valueToken(THex, parseBaseInt(16))
		return tok, l.lexSegment, err
	case isHex(r):
		l.buffer(r, r)
		return noToken, l.lexHexNum, nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected hex digit or separator", r)
}

func (l *Lexer) lexBinNum(r rune) (Token, consumerFunc, error) {
	switch {
	case isStatementSep(r):
		l.unread()
		tok, err := l.valueToken(TBinary, parseBaseInt(2))
		return tok, l.lexSegment, err
	case isBinary(r):
		l.buffer(r, r)
		return noToken, l.lexBinNum, nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected binary digit or separator", r)
}

func parseRational(t Token) (Token, error) {
	var x big.Rat
	text := t.Value.(string)
	if _, ok := x.SetString(text); !ok {
		return t, fmt.Errorf("malformed rational %q", text)
	}
	t.Value = &x
	return t, nil
}

func (l *Lexer) lexRationalDenomInitial(r rune) (Token, consumerFunc, error) {
	switch {
	case r >= '1' && r <= '9':
		l.buffer(r, r)
		return noToken, l.lexRationalDenomTail, nil
	case r == eof:
		return noToken, l.lexRationalDenomInitial, ErrUnexpectedEOF
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected positive number", r)
}

func (l *Lexer) lexRationalDenomTail(r rune) (Token, consumerFunc, error) {
	switch {
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexRationalDenomTail, nil
	}
	l.unread()
	tok, err := l.valueToken(TRational, parseRational)
	return tok, l.lexSegment, err
}

func parseBigFloat(prec uint) convertFunc {
	if prec == 0 {
		prec = DefaultPrecision
	}

	return func(tok Token) (Token, error) {
		var f big.Float
		f.SetPrec(prec)
		text := tok.Value.(string)
		if _, ok := f.SetString(text); !ok {
			return tok, fmt.Errorf("malformed decimal %q", text)
		}
		tok.Value = &f
		return tok, nil
	}
}

func (l *Lexer) lexDecimalExponentUnsigned(r rune) (Token, consumerFunc, error) {
	switch {
	case r == eof:
		return noToken, l.lexDecimalExponentUnsigned, ErrUnexpectedEOF
	case r == '0': // End of float
		l.buffer(r, r)
		return noToken, l.lexDecimalEnd, nil
	case isSign(r) || isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexDecimalExponentSignedInitial, nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected sign or digit", r)
}

func (l *Lexer) lexDecimalExponentSignedTail(r rune) (Token, consumerFunc, error) {
	switch {
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexDecimalExponentSignedTail, nil
	case isStatementSep(r), r == eof:
		l.unread()
		tok, err := l.valueToken(TDecimal, parseBigFloat(l.Precision))
		return tok, l.lexSegment, err
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected digit or separator", r)
}

func (l *Lexer) lexDecimalExponentSignedInitial(r rune) (Token, consumerFunc, error) {
	if r == '0' {
		l.buffer(r, r)
		return noToken, l.lexDecimalEnd, nil
	}
	return l.lexDecimalExponentSignedTail(r)
}

func (l *Lexer) lexDecimalEnd(r rune) (Token, consumerFunc, error) {
	if !isStatementSep(r) {
		return noToken, nil, fmt.Errorf("unexpected character %q: expected separator", r)
	}
	l.unread()
	tok, err := l.valueToken(TDecimal, parseBigFloat(l.Precision))
	return tok, l.lexSegment, err
}

func (l *Lexer) lexDecimalPointInitial(r rune) (Token, consumerFunc, error) {
	// Must have at least one trailing number
	switch {
	case r == eof:
		return noToken, l.lexDecimalPointInitial, ErrUnexpectedEOF
	case !isDecimal(r):
		return noToken, nil, fmt.Errorf("unexpected character %q: expected digit, exponent, or separator", r)
	}
	return l.lexDecimalPoint(r)
}

func (l *Lexer) lexDecimalPoint(r rune) (Token, consumerFunc, error) {
	switch {
	case isStatementSep(r):
		l.unread()
		tok, err := l.valueToken(TDecimal, parseBigFloat(l.Precision))
		return tok, l.lexSegment, err
	case r == 'E' || r == 'e': // exponent
		l.buffer(r, r)
		return noToken, l.lexDecimalExponentUnsigned, nil
	case isIntervalInitial(r):
		return l.lexIntervalConsumer(r)
	case !isDecimal(r):
		return noToken, nil, fmt.Errorf("unexpected character %q: expected digit, exponent, or separator", r)
	}
	l.buffer(r, r)
	return noToken, l.lexDecimalPoint, nil
}

func parseDuration(tok Token) (Token, error) {
	text := tok.Value.(string)
	d, err := time.ParseDuration(text)
	if err != nil {
		return tok, fmt.Errorf("malformed duration %q: %s", text, err)
	}
	tok.Value = d
	return tok, nil
}

func (l *Lexer) lexIntervalConsumer(r rune) (Token, consumerFunc, error) {
	l.buffer(r, r)
	if isLongIntervalInitial(r) {
		return noToken, l.lexIntervalUnitLong, nil
	} else if isMaybeLongIntervalInitial(r) {
		return noToken, l.lexIntervalUnitMaybeLong, nil
	}
	return noToken, l.lexInterval, nil
}

func (l *Lexer) lexIntervalUnitMaybeLong(r rune) (Token, consumerFunc, error) {
	if isStatementSep(r) || r == eof {
		return l.lexInterval(r)
	} else if !(r == 's' || isDecimal(r)) {
		return noToken, nil, fmt.Errorf("unexpected character %q: expected digit or 's'", r)
	}
	l.buffer(r, r)
	return noToken, l.lexInterval, nil
}

func (l *Lexer) lexIntervalUnitLong(r rune) (Token, consumerFunc, error) {
	if r == eof {
		return noToken, l.lexIntervalUnitLong, ErrUnexpectedEOF
	} else if r != 's' {
		return noToken, nil, fmt.Errorf("unexpected character %q: expected 's'", r)
	}
	l.buffer(r, r)
	return noToken, l.lexInterval, nil
}

func (l *Lexer) lexIntervalDecimalInitial(r rune) (Token, consumerFunc, error) {
	switch {
	case r == eof:
		return noToken, l.lexIntervalDecimalInitial, ErrUnexpectedEOF
	case r == '0':
		l.buffer(r, r)
		return noToken, l.lexIntervalZero, nil
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexIntervalDecimalTail, nil
	}
	return noToken, l.lexIntervalDecimalInitial, fmt.Errorf("unexpected character %q: expected decimal number", r)
}

func (l *Lexer) lexIntervalDecimalTail(r rune) (Token, consumerFunc, error) {
	if r == eof {
		return noToken, l.lexIntervalDecimalTail, ErrUnexpectedEOF
	} else if isIntervalInitial(r) {
		return l.lexIntervalConsumer(r)
	} else if isDecimal(r) {
		l.buffer(r, r)
		return noToken, l.lexIntervalDecimalTail, nil
	}
	return noToken, l.lexIntervalDecimalTail, fmt.Errorf("unexpected character %s: expected digit or interval unit", TDuration)
}

func (l *Lexer) lexIntervalZero(r rune) (Token, consumerFunc, error) {
	if r == eof {
		return noToken, l.lexIntervalZero, ErrUnexpectedEOF
	} else if isIntervalInitial(r) {
		return l.lexIntervalConsumer(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected interval unit", r)
}

func (l *Lexer) lexInterval(r rune) (Token, consumerFunc, error) {
	switch {
	case r == '.':
		l.buffer(r, r)
		return noToken, l.lexIntervalDecimalInitial, nil
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexInterval, nil
	case isStatementSep(r), r == eof:
		l.unread()
		tok, err := l.valueToken(TDuration, parseDuration)
		return tok, l.lexSegment, err
	case isIntervalInitial(r):
		return l.lexIntervalConsumer(r)
	}
	return noToken, l.lexInterval, fmt.Errorf("unexpected character %q: expected number or interval unit", r)
}

func (l *Lexer) lexZero(r rune) (Token, consumerFunc, error) {
	switch {
	case isStatementSep(r):
		l.unread()
		tok, err := l.valueToken(TInteger, parseBaseInt(10))
		return tok, l.lexSegment, err
	case isOctal(r):
		l.buffer(r, r)
		return noToken, l.lexOctalNumber, nil
	case r == '/':
		l.buffer(r, r)
		return noToken, l.lexRationalDenomInitial, nil
	case r == 'b' || r == 'B':
		l.buffer(r, -1)
		return noToken, l.lexNoTerminate(l.lexBinNum, "binary digit"), nil
	case r == 'x' || r == 'X':
		l.buffer(r, -1)
		return noToken, l.lexNoTerminate(l.lexHexNum, "hex digit"), nil
	case r == '.':
		l.buffer(r, r)
		return noToken, l.lexDecimalPointInitial, nil
	case isIntervalInitial(r):
		return l.lexIntervalConsumer(r)
	case r == 'E' || r == 'e':
		l.buffer(r, r)
		return noToken, l.lexDecimalExponentUnsigned, nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected b, x, X, octal, duration unit, or separator", r)
}

func (l *Lexer) lexNonZero(r rune) (Token, consumerFunc, error) {
	switch {
	case isStatementSep(r):
		l.unread()
		tok, err := l.valueToken(TInteger, parseBaseInt(10))
		return tok, l.lexSegment, err
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexNonZero, nil
	case isIntervalInitial(r):
		return l.lexIntervalConsumer(r)
	}

	switch r {
	case '#':
		l.buffer(r, -1)

		str := l.strbuf.String()
		neg := str[0] == '-'
		// Discard sign char in strbuf
		// Parse base number
		base, err := strconv.Atoi(strings.TrimLeft(str, "-+"))
		if err != nil {
			return noToken, nil, fmt.Errorf("malformed base number: %v", err)
		} else if base < 2 || base > 36 {
			return noToken, nil, fmt.Errorf("malformed base number %d: must be 2-36", base)
		}

		l.strbuf.Reset()
		return noToken, l.lexNoTerminate(l.lexBaseNumber(neg, base), fmt.Sprintf("base-%d digit", base)), nil
	case '/':
		l.buffer(r, r)
		return noToken, l.lexRationalDenomInitial, nil
	case '.':
		l.buffer(r, r)
		return noToken, l.lexDecimalPointInitial, nil
	case 'E', 'e':
		l.buffer(r, r)
		return noToken, l.lexDecimalExponentUnsigned, nil
	}

	return noToken, nil, fmt.Errorf("unexpected character %q: expected #, decimal, or separator", r)
}

func isBaseDigit(base int, r rune) bool {
	if base <= 10 {
		return r >= '0' && r < '0'+rune(base)
	}

	count := rune(base - 10)
	return (r >= '0' && r <= '9') ||
		(r >= 'a' && r < 'a'+count) ||
		(r >= 'A' && r < 'A'+count)
}

func (l *Lexer) lexBaseNumber(neg bool, base int) (consumer consumerFunc) {
	n := 0
	consumer = func(r rune) (Token, consumerFunc, error) {
		if !isBaseDigit(base, r) {
			l.unread()
			if n == 0 || (r != eof && !isStatementSep(r)) {
				return noToken, nil, fmt.Errorf("unexpected character %q: expected base-%d digit", r, base)
			}
			tok, err := l.valueToken(TBaseInt, parseBaseInt(base))
			if err == nil && neg {
				i := tok.Value.(*big.Int)
				i.Neg(i)
			}
			return tok, l.lexSegment, err
		}

		n++
		l.buffer(r, r)

		return noToken, consumer, nil
	}
	return consumer
}

func (l *Lexer) lexString(r rune) (Token, consumerFunc, error) {
	l.buffer(r, -1)
	switch r {
	case eof:
		return noToken, l.lexString, ErrUnexpectedEOF
	case '\\':
		return noToken, l.lexStringEscape, nil
	case '"':
		return l.token(TString, true), l.lexSegment, nil
	}
	l.buffer(-1, r)
	return noToken, l.lexString, nil
}

func (l *Lexer) lexStringEscape(r rune) (Token, consumerFunc, error) {
	next := l.lexString
	switch r {
	case eof:
		return noToken, l.lexStringEscape, ErrUnexpectedEOF
	case 'a':
		l.buffer(r, '\a')
	case 'b':
		l.buffer(r, '\b')
	case 'f':
		l.buffer(r, '\f')
	case 'n':
		l.buffer(r, '\n')
	case 'r':
		l.buffer(r, '\r')
	case 't':
		l.buffer(r, '\t')
	case 'v':
		l.buffer(r, '\v')
	case '\\':
		l.buffer(r, '\\')
	case '"':
		l.buffer(r, '"')
	case 'x': // 2 hex digits
		l.buffer(r, -1)
		next = l.lexHexStringEscape(1, func(u uint32) { l.strbuf.WriteByte(byte(u)) })
	case 'u': // 4 hex digits
		l.buffer(r, -1)
		next = l.lexHexStringEscape(2, func(u uint32) { l.strbuf.WriteRune(rune(u)) })
	case 'U': // 8 hex digits
		l.buffer(r, -1)
		next = l.lexHexStringEscape(4, func(u uint32) { l.strbuf.WriteRune(rune(u)) })
	case '0', '1', '2', '3', '4', '5', '6', '7': // 3 octal digits
		l.unread()
		next = l.lexOctalStringEscape()
	default:
		return noToken, nil, fmt.Errorf("invalid escape character %q", r)
	}
	return noToken, next, nil
}

func (l *Lexer) lexOctalStringEscape() (consumer consumerFunc) {
	var (
		final byte
		want  = 3
	)
	consumer = func(r rune) (Token, consumerFunc, error) {
		if r == eof {
			return noToken, consumer, ErrUnexpectedEOF
		} else if !isOctal(r) {
			return noToken, nil, fmt.Errorf("unexpected character %q: expected octal digit", r)
		}
		l.buffer(r, -1)
		final = (final << 3) | byte(r-'0')
		if want--; want > 0 {
			return noToken, consumer, nil
		}
		l.strbuf.WriteByte(final)
		return noToken, l.lexString, nil
	}
	return consumer
}

func xtoi(r rune) byte {
	switch {
	case r >= '0' && r <= '9':
		return byte(r - '0')
	case r >= 'A' && r <= 'F':
		return byte(0xa + r - 'A')
	case r >= 'a' && r <= 'f':
		return byte(0xa + r - 'a')
	}
	panic("unreachable")
}

func (l *Lexer) lexHexStringEscape(numbytes int, write func(final uint32)) (consumer consumerFunc) {
	var final uint32
	want := numbytes * 2
	consumer = func(r rune) (Token, consumerFunc, error) {
		if r == eof {
			return noToken, consumer, ErrUnexpectedEOF
		} else if !isHex(r) {
			return noToken, nil, fmt.Errorf("unexpected character %q: expected hex digit", r)
		}
		l.buffer(r, -1)
		final = (final << 4) | uint32(xtoi(r))
		if want--; want > 0 {
			return noToken, consumer, nil
		}
		write(final)
		return noToken, l.lexString, nil
	}
	return consumer
}

func (l *Lexer) lexSpecial(r rune) (Token, consumerFunc, error) {
	switch r {
	case '{':
		return l.token(TMapOpen, false), l.lexSegment, nil
	case '/':
		l.buffer('#', -1)
		l.buffer(r, -1)
		return noToken, l.lexRegexp, nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q after #: expected { or /", r)
}

func (l *Lexer) lexEscapeRegexp(r rune) (Token, consumerFunc, error) {
	if r != '/' {
		l.buffer(-1, '\\')
	}
	l.buffer(r, r)
	return noToken, l.lexRegexp, nil
}

func parseRegexp(tok Token) (Token, error) {
	rx, err := regexp.Compile(tok.Value.(string))
	if err == nil {
		tok.Value = rx
	}
	return tok, err
}

func (l *Lexer) lexRegexp(r rune) (Token, consumerFunc, error) {
	switch r {
	case eof:
		return noToken, l.lexRegexp, ErrUnexpectedEOF
	case '\\':
		l.buffer(r, -1)
		return noToken, l.lexEscapeRegexp, nil
	case '/':
		l.buffer(r, -1)
		tok, err := l.valueToken(TRegexp, parseRegexp)
		return tok, l.lexSegment, err
	}
	l.buffer(r, r)
	return noToken, l.lexRegexp, nil

}
