package codf // import "go.spiff.io/codf"

import (
	"bufio"
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

// DefaultPrecision is the default precision of TFloat tokens produced by Lexer.
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

	// BarewordRune := ![;{}\[\]"'`] Unicode(L,M,N,P,S)

	TWhitespace   // [ \n\r\t]+
	TComment      // '\'' { !EOL . } ( EOL | EOF )
	TWord         // BarewordRune {BarewordRune}
	TSemicolon    // ';'
	TCurlOpen     // '{'
	TCurlClose    // '}'
	TBracketOpen  // '['
	TBracketClose // ']'
	TMapOpen      // '#{'
	TRegexp       // '#/' { '\\/' | [^/] } '/'

	// Strings also include TWord, above, which is an unquoted string.
	// Escape := '\\' ( [abfnrtv\\"] | 'x' Hex2 | 'u' Hex4 | 'U' Hex8 | Oct3 )
	TString    // '"' ( Escape | [^"] )* '"'
	TRawString // '`' ( '``' | [^`] )* '`'

	// TBoolean is produced by the parser transforming a boolean TWord into a TBoolean with
	// a corresponding bool value.
	TBoolean // Title/lower/UPPER of: 'true' | 'false' | 'yes' | 'no

	// All numbers may begin with '-' (negative) or '+' (positive).
	// Numbers without signs are positive.

	// Leading zeroes are only permitted on octal numbers or following the 'b', 'x', or '#' of
	// a base number. For example, 10#00001 is the integer 1.

	TInteger  // '0' | [1-9] [0-9]*
	TFloat    // Integer '.' Integer Exponent? | Integer Exponent
	THex      // '0' [Xx] [a-fA-F0-9]+
	TOctal    // '0' [0-7]+
	TBinary   // '0' [bB] [01]+
	TBaseInt  // 2-36 '#' [a-zA-Z0-9]+ (corresponding to base)
	TDuration // 1m1.033s1h...
	TRational // Integer '/' Integer
)

var tokenNames = []string{
	tEmpty: "empty",

	TEOF: "EOF",

	TWhitespace: "whitespace",
	TComment:    "comment",

	TWord: "word",

	TSemicolon:    "semicolon",
	TCurlOpen:     "open brace",
	TCurlClose:    "close brace",
	TBracketOpen:  "open bracket",
	TBracketClose: "close bracket",

	TMapOpen: "map",
	TRegexp:  "regexp",

	TString:    "string",
	TRawString: "raw string",

	TBoolean: "bool",

	TInteger:  "integer",
	TFloat:    "float",
	THex:      "hex integer",
	TOctal:    "octal integer",
	TBinary:   "binary integer",
	TBaseInt:  "base integer",
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
//      | TFloat    | *big.Float     |
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
	Name   string // Name is an identifier, usually a file path, for the location.
	Offset int    // A byte offset into an input sequence. Starts at 0.
	Line   int    // A line number, delimited by '\n'. Starts at 1.
	Column int    // A column number. Starts at 1.
}

func (l Location) String() string {
	pos := strconv.Itoa(l.Line) + ":" + strconv.Itoa(l.Column) + ":" + strconv.Itoa(l.Offset)
	if l.Name != "" {
		return l.Name + ":" + pos
	}
	return pos
}

func (l Location) add(r rune, size int) Location {
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

// NamedReader is an optional interface that an io.Reader can implement to provide a name for its
// data source.
type NamedReader interface {
	io.Reader

	// Name returns a non-empty string identifying the reader's data source. This may be a file,
	// URL, resource ID, or some other thing. If the returned string is empty, it will be
	// treated as unnamed.
	Name() string
}

var noToken Token

// Special lexer runes
const (
	rSentinel     = ';'
	rCurlOpen     = '{'
	rCurlClose    = '}'
	rBracketOpen  = '['
	rBracketClose = ']'
	rDoubleQuote  = '"'
	rBackQuote    = '`'
	rSpecial      = '#'
	rComment      = '\''
	rDot          = '.'
	rFracSep      = '/'
	rBaseSep      = '#'
	rRegexpOpen   = '/'
	rRegexpClose  = '/'
)

// Lexer takes an input sequence of runes and constructs Tokens from it.
type Lexer struct {
	// Precision is the precision used in *big.Float when taking the actual value of a TFloat
	// token.
	Precision uint
	// Name is the name of the token source currently being lexed. It is used to identify the
	// source of a location by name. It is not necessarily a filename, but usually is.
	//
	// If the scanner provided to the Lexer implements NamedScanner, the scanner's name takes
	// priority.
	Name string

	scanner io.RuneReader

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
func NewLexer(r io.Reader) *Lexer {
	rr := runeReader(r)

	le := &Lexer{
		scanner: rr,
		pos:     Location{Line: 1, Column: 1},
	}
	return le
}

type nameRuneReader struct {
	*bufio.Reader
	namefn func() string
}

func (n nameRuneReader) Name() string {
	return n.namefn()
}

func runeReader(r io.Reader) io.RuneReader {
	switch r := r.(type) {
	case io.RuneReader:
		return r
	case NamedReader:
		return nameRuneReader{bufio.NewReader(r), r.Name}
	default:
		return bufio.NewReader(r)
	}
}

// ReadToken returns a token or an error. If EOF occurs, a TEOF token is returned without an error,
// and will be returned by all subsequent calls to ReadToken.
func (l *Lexer) ReadToken() (tok Token, err error) {
	l.reset()
	if l.next == nil {
		l.next = l.lexSegment
	}

	if l.pos == (Location{Line: 1, Column: 1}) {
		l.pos.Name = l.posName()
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
	const invalid rune = '\uFFFD'

	if l.pending {
		l.pending = false
		return l.lastScan.r, l.lastScan.err
	}

	var size int
	l.pos.Name = l.posName()
	r, size, err = l.scanner.ReadRune()
	if err == io.EOF {
		r, size, err = eof, 0, nil
	}
	res := scanResult{r: r, size: size, err: err}
	l.lastScan, l.lastPos = res, l.pos
	if size > 0 {
		l.pos = l.pos.add(r, size)
	}

	if r == invalid && err == nil {
		err = fmt.Errorf("invalid UTF-8 at %v", l.pos)
	}

	return
}

func (l *Lexer) posName() string {
	if named, ok := l.scanner.(NamedReader); ok {
		if name := named.Name(); name != "" {
			return name
		}
	}
	return l.Name
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

var barewordTables = []*unicode.RangeTable{
	unicode.L, // Letters
	unicode.M, // Marks
	unicode.N, // Numbers
	unicode.P, // Punctuation
	unicode.S, // Symbols
}

// isBarewordRune returns true if r is valid inside of a bareword (but not necessarily if the lexer
// can start a bareword from r initially).
func isBarewordRune(r rune) bool {
	return unicode.In(r, barewordTables...) &&
		!isBarewordForbidden(r)
}

// isBarewordTransition returns true if r is valid inside of a token that is not a bareword but
// would become one by consuming r.
func isBarewordTransition(r rune) bool {
	return unicode.In(r, barewordTables...) &&
		!isStatementSep(r)
}

// isBarewordForbidden returns true if r is one of the characters that may not appear in a bareword.
func isBarewordForbidden(r rune) bool {
	return isWordSep(r) || unicode.IsControl(r)
}

func isWordSep(r rune) bool {
	return unicode.IsSpace(r) ||
		r == rSentinel || // End statement
		r == rDoubleQuote || // Quoted string
		r == rBackQuote // Raw string
}

func isStatementSep(r rune) bool {
	return unicode.IsSpace(r) ||
		r == rSentinel || // End statement
		r == rCurlOpen || // Begin section (in statement)
		r == rCurlClose || // Close map (in statement)
		r == rBracketOpen || // Open array
		r == rBracketClose || // Close array
		r == rDoubleQuote || // Quoted string
		r == rBackQuote // Raw string
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

func isNonZero(r rune) bool {
	return r >= '1' && r <= '9'
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
	return isDecimal(r) ||
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
	case r == rSentinel:
		return l.token(TSemicolon, false), l.lexSegment, nil

	// Braces
	case r == rCurlOpen:
		return l.token(TCurlOpen, false), l.lexSegment, nil
	case r == rCurlClose:
		return l.token(TCurlClose, false), l.lexSegment, nil

	// Brackets
	case r == rBracketOpen:
		return l.token(TBracketOpen, false), l.lexSegment, nil
	case r == rBracketClose:
		return l.token(TBracketClose, false), l.lexSegment, nil

	// Comment
	case r == rComment:
		return noToken, l.lexComment(l.lexSegment), nil

	// Map / regexp (#// | #{})
	case r == rSpecial:
		return noToken, l.lexSpecial, nil

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
	case r == rDoubleQuote:
		l.buffer(r, -1)
		return noToken, l.lexString, nil
	case r == rBackQuote:
		l.buffer(r, -1)
		return noToken, l.lexRawString, nil

	// Word
	case isBarewordRune(r):
		return l.lexBecomeWord(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q at %v", r, l.pos)
}

func (l *Lexer) lexWordTail(next consumerFunc) consumerFunc {
	var wordConsumer consumerFunc
	var braces int
	wordConsumer = func(r rune) (Token, consumerFunc, error) {
		switch {
		case r == rCurlOpen || r == rBracketOpen:
			braces++
			l.buffer(r, r)
			return noToken, wordConsumer, nil
		case (r == rCurlClose || r == rBracketClose):
			if braces <= 0 {
				break
			}
			braces--
			fallthrough
		case isBarewordRune(r):
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

func (l *Lexer) lexBecomeWord(r rune) (Token, consumerFunc, error) {
	if r >= 0 {
		l.buffer(r, r)
	}
	return noToken, l.lexWordTail(l.lexSegmentTail), nil
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
	switch {
	case isDecimal(r):
		l.buffer(r, r)
		if r == '0' {
			return noToken, l.lexZero, nil
		}
		return noToken, l.lexNonZero, nil
	case isStatementSep(r) || r == eof:
		l.unread()
		return l.lexBecomeWord(-1)
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected number after sign", r)
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
	case isOctal(r):
		l.buffer(r, r)
		return noToken, l.lexOctalNumber, nil
	case isStatementSep(r) || r == eof:
		l.unread()
		tok, err := l.valueToken(TOctal, parseBaseInt(8))
		return tok, l.lexSegment, err
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
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
	case isHex(r):
		l.buffer(r, r)
		return noToken, l.lexHexNum, nil
	case isStatementSep(r) || r == eof:
		l.unread()
		tok, err := l.valueToken(THex, parseBaseInt(16))
		return tok, l.lexSegment, err
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected hex digit or separator", r)
}

func (l *Lexer) lexBinNum(r rune) (Token, consumerFunc, error) {
	switch {
	case isBinary(r):
		l.buffer(r, r)
		return noToken, l.lexBinNum, nil
	case isStatementSep(r) || r == eof:
		l.unread()
		tok, err := l.valueToken(TBinary, parseBaseInt(2))
		return tok, l.lexSegment, err
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
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
	case isNonZero(r):
		l.buffer(r, r)
		return noToken, l.lexRationalDenomTail, nil
	case isStatementSep(r) || r == eof:
		l.unread()
		return l.lexBecomeWord(-1)
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected positive number", r)
}

func (l *Lexer) lexRationalDenomTail(r rune) (Token, consumerFunc, error) {
	switch {
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexRationalDenomTail, nil
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	case isStatementSep(r) || r == eof:
		l.unread()
		tok, err := l.valueToken(TRational, parseRational)
		return tok, l.lexSegment, err
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected rational number", r)
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

func (l *Lexer) lexFloatExponentUnsigned(r rune) (Token, consumerFunc, error) {
	//
	// Occurs after an 'e' or 'E', indicating that a float has an exponent. This is before any
	// digit of the exponent has been consumed.
	//
	// '0'          -> lex decimal end
	// [1-9]        -> lex signed tail (implicit positive sign)
	// '-' | '+'    -> lex signed initial (explicit sign, no digit)
	// Sep          -> Bareword
	// BarewordRune -> lex bareword
	//
	switch {
	case r == '0': // End of float
		l.buffer(r, r)
		return noToken, l.lexFloatEnd, nil
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexFloatExponentSignedTail, nil
	case isSign(r):
		l.buffer(r, r)
		return noToken, l.lexFloatExponentSignedInitial, nil
	case r == eof || isStatementSep(r):
		l.unread()
		return l.lexBecomeWord(-1)
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected sign or digit", r)
}

func (l *Lexer) lexFloatExponentSignedTail(r rune) (Token, consumerFunc, error) {
	//
	// Occurs in the middle of a signed float exponent (meaning either implicitly or explicitly
	// signed).
	//
	switch {
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexFloatExponentSignedTail, nil
	case isStatementSep(r) || r == eof:
		l.unread()
		tok, err := l.valueToken(TFloat, parseBigFloat(l.Precision))
		return tok, l.lexSegment, err
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected digit or separator", r)
}

func (l *Lexer) lexFloatExponentSignedInitial(r rune) (Token, consumerFunc, error) {
	//
	// Occurs after an '-' or '+' in a float's exponent.
	//
	// '0' -> lex decimal end
	// Sep -> Bareword
	// _   -> lex decimal exponent signed tail
	//
	if r == '0' {
		l.buffer(r, r)
		return noToken, l.lexFloatEnd, nil
	} else if isStatementSep(r) || r == eof {
		l.unread()
		return l.lexBecomeWord(-1)
	}
	return l.lexFloatExponentSignedTail(r)
}

func (l *Lexer) lexFloatEnd(r rune) (Token, consumerFunc, error) {
	//
	// Occurs after a '0' in an exponent, indicating that the number must necessarily end. If
	// r is not a separator and is a valid bareword rune, it becomes a bareword.
	//
	// Sep          -> Float
	// BarewordRune -> lex bareword
	//
	// Any other character following 1e0 is invalid (e.g., the string "1e0\x00" cannot be
	// lexed).
	//
	switch {
	case r == eof || isStatementSep(r):
		l.unread()
		tok, err := l.valueToken(TFloat, parseBigFloat(l.Precision))
		return tok, l.lexSegment, err
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected separator", r)
}

func (l *Lexer) lexFloatPointInitial(r rune) (Token, consumerFunc, error) {
	//
	// Occurs after a '.' while lexing an integer -- the token is lexed as a float from then on.
	//
	// [0-9]        -> lex decimal point
	// Sep          -> Bareword
	// BarewordRune -> lex bareword
	//
	switch {
	case isDecimal(r):
	case r == eof || isStatementSep(r):
		l.unread()
		return l.lexBecomeWord(-1)
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	}
	return l.lexFloatPoint(r)
}

func (l *Lexer) lexFloatPoint(r rune) (Token, consumerFunc, error) {
	//
	// Occurs after a '.' and at least one digit while lexing an integer -- the token is lexed
	// as a float from then on.
	//
	// [Ee]         -> lex float from exponent
	// IntervalUnit -> lex interval unit (lexed as interval from then on)
	// [0-9]        -> continue
	// Sep          -> Float
	// BarewordRune -> lex bareword
	//
	switch {
	case r == 'E' || r == 'e': // exponent
		l.buffer(r, r)
		return noToken, l.lexFloatExponentUnsigned, nil
	case isIntervalInitial(r):
		return l.lexIntervalConsumer(r)
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexFloatPoint, nil
	case isStatementSep(r) || r == eof:
		l.unread()
		tok, err := l.valueToken(TFloat, parseBigFloat(l.Precision))
		return tok, l.lexSegment, err
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected digit, exponent, or separator", r)
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

// lexIntervalConsumer returns the next consumerFunc for a given interval unit, depending on whether
// the unit is necessarily long (two runes), maybe long (one to two runes), or short (one rune).
func (l *Lexer) lexIntervalConsumer(r rune) (Token, consumerFunc, error) {
	l.buffer(r, r)
	if isLongIntervalInitial(r) {
		return noToken, l.lexIntervalUnitLong, nil
	} else if isMaybeLongIntervalInitial(r) {
		return noToken, l.lexIntervalUnitMaybeLong, nil
	}
	return noToken, l.lexIntervalInitial, nil
}

func (l *Lexer) lexIntervalUnitMaybeLong(r rune) (Token, consumerFunc, error) {
	//
	// Occurs after an 'm' begins an interval unit and expects either an 's' or a digit.
	// If an 's' occurs, the unit is milliseconds. If a digit occurs, the unit is seconds.
	// It follows at least one digit.
	//
	// 's' | [0-9]  -> lex interval initial
	// Sep          -> Interval
	// BarewordRune -> lex bareword
	//
	switch {
	case r == 's' || isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexIntervalInitial, nil
	case isStatementSep(r) || r == eof:
		return l.lexIntervalInitial(r)
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected digit or 's'", r)
}

func (l *Lexer) lexIntervalUnitLong(r rune) (Token, consumerFunc, error) {
	//
	// Occurs after an 'n', 'u', or 'μ' in an interval and expects an 's'.
	// It follows at least one digit.
	//
	// 's'          -> lex interval initial
	// Sep          -> Bareword
	// BarewordRune -> lex bareword
	//
	switch {
	case r == 's':
		l.buffer(r, r)
		return noToken, l.lexIntervalInitial, nil
	case isStatementSep(r) || r == eof:
		l.unread()
		return l.lexBecomeWord(-1)
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected 's'", r)
}

func (l *Lexer) lexIntervalFloatInitial(r rune) (Token, consumerFunc, error) {
	//
	// Occurs after a '.' in an interval (e.g., "1s0." -- any initial '.' in a number is handled
	// by lexFloatPointInitial and does not affect intervals except in parsing as a duration).
	//
	// IntervalUnit -> lex bareword
	// _            -> lex interval float tail
	//
	// This function exists primarily to handle the case where an interval unit occurs and
	// converts the interval to a bareword because it becomes invalid.
	//
	if isIntervalInitial(r) {
		return l.lexBecomeWord(r)
	}
	return l.lexIntervalFloatTail(r)
}

func (l *Lexer) lexIntervalFloatTail(r rune) (Token, consumerFunc, error) {
	//
	// Occurs after a decimal point in an interval number and before an interval initial.
	//
	// [0-9]        -> continue
	// IntervalUnit -> lex interval unit
	// BarewordRune -> lex bareword
	// Sep          -> Bareword
	//
	switch {
	case isIntervalInitial(r):
		return l.lexIntervalConsumer(r)
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexIntervalFloatTail, nil
	case isStatementSep(r) || r == eof:
		l.unread()
		return l.lexBecomeWord(-1)
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	}
	return noToken, l.lexIntervalFloatTail, fmt.Errorf("unexpected character %s: expected digit or interval unit", TDuration)
}

func (l *Lexer) lexIntervalInitial(r rune) (Token, consumerFunc, error) {
	//
	// If a separator occurs, cut the token as a Duration.
	// Otherwise, defer to lexInterval.
	//
	if isStatementSep(r) || r == eof {
		l.unread()
		tok, err := l.valueToken(TDuration, parseDuration)
		return tok, l.lexSegment, err
	}
	return l.lexInterval(r)
}

func (l *Lexer) lexInterval(r rune) (Token, consumerFunc, error) {
	//
	// Lexes an interval in a state where the interval cannot be cut as a Duration.
	//
	// '.'          -> lex float interval (e.g., "0.5s")
	// [0-9]        -> continue
	// IntervalUnit -> lex interval unit
	// BarewordRune -> lex bareword
	// Sep          -> Bareword
	//
	switch {
	case r == rDot:
		l.buffer(r, r)
		return noToken, l.lexIntervalFloatInitial, nil
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexInterval, nil
	case isIntervalInitial(r):
		return l.lexIntervalConsumer(r)
	case isStatementSep(r) || r == eof:
		l.unread()
		return l.lexBecomeWord(-1)
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected number or interval unit", r)
}

func (l *Lexer) lexZero(r rune) (Token, consumerFunc, error) {
	//
	// Occurs after '0' was lexed as the initial digit of a number.
	//
	// Sep          -> Integer
	// EOF          -> Integer
	// [0-7]        -> lex octal
	// '/'          -> lex rational
	// [Bb]         -> lex binary
	// [Xx]         -> lex hexadecimal
	// '.'          -> lex float
	// 'Ee'         -> lex float from exponent (necessarily zero)
	// BarewordRune -> lex bareword
	//
	switch {
	case isStatementSep(r), r == -1:
		l.unread()
		tok, err := l.valueToken(TInteger, parseBaseInt(10))
		return tok, l.lexSegment, err
	case isOctal(r):
		l.buffer(r, r)
		return noToken, l.lexOctalNumber, nil
	case r == rFracSep:
		l.buffer(r, r)
		return noToken, l.lexRationalDenomInitial, nil
	case r == 'b' || r == 'B':
		l.buffer(r, -1)
		return noToken, l.lexNoTerminate(l.lexBinNum, "binary digit"), nil
	case r == 'x' || r == 'X':
		l.buffer(r, -1)
		return noToken, l.lexNoTerminate(l.lexHexNum, "hex digit"), nil
	case r == rDot:
		l.buffer(r, r)
		return noToken, l.lexFloatPointInitial, nil
	case isIntervalInitial(r):
		return l.lexIntervalConsumer(r)
	case r == 'E' || r == 'e':
		l.buffer(r, r)
		return noToken, l.lexFloatExponentUnsigned, nil
	case isBarewordTransition(r):
		return l.lexBecomeWord(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected b, x, X, octal, duration unit, or separator", r)
}

func (l *Lexer) lexNonZero(r rune) (Token, consumerFunc, error) {
	//
	// Occurs after [1-9] was lexed as the initial digit of a number.
	//
	// Sep          -> Integer
	// EOF          -> Integer
	// [0-9]        -> repeat
	// IntervalUnit -> lex interval
	// '#'          -> lex base number (base '#' {base-digit})
	// '/'          -> lex rational (integer '/' integer)
	// '.'          -> lex float from fraction (integer '.' digit {digit} [exponent])
	// [Ee]         -> lex float from exponent
	// BarewordRune -> lex bareword
	//
	switch {
	case isStatementSep(r), r == eof:
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
	case rBaseSep:
		l.buffer(r, -1)

		str := l.strbuf.String()
		neg := str[0] == '-'
		// Discard sign char in strbuf
		// Parse base number
		base, err := strconv.Atoi(strings.TrimLeft(str, "-+"))
		if err != nil || base < 2 || base > 36 {
			l.buffer(-1, r)
			return l.lexBecomeWord(-1)
		}

		l.strbuf.Reset()
		return noToken, l.lexBaseNumber(neg, base), nil
	case rFracSep:
		l.buffer(r, r)
		return noToken, l.lexRationalDenomInitial, nil
	case rDot:
		l.buffer(r, r)
		return noToken, l.lexFloatPointInitial, nil
	case 'E', 'e':
		l.buffer(r, r)
		return noToken, l.lexFloatExponentUnsigned, nil
	}

	if isBarewordTransition(r) {
		return l.lexBecomeWord(r)
	}

	return noToken, nil, fmt.Errorf("unexpected character %q: expected #, decimal point, or separator", r)
}

func isBaseDigit(base int, r rune) bool {
	if base <= 10 {
		return r >= '0' && r < '0'+rune(base)
	}

	count := rune(base - 10)
	return isDecimal(r) ||
		(r >= 'a' && r < 'a'+count) ||
		(r >= 'A' && r < 'A'+count)
}

func (l *Lexer) lexBaseNumber(neg bool, base int) (consumer consumerFunc) {
	//
	// Consume one or more runes that are valid for the given base until a separator is found.
	//
	// Otherwise, if the rune is either a separator or a bareword rune, convert the token to
	// a bareword.
	//
	// In all other cases, return an error.
	//
	n := 0
	consumer = func(r rune) (Token, consumerFunc, error) {
		if isBaseDigit(base, r) {
			n++
			l.buffer(r, r)
			return noToken, consumer, nil
		} else if isBarewordTransition(r) {
			return l.lexBecomeWord(r)
		} else if n == 0 && (isStatementSep(r) || r == eof) {
			l.unread()
			return l.lexBecomeWord(-1)
		}

		l.unread()
		if !(isStatementSep(r) || r == eof) {
			// An example for this case is "4#\x00", since a NUL won't cover any valid
			// case for this.
			return noToken, nil, fmt.Errorf("unexpected character %q: expected base-%d digit", r, base)
		}

		tok, err := l.valueToken(TBaseInt, parseBaseInt(base))
		if err == nil && neg {
			i := tok.Value.(*big.Int)
			i.Neg(i)
		}
		return tok, l.lexSegment, err
	}
	return consumer
}

func (l *Lexer) lexRawString(r rune) (Token, consumerFunc, error) {
	//
	// Consume any rune between `s without filtering except for `` (which is an escaped `).
	//
	l.buffer(r, -1)
	switch r {
	case eof:
		return noToken, l.lexRawString, ErrUnexpectedEOF
	case rBackQuote:
		return noToken, l.lexRawStringEscape, nil
	}
	l.buffer(-1, r)
	return noToken, l.lexRawString, nil
}

func (l *Lexer) lexRawStringEscape(r rune) (Token, consumerFunc, error) {
	//
	// If the next rune is a '`', it is an escaped backquote.
	// Otherwise, the RawString has ended.
	//
	if r == rBackQuote {
		l.buffer(r, r)
		return noToken, l.lexRawString, nil
	}
	l.unread()
	return l.token(TRawString, true), l.lexSegment, nil
}

func (l *Lexer) lexString(r rune) (Token, consumerFunc, error) {
	//
	// Consume runes until an ending double-quote or backslash for escapes is found.
	//
	l.buffer(r, -1)
	switch r {
	case eof:
		return noToken, l.lexString, ErrUnexpectedEOF
	case '\\':
		return noToken, l.lexStringEscape, nil
	case rDoubleQuote:
		return l.token(TString, true), l.lexSegment, nil
	}
	l.buffer(-1, r)
	return noToken, l.lexString, nil
}

func (l *Lexer) lexStringEscape(r rune) (Token, consumerFunc, error) {
	//
	// Consume a rune to determine the kind of escape that should be handled.
	//
	// Must accept all Go escapes valid in double quotes.
	//
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
	case rDoubleQuote:
		l.buffer(r, rDoubleQuote)
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
		return l.lexOctalStringEscape()(r)
	default:
		return noToken, nil, fmt.Errorf("invalid escape character %q", r)
	}
	return noToken, next, nil
}

func (l *Lexer) lexOctalStringEscape() (consumer consumerFunc) {
	//
	// Read three octal digits and buffer the resulting byte (after truncating it to 8 bits).
	//
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
	case isDecimal(r):
		return byte(r - '0')
	case r >= 'A' && r <= 'F':
		return byte(0xa + r - 'A')
	case r >= 'a' && r <= 'f':
		return byte(0xa + r - 'a')
	}
	panic("unreachable")
}

func (l *Lexer) lexHexStringEscape(numbytes int, write func(final uint32)) (consumer consumerFunc) {
	//
	// Read up to numbytes worth of hex digits (2*numbytes) and, upon successfully reading all
	// digits, write the resulting byte using the write func.
	//
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
	//
	// lexSpecial occurs when a '#' is the first character read.
	// The initial '#' is not buffered.
	//
	// '{'          -> MapOpen
	// '/'          -> lex regexp
	// BarewordRune -> lex bareword
	// Sep          -> Bareword
	//
	switch {
	case r == rCurlOpen:
		return l.token(TMapOpen, false), l.lexSegment, nil
	case r == rRegexpOpen:
		l.buffer(rSpecial, -1)
		l.buffer(r, -1)
		return noToken, l.lexRegexp, nil
	case isStatementSep(r) || r == eof:
		l.buffer(rSpecial, rSpecial)
		l.unread()
		return l.lexBecomeWord(-1)
	case isBarewordRune(r):
		l.buffer(rSpecial, rSpecial)
		return l.lexBecomeWord(r)
	}
	return noToken, nil, fmt.Errorf("unexpected character %q after #: expected { or /", r)
}

func (l *Lexer) lexEscapeRegexp(r rune) (Token, consumerFunc, error) {
	//
	// Escapes forward slashes in Regexp tokens. If the character following a backslash is not
	// a forward slash, the backslash is buffered as part of the regexp (e.g., so that /\d+/
	// does not require escaping).
	//
	if r != rRegexpClose {
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
	//
	// Reads regular expression tokens until the next forward slash ('/') or backslash ('\')
	//
	// Only forward slashes may be escaped.
	//
	switch r {
	case eof:
		return noToken, l.lexRegexp, ErrUnexpectedEOF
	case '\\':
		l.buffer(r, -1)
		return noToken, l.lexEscapeRegexp, nil
	case rRegexpClose:
		l.buffer(r, -1)
		tok, err := l.valueToken(TRegexp, parseRegexp)
		return tok, l.lexSegment, err
	}
	l.buffer(r, r)
	return noToken, l.lexRegexp, nil
}
