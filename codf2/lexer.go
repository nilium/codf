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
	"unicode"
)

var ErrUnexpectedEOF = errors.New("unexpected EOF")

const eof rune = -1

type TokenKind uint

func (t TokenKind) String() string {
	i := int(t)
	if i < 0 || len(tokenNames) <= i {
		return "invalid"
	}
	return tokenNames[t]
}

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

	// TBoolean is produced when the lexer encounters a boolean word in the middle of
	// a statement (i.e., after the first word and before a ; or {)
	TBoolean // Title/lower/UPPER of: 'true' | 'false' | 'yes' | 'no

	// All numbers may implicitly begin with '-' or '+'

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

type Token struct {
	Start, End Location
	Kind       TokenKind
	Raw        []byte
	Value      interface{}
}

type Location struct {
	Offset int
	Line   int
	Column int
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

var errStop = errors.New("lexer: stopped")

var noToken Token

type embedType byte

const (
	embedArray = embedType(iota)
	embedMap
	embedSection
)

var embedTypeNames = []string{
	embedArray:   "array",
	embedMap:     "map",
	embedSection: "section",
}

func (e embedType) String() string {
	return embedTypeNames[e]
}

type Lexer struct {
	scanner io.RuneScanner

	pending  bool
	lastScan scanResult
	lastPos  Location

	startPos Location
	pos      Location

	next consumerFunc
	err  error

	embed  []embedType
	_embed [8]embedType

	buf    bytes.Buffer
	strbuf bytes.Buffer
}

func NewLexer(r io.RuneScanner) *Lexer {
	le := &Lexer{
		scanner: r,
		pos:     Location{Line: 1, Column: 1},
	}
	le.embed = le._embed[:0]
	return le
}

func (l *Lexer) ReadToken() (tok Token, err error) {
	if l.next == nil {
		l.next = l.lexSegment
	}

	l.startPos = l.scanPos()

	var r rune
	for {
		r, err = l.readRune()
		if err == io.EOF {
			tok, l.next, err = l.next(eof)
			if tok.Kind == tEmpty {
				err = ErrUnexpectedEOF
			}
			return tok, err
		} else if err != nil {
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

func (l *Lexer) peek() (r rune, err error) {
	if l.pending {
		return l.lastScan.r, l.lastScan.err
	}

	sym, _, err := l.scanner.ReadRune()
	if err != nil && err != io.EOF {
		return 0, err
	}
	if err == io.EOF {
		return eof, nil
	}
	return sym, l.scanner.UnreadRune()
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

// func (l *Lexer) take(conds ...func(rune) bool) (match int, r rune, err error) {
// 	r, err = l.peek()
// 	if err == io.EOF {
// 		r, err = eof, nil
// 	} else if err != nil {
// 		return -1, r, err
// 	}
// 	for i, cond := range conds {
// 		if cond(r) {
// 			r, err = l.readRune()
// 			return i, r, err
// 		}
// 	}
// 	return -1, 0, nil
// }

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

var isSpace = unicode.IsSpace

func isBarewordInitial(r rune) bool {
	switch r {
	case '=', '<', '>', '.', '?', '/', '!', '@', '$', '%', '^', '&', '*', ':', '|', '_':
		return true
	}
	return unicode.IsLetter(r)
}

func isBarewordTail(r rune) bool {
	// This is the set of characters for BarewordInitial with numbers and '-+#'.
	switch r {
	case '=', '<', '>', '.', '?', '/', '!', '@', '$', '%', '^', '&', '*', ':', '|', '_', '#', '-', '+':
		return true
	}
	return unicode.IsLetter(r) || unicode.IsNumber(r)
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
	case r == eof:
		return l.token(TEOF, false), l.lexSegment, nil
	case unicode.IsSpace(r):
		return noToken, l.lexSpace(l.lexSegment), nil
	case r == ';':
		return l.token(TSemicolon, false), l.lexSegment, nil
	case r == '}':
		if err := l.endEmbed(embedSection); err != nil {
			return noToken, nil, err
		}
		return l.token(TCurlClose, false), l.lexSegment, nil
	case r == '\'':
		return noToken, l.lexComment(l.lexSegment), nil
	case isBarewordInitial(r):
		l.buffer(r, r)
		return noToken, l.lexWordTail(false, l.lexSegmentTail), nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected a name", r)
}

func (l *Lexer) lexWordTail(parameter bool, next consumerFunc) consumerFunc {
	var wordConsumer consumerFunc
	wordConsumer = func(r rune) (Token, consumerFunc, error) {
		if isBarewordTail(r) {
			l.buffer(r, r)
			return noToken, wordConsumer, nil
		}
		l.unread()
		tok := l.token(TWord, true)
		tok.Value = string(tok.Raw)
		if parameter {
			switch tok.Value.(string) {
			case "TRUE", "True", "true", "YES", "Yes", "yes":
				tok.Kind, tok.Value = TBoolean, true
			case "FALSE", "False", "false", "NO", "No", "no":
				tok.Kind, tok.Value = TBoolean, false
			}
		}
		return tok, next, nil
	}
	return wordConsumer
}

func (l *Lexer) lexSegmentTail(r rune) (Token, consumerFunc, error) {
	l.unread()
	switch {
	case r == eof:
		return noToken, l.lexSegment, nil
	case unicode.IsSpace(r):
		return noToken, l.lexStatement, nil
	case r == ';':
		return noToken, l.lexSegment, nil
	case r == '{':
		return noToken, l.lexSectionOpen, nil
	case r == '\'':
		return noToken, l.lexStatement, nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected a name character", r)
}

func (l *Lexer) lexSignedNumber(r rune) (Token, consumerFunc, error) {
	if !isDecimal(r) {
		return noToken, nil, fmt.Errorf("unexpected character %q in parameter context: expected number after sign", r)
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
	case unicode.IsSpace(r),
		r == ';' || r == '{' || r == '\'':
		l.unread()
		tok, err := l.valueToken(TOctal, parseBaseInt(8))
		return tok, l.lexStatement, err
	case isOctal(r):
		l.buffer(r, r)
		return noToken, l.lexOctalNumber, nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected octal digit or separator", r)
}

func (l *Lexer) lexHexnum(r rune) (Token, consumerFunc, error) {
	switch {
	case unicode.IsSpace(r),
		r == ';' || r == '{' || r == '\'':
		l.unread()
		tok, err := l.valueToken(THex, parseBaseInt(16))
		return tok, l.lexStatement, err
	case isHex(r):
		l.buffer(r, r)
		return noToken, l.lexHexnum, nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected hex digit or separator", r)
}

func (l *Lexer) lexBinnum(r rune) (Token, consumerFunc, error) {
	switch {
	case unicode.IsSpace(r),
		r == ';' || r == '{' || r == '\'':
		l.unread()
		tok, err := l.valueToken(TBinary, parseBaseInt(2))
		return tok, l.lexStatement, err
	case isBinary(r):
		l.buffer(r, r)
		return noToken, l.lexBinnum, nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected binary digit or separator", r)
}

func (l *Lexer) lexZero(r rune) (Token, consumerFunc, error) {
	switch {
	case unicode.IsSpace(r),
		r == ';' || r == '{' || r == '\'' || r == '}' || r == ']':
		l.unread()
		tok, err := l.valueToken(TInteger, parseBaseInt(10))
		return tok, l.lexStatement, err
	case isOctal(r):
		l.buffer(r, r)
		return noToken, l.lexOctalNumber, nil
	case r == 'b' || r == 'B':
		l.buffer(r, -1)
		return noToken, l.lexBinnum, nil
	case r == 'x' || r == 'X':
		l.buffer(r, -1)
		return noToken, l.lexHexnum, nil
	}
	return noToken, nil, fmt.Errorf("unexpected character %q: expected b, x, X, octal, or separator", r)
}

func (l *Lexer) lexNonZero(r rune) (Token, consumerFunc, error) {
	switch {
	case unicode.IsSpace(r):
		l.unread()
		tok, err := l.valueToken(TInteger, parseBaseInt(10))
		return tok, l.lexStatement, err
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexNonZero, nil
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
		return noToken, l.lexBaseNumber(neg, base), nil
	case ';', '{', '\'', '}', ']':
		l.unread()
		tok, err := l.valueToken(TInteger, parseBaseInt(10))
		return tok, l.lexStatement, err
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
		if r == eof {
			return noToken, consumer, ErrUnexpectedEOF
		} else if !isBaseDigit(base, r) {
			l.unread()
			if n == 0 {
				return noToken, nil, fmt.Errorf("unexpected character %q: expected base-%d digit", r, base)
			}
			tok, err := l.valueToken(TBaseInt, parseBaseInt(base))
			if err == nil && neg {
				i := tok.Value.(*big.Int)
				i.Neg(i)
			}
			return tok, l.lexStatement, err
		}

		n++
		l.buffer(r, r)

		return noToken, consumer, nil
	}
	return consumer
}

func (l *Lexer) lexStatement(r rune) (Token, consumerFunc, error) {
	switch {
	case isSign(r):
		l.buffer(r, r)
		return noToken, l.lexSignedNumber, nil
	case r == '0':
		l.buffer(r, r)
		return noToken, l.lexZero, nil
	case isDecimal(r):
		l.buffer(r, r)
		return noToken, l.lexNonZero, nil
	case r == '\'':
		return noToken, l.lexComment(l.lexStatement), nil
	case r == ';':
		return l.token(TSemicolon, false), l.lexSegment, nil
	case r == '{':
		l.unread()
		return noToken, l.lexSectionOpen, nil
	case r == '#':
		l.reset()
		l.buffer(r, -1)
		return noToken, l.lexSpecial, nil
	case r == '[':
		l.beginEmbed(embedArray)
		return l.token(TBracketOpen, false), l.lexStatement, nil
	case r == ']':
		if err := l.endEmbed(embedArray); err != nil {
			return noToken, nil, err
		}
		return l.token(TBracketClose, false), l.lexStatement, nil
	case r == '}':
		if err := l.endEmbed(embedMap); err != nil {
			return noToken, nil, err
		}
		return l.token(TCurlClose, false), l.lexStatement, nil
	case r == '"':
		l.buffer(r, -1)
		return noToken, l.lexString, nil
	case isBarewordInitial(r):
		l.buffer(r, r)
		return noToken, l.lexWordTail(true, l.lexStatement), nil
	case unicode.IsSpace(r):
		return noToken, l.lexSpace(l.lexStatement), nil
	case r == eof:
		return noToken, l.lexStatement, ErrUnexpectedEOF
	}
	return noToken, nil, fmt.Errorf("unexpected character %q in parameter context: expected literal, {, or ;", r)
}

func (l *Lexer) lexString(r rune) (Token, consumerFunc, error) {
	l.buffer(r, -1)
	switch r {
	case eof:
		return noToken, l.lexString, ErrUnexpectedEOF
	case '\\':
		return noToken, l.lexStringEscape, nil
	case '"':
		return l.token(TString, true), l.lexStatement, nil
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
		l.reset()
		l.beginEmbed(embedMap)
		return l.token(TMapOpen, false), l.lexStatement, nil
	case '/':
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
		return tok, l.lexStatement, err
	}
	l.buffer(r, r)
	return noToken, l.lexRegexp, nil

}

func (l *Lexer) lexSectionOpen(r rune) (Token, consumerFunc, error) {
	l.beginEmbed(embedSection)
	if r != '{' {
		return noToken, l.lexSegment, fmt.Errorf("unexpected %q: expected %q", r, '{')
	}
	return l.token(TCurlOpen, false), l.lexSegment, nil
}

func (l *Lexer) beginEmbed(embed embedType) {
	l.embed = append(l.embed, embed)
}

func (l *Lexer) endEmbed(embed embedType) error {
	n := len(l.embed) - 1
	if n < 0 || l.embed[n] != embed {
		return fmt.Errorf("unexpected %v closing", embed)
	}
	l.embed = l.embed[:n]
	return nil
}

// func (l *Lexer) lexExpr
