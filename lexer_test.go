package codf

import (
	"bytes"
	"fmt"
	"math/big"
	"regexp"
	"strconv"
	"testing"
	"time"
)

func compareValue(l, r interface{}) bool {
	switch ll := l.(type) {
	case nil:
		// Don't compare -- nil isn't a value and only used for punctuation.
		return true
	case bool:
		rr, ok := r.(bool)
		return ok && ll == rr
	case string:
		rr, ok := r.(string)
		return ok && ll == rr
	case time.Duration:
		rr, ok := r.(time.Duration)
		return ok && ll == rr
	case *big.Int:
		rr, ok := r.(*big.Int)
		return ok && ll.Cmp(rr) == 0
	case *big.Float:
		rr, ok := r.(*big.Float)
		return ok && ll.Cmp(rr) == 0
	case *big.Rat:
		rr, ok := r.(*big.Rat)
		return ok && ll.Cmp(rr) == 0
	case *regexp.Regexp:
		rr, ok := r.(*regexp.Regexp)
		return ok && ll.String() == rr.String()
	default:
		// Not a known type -- can't be valid
		return false
	}
}

func reader(s string) *bytes.Buffer {
	return bytes.NewBuffer([]byte(s))
}

func requireEOF(t *testing.T, b *bytes.Buffer) {
	if b.Len() > 0 {
		t.Fatalf("expected EOF; %d bytes remaining", b.Len())
	}
}

func checkToken(t *testing.T, prefix string, got, want Token) {
	if got.Kind != want.Kind {
		t.Errorf("%stok.Kind = %v; want %v", prefix, got.Kind, want.Kind)
	}
	if want.Start.Column > 0 && got.Start != want.Start {
		t.Errorf("%stok.Start = %#v; want %#v", prefix, got.Start, want.Start)
	}
	if want.End.Column > 0 && got.End != want.End {
		t.Errorf("%stok.End = %#v; want %#v", prefix, got.End, want.End)
	}
	if want.Raw == nil {
		// Skip check
	} else if want.Raw != nil && got.Raw == nil {
		t.Errorf("%stok.Raw = nil; want %q", prefix, want.Raw)
	} else if !bytes.Equal(got.Raw, want.Raw) {
		t.Errorf("%stok.Raw = %q; want %q", prefix, got.Raw, want.Raw)
	}

	if !compareValue(want.Value, got.Value) {
		t.Errorf("%stok.Value = %T(%#v); want %T(%#v)", prefix,
			got.Value, got.Value,
			want.Value, want.Value)
	}

	if t.Failed() {
		t.Logf("%stok.Raw = %q", prefix, got.Raw)
	}
}

type tokenCase struct {
	Token
	Err bool
}

// Common punctuation tokens
var (
	_error        = tokenCase{Err: true}
	_ws           = tokenCase{Token: Token{Kind: TWhitespace}}
	_eof          = tokenCase{Token: Token{Kind: TEOF}}
	_semicolon    = tokenCase{Token: Token{Kind: TSemicolon}}
	_curlopen     = tokenCase{Token: Token{Kind: TCurlOpen}}
	_curlclose    = tokenCase{Token: Token{Kind: TCurlClose}}
	_bracketopen  = tokenCase{Token: Token{Kind: TBracketOpen}}
	_bracketclose = tokenCase{Token: Token{Kind: TBracketClose}}
	_mapopen      = tokenCase{Token: Token{Kind: TMapOpen}}
	_comment      = tokenCase{Token: Token{Kind: TComment}}
)

type tokenSeq []tokenCase

func (seq tokenSeq) Run(t *testing.T, input string) {
	buf := reader(input)
	lex := NewLexer(buf)

	for i, want := range seq {
		prefix := fmt.Sprintf("%d: ", i+1)

		tok, err := lex.ReadToken()
		if want.Err && err == nil {
			t.Errorf("%sgot error = nil; want error", prefix)
		} else if !want.Err && err != nil {
			t.Errorf("%sgot error = %v; want %v", prefix, err, want.Kind)
		}

		if want.Err && err != nil {
			return
		}

		checkToken(t, prefix, tok, want.Token)

		if t.Failed() {
			return
		}
	}

	requireEOF(t, buf)
}

type tokenSeqTest struct {
	Name  string
	Input string
	Seq   tokenSeq
}

func (tt *tokenSeqTest) Run(t *testing.T) {
	t.Run(tt.Name, func(t *testing.T) {
		tt.Seq.Run(t, tt.Input)
	})
}

func TestComment(t *testing.T) {
	tokenSeq{
		{Token: Token{Kind: TComment, Raw: []byte("")}},
		{Token: Token{Kind: TComment, Raw: []byte(" foo bar baz")}},
		_eof,
	}.Run(t, "'\n' foo bar baz")
}

func TestBareword(t *testing.T) {
	tokenSeq{
		{Token: Token{
			Kind:  TWhitespace,
			Start: Location{Offset: 0, Line: 1, Column: 1},
			End:   Location{Offset: 1, Line: 1, Column: 2},
		}},
		{Token: Token{
			Kind:  TWord,
			Raw:   []byte(".foo$bar#baz=quux"),
			Start: Location{Offset: 1, Line: 1, Column: 2},
			End:   Location{Offset: 18, Line: 1, Column: 19},
			Value: ".foo$bar#baz=quux",
		}},
		_semicolon,
		{Token: Token{Kind: TWhitespace}},
		{Token: Token{Kind: TComment, Raw: []byte(" foo")}},
		{Token: Token{Kind: TWhitespace}},
		_eof,
	}.Run(t, "\t.foo$bar#baz=quux; ' foo\n\n")
}

func TestWhitespace(t *testing.T) {
	tokenSeq{
		{
			Token: Token{
				Start: Location{1, 1, 0},
				End:   Location{Column: 3, Line: 3, Offset: 6},
				Kind:  TWhitespace,
			},
		},
	}.Run(t, " \n\r\n\t ")
}

func TestBooleans(t *testing.T) {
	tokenSeq{
		{Token: Token{Kind: TWord, Raw: []byte("TRUE"), Value: "TRUE"}},
		_ws, {Token: Token{Kind: TBoolean, Raw: []byte("true"), Value: true}},
		_ws, {Token: Token{Kind: TBoolean, Raw: []byte("Yes"), Value: true}},
		_ws, {Token: Token{Kind: TBoolean, Raw: []byte("FALSE"), Value: false}},
		_curlopen,
		_curlclose,
		_eof,
	}.Run(t, "TRUE true Yes FALSE{}")
}

func TestStatement(t *testing.T) {
	tokenSeq{
		_ws, {Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}},
		_ws, {Token: Token{Kind: TInteger, Raw: []byte("-1234"), Value: big.NewInt(-1234)}},
		_ws, {Token: Token{Kind: TOctal, Raw: []byte("+0600"), Value: big.NewInt(0600)}},
		_ws, {Token: Token{Kind: THex, Raw: []byte("-0xf"), Value: big.NewInt(-15)}},
		_ws, {Token: Token{Kind: THex, Raw: []byte("0x12f"), Value: big.NewInt(303)}},
		_semicolon,
		_ws, {Token: Token{Kind: TWord, Raw: []byte("stmt/2"), Value: "stmt/2"}},
		_semicolon,
		_ws, {Token: Token{Kind: TWord, Raw: []byte("sect"), Value: "sect"}},
		_curlopen, _curlclose,
		_ws, {Token: Token{Kind: TWord, Raw: []byte("a"), Value: "a"}},
		_semicolon,
		_ws, {Token: Token{Kind: TWord, Raw: []byte("b"), Value: "b"}},
		_curlopen, _curlclose,
		_ws, {Token: Token{Kind: TWord, Raw: []byte("c"), Value: "c"}},
		_comment,
		_ws, _semicolon, _semicolon,
		_ws, _eof,
	}.Run(t, `
		stmt -1234 +0600 -0xf 0x12f;
		stmt/2;
		sect{}
		a;
		b{}
		c'foo
		;;
		`)
}

func TestSectionDoubleClose(t *testing.T) {
	tokenSeq{
		{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}},
		_ws, {Token: Token{Kind: TWord, Raw: []byte("foo"), Value: "foo"}},
		_ws, _bracketopen, _bracketopen,
		{Token: Token{Kind: TInteger, Raw: []byte("1"), Value: big.NewInt(1)}},
		_ws, {Token: Token{Kind: TInteger, Raw: []byte("2"), Value: big.NewInt(2)}},
		_bracketclose,
		_ws, {Token: Token{Kind: TInteger, Raw: []byte("3"), Value: big.NewInt(3)}},
		_bracketclose,
		_ws, _mapopen, _curlclose,
		_ws, _curlopen,
		_ws, _semicolon,
		_ws, _curlclose,
		_ws, _error,
	}.Run(t, "stmt foo [[1 2] 3] #{} { ; } }")
}

func TestRegexp(t *testing.T) {
	regex := regexp.MustCompile
	tokenSeq{
		{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}},
		_ws, {Token: Token{Kind: TRegexp, Raw: []byte("#/foo\\/bar\n/"), Value: regex("foo/bar\n")}},
		_ws, {Token: Token{Kind: TRegexp, Raw: []byte("#//"), Value: regex("")}},
		_ws, {Token: Token{Kind: TRegexp, Raw: []byte("#/\\./"), Value: regex("\\.")}},
		_semicolon,
		_eof,
	}.Run(t, "stmt #/foo\\/bar\n/ #// #/\\./;")

	// Fail on EOF at points in regexp parsing
	// EOF after pound
	tokenSeq{
		{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}},
		_ws, _error,
	}.Run(t, "stmt #")

	// EOF in regexp (start)
	tokenSeq{
		{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}},
		_ws, _error,
	}.Run(t, "stmt #/")

	// EOF in regexp (middle)
	tokenSeq{
		{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}},
		_ws, _error,
	}.Run(t, "stmt #/foobar")

	// EOF in statement
	tokenSeq{
		{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}},
		_ws, {Token: Token{Kind: TRegexp, Raw: []byte("#/foobar/"), Value: regex("foobar")}},
		_error,
	}.Run(t, "stmt #/foobar/")
}

func TestString(t *testing.T) {
	tokenSeq{
		{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}},
		_ws, {Token: Token{Kind: TString, Raw: []byte(`""`), Value: ""}},
		_ws, {Token: Token{Kind: TString, Raw: []byte(`"simple string"`), Value: "simple string"}},
		_ws, {Token: Token{Kind: TString, Raw: []byte(`"\a\b\f\n\r\t\v\\\""`), Value: "\a\b\f\n\r\t\v\\\""}},
		_ws, {Token: Token{Kind: TString, Raw: []byte(`"\123\xff\u7fff\U00001234"`), Value: "\123\xff\u7fff\U00001234"}},
		_ws, _semicolon,
		_eof,
	}.Run(t,
		`stmt   ""
			"simple string"
			"\a\b\f\n\r\t\v\\\""
			"\123\xff\u7fff\U00001234"
		;`)
}

func TestBaseInteger(t *testing.T) {
	num := big.NewInt(-12345)
	pos := big.NewInt(12345)
	for base := 2; base <= 36; base++ {
		basenum := pos.Text(base)
		lit := fmt.Sprintf("-%d#%s", base, basenum)
		stmt := "stmt " + lit + " foo 0;"
		tok := Token{
			Kind:  TBaseInt,
			Raw:   []byte(lit),
			Value: num,
		}

		(&tokenSeqTest{
			Name:  "Base-" + strconv.Itoa(base),
			Input: stmt,
			Seq: tokenSeq{
				{Token: Token{Kind: TWord, Value: "stmt"}},
				_ws, {Token: tok},
				_ws, {Token: Token{Kind: TWord, Value: "foo"}},
				_ws, {Token: Token{Kind: TInteger, Value: big.NewInt(0)}},
				_semicolon,
				_eof,
			},
		}).Run(t)
	}

	// Invalid things
	badtext := []string{
		`0#0`,
		`1#0`,
		`37#zz`,
	}

	bad := tokenSeq{
		{Token: Token{Kind: TWord, Value: "stmt"}},
		_ws, _error,
	}

	for _, c := range badtext {
		t.Run(c, func(t *testing.T) {
			bad.Run(t, `stmt `+c+`;`)
		})
	}
}

func TestInvalidStrings(t *testing.T) {
	invalid := tokenSeq{
		{Token: Token{
			Kind:  TWord,
			Raw:   []byte("stmt"),
			Value: "stmt",
		}},
		_ws, _error,
	}

	cases := []tokenSeqTest{
		{Name: "EOF", Input: `stmt "`},
		{Name: "BadContext", Input: ` ""`, Seq: tokenSeq{_ws, _error}},
		{Name: "Octal-Invalid", Input: `stmt "\60z";`},
		{Name: "Octal-Invalid", Input: `stmt "\608";`},
		{Name: "Octal-EOF", Input: `stmt "\`},
		{Name: "Octal-EOF", Input: `stmt "\7`},
		{Name: "Octal-EOF", Input: `stmt "\60`},
		{Name: "Hex-Invalid", Input: `stmt "\xz";`},
		{Name: "Hex-Invalid", Input: `stmt "\xfz";`},
		{Name: "Hex-EOF", Input: `stmt "\x`},
		{Name: "Hex-EOF", Input: `stmt "\xf`},
		{Name: "Uni16-Invalid", Input: `stmt "\uff";`},
		{Name: "Uni16-Invalid", Input: `stmt "\uffxx";`},
		{Name: "Uni16-EOF", Input: `stmt "\uf`},
		{Name: "Uni32-Invalid", Input: `stmt "\U12345";`},
		{Name: "Uni32-Invalid", Input: `stmt "\U12345xx";`},
		{Name: "Uni32-EOF", Input: `stmt "\U123456`},
		{Name: "BadEscape", Input: `stmt "\z";`},
	}

	for i, c := range cases {
		c.Name = fmt.Sprint(c.Name, "-", i+1)
		if c.Seq == nil {
			c.Seq = invalid
		}
		c.Run(t)
	}
}

func TestIntegers(t *testing.T) {
	neg := big.NewInt(-1234)
	pos := big.NewInt(1234)
	tokenSeq{
		{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}},
		// Negative sign
		_ws, {Token: Token{Kind: TBinary, Value: neg, Raw: []byte("-0b10011010010")}},
		_ws, {Token: Token{Kind: TBinary, Value: neg, Raw: []byte("-0B10011010010")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: neg, Raw: []byte("-2#10011010010")}},
		_ws, {Token: Token{Kind: TOctal, Value: neg, Raw: []byte("-02322")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: neg, Raw: []byte("-8#2322")}},
		_ws, {Token: Token{Kind: TInteger, Value: neg, Raw: []byte("-1234")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: neg, Raw: []byte("-10#1234")}},
		_ws, {Token: Token{Kind: THex, Value: neg, Raw: []byte("-0x4d2")}},
		_ws, {Token: Token{Kind: THex, Value: neg, Raw: []byte("-0X4D2")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: neg, Raw: []byte("-16#4D2")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: neg, Raw: []byte("-36#ya")}},
		// Positive sign
		_ws, {Token: Token{Kind: TBinary, Value: pos, Raw: []byte("+0b10011010010")}},
		_ws, {Token: Token{Kind: TBinary, Value: pos, Raw: []byte("+0B10011010010")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: pos, Raw: []byte("+2#10011010010")}},
		_ws, {Token: Token{Kind: TOctal, Value: pos, Raw: []byte("+02322")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: pos, Raw: []byte("+8#2322")}},
		_ws, {Token: Token{Kind: TInteger, Value: pos, Raw: []byte("+1234")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: pos, Raw: []byte("+10#1234")}},
		_ws, {Token: Token{Kind: THex, Value: pos, Raw: []byte("+0x4d2")}},
		_ws, {Token: Token{Kind: THex, Value: pos, Raw: []byte("+0X4D2")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: pos, Raw: []byte("+16#4D2")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: pos, Raw: []byte("+36#ya")}},
		// No sign
		_ws, {Token: Token{Kind: TBinary, Value: pos, Raw: []byte("0b10011010010")}},
		_ws, {Token: Token{Kind: TBinary, Value: pos, Raw: []byte("0B10011010010")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: pos, Raw: []byte("2#10011010010")}},
		_ws, {Token: Token{Kind: TOctal, Value: pos, Raw: []byte("02322")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: pos, Raw: []byte("8#2322")}},
		_ws, {Token: Token{Kind: TInteger, Value: pos, Raw: []byte("1234")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: pos, Raw: []byte("10#1234")}},
		_ws, {Token: Token{Kind: THex, Value: pos, Raw: []byte("0x4d2")}},
		_ws, {Token: Token{Kind: THex, Value: pos, Raw: []byte("0X4D2")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: pos, Raw: []byte("16#4D2")}},
		_ws, {Token: Token{Kind: TBaseInt, Value: pos, Raw: []byte("36#ya")}},
		_ws, _semicolon,
		_eof,
	}.Run(t, `stmt
			-0b10011010010
			-0B10011010010
			-2#10011010010
			-02322
			-8#2322
			-1234
			-10#1234
			-0x4d2
			-0X4D2
			-16#4D2
			-36#ya
			+0b10011010010
			+0B10011010010
			+2#10011010010
			+02322
			+8#2322
			+1234
			+10#1234
			+0x4d2
			+0X4D2
			+16#4D2
			+36#ya
			 0b10011010010
			 0B10011010010
			 2#10011010010
			 02322
			 8#2322
			 1234
			 10#1234
			 0x4d2
			 0X4D2
			 16#4D2
			 36#ya
		;`)
}

func TestRationals(t *testing.T) {
	zero := big.NewRat(0, 1)
	neg := big.NewRat(-3, 4)
	pos := big.NewRat(3, 4)
	tokenSeq{
		{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}},
		// Zero
		_ws, {Token: Token{Kind: TRational, Value: zero, Raw: []byte("0/100")}},
		_ws, {Token: Token{Kind: TRational, Value: zero, Raw: []byte("-0/200")}},
		_ws, {Token: Token{Kind: TRational, Value: zero, Raw: []byte("-0/1")}},
		_ws, {Token: Token{Kind: TRational, Value: zero, Raw: []byte("0/1")}},
		// Negative sign
		_ws, {Token: Token{Kind: TRational, Value: neg, Raw: []byte("-3/4")}},
		_ws, {Token: Token{Kind: TRational, Value: neg, Raw: []byte("-6/8")}},
		_ws, {Token: Token{Kind: TRational, Value: neg, Raw: []byte("-75/100")}},
		// Positive sign
		_ws, {Token: Token{Kind: TRational, Value: pos, Raw: []byte("+3/4")}},
		_ws, {Token: Token{Kind: TRational, Value: pos, Raw: []byte("+6/8")}},
		_ws, {Token: Token{Kind: TRational, Value: pos, Raw: []byte("+75/100")}},
		// No sign
		_ws, {Token: Token{Kind: TRational, Value: pos, Raw: []byte("3/4")}},
		_ws, {Token: Token{Kind: TRational, Value: pos, Raw: []byte("6/8")}},
		_ws, {Token: Token{Kind: TRational, Value: pos, Raw: []byte("75/100")}},
		_ws, _semicolon,
		_eof,
	}.Run(t, `stmt
			 0/100
			-0/200
			-0/1
			 0/1
			-3/4
			-6/8
			-75/100
			+3/4
			+6/8
			+75/100
			 3/4
			 6/8
			 75/100
		;`)

	// Zero denominator -> error
	tokenSeq{
		{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}},
		_ws, _error,
	}.Run(t, `stmt 5/0;`)

	// Fail on EOF in rational (needs a sentinel)
	tokenSeq{
		{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}},
		_ws, _error,
	}.Run(t, `stmt 5/`)

	tokenSeq{
		{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}},
		_ws, _error,
	}.Run(t, `stmt 5/1`)
}

func TestLocationString(t *testing.T) {
	const want = "2:34@45"
	loc := Location{
		Line:   2,
		Column: 34,
		Offset: 45,
	}
	if got := loc.String(); got != want {
		t.Fatalf("%#+v.String() = %q; want %q", loc, got, want)
	}
}

func TestDecimals(t *testing.T) {
	dec := func(text string) tokenCase {
		var f big.Float
		f.SetPrec(DefaultPrecision)
		if _, ok := f.SetString(text); !ok {
			panic("error creating float " + text)
		}
		return tokenCase{
			Token: Token{
				Kind:  TDecimal,
				Raw:   []byte(text),
				Value: &f,
			},
		}
	}

	_stmt := tokenCase{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}}
	tokenSeq{
		_stmt,
		// Negative sign
		_ws, dec("-0.0"),
		_ws, dec("-0.5"),
		_ws, dec("-0.0e0"),
		_ws, dec("-0.0E0"),
		_ws, dec("-1.2345"),
		_ws, dec("-12345e-4"),
		_ws, dec("-1.2345e4"),
		_ws, dec("-1.2345e+4"),
		// Positive sign
		_ws, dec("+0.0"),
		_ws, dec("+0.5"),
		_ws, dec("+0.0e0"),
		_ws, dec("+0.0E0"),
		_ws, dec("+1.2345"),
		_ws, dec("+12345E-4"),
		_ws, dec("+1.2345E4"),
		_ws, dec("+1.2345E+4"),
		// No sign
		_ws, _bracketopen, dec("0.0"), _bracketclose,
		_ws, _mapopen,
		{Token: Token{Kind: TWord, Value: "k"}},
		_ws, dec("0.5"),
		_curlclose,
		_ws, dec("0.0e0"),
		_ws, dec("0.0E0"),
		_ws, dec("1.2345"),
		_ws, dec("12345e-4"),
		_ws, dec("1.2345e4"),
		_ws, dec("1.2345e+4"),
		_ws, _semicolon,
		_eof,
	}.Run(t, `stmt
			-0.0 -0.5 -0.0e0 -0.0E0
			-1.2345 -12345e-4 -1.2345e4 -1.2345e+4
			+0.0 +0.5 +0.0e0 +0.0E0
			+1.2345 +12345E-4 +1.2345E4 +1.2345E+4
			[0.0] #{k 0.5} 0.0e0 0.0E0
			1.2345 12345e-4 1.2345e4 1.2345e+4
		;`)

	// Check invalid cases...

	// ... with no leading word
	tokenSeq{_error}.Run(t, `5.5;`)
	tokenSeq{_error}.Run(t, `5e5;`)

	// ... after a leading word
	bad := tokenSeq{_stmt, _ws, _error}
	badValues := []string{
		`5z`,    // invalid char
		`5ez`,   // invalid char
		`5.`,    // eof
		`5.z`,   // invalid char
		`5e+z`,  // invalid char
		`5e-z`,  // invalid char
		`5e+5z`, // invalid char
		`5e-5z`, // invalid char
		`5e+0z`, // invalid char
		`5e-0z`, // invalid char
		`0.5e`,  // eof
		`0.5e+`, // eof
		`0.5e-`, // eof
		`0.5e0`, // eof
		`0.5e9`, // eof
		`0`,     // eof
		`5e9`,   // eof
		`0e9`,   // eof
		`5e+9`,  // eof
		`0e+9`,  // eof
	}
	for _, c := range badValues {
		bad.Run(t, "stmt "+c)
	}
}

func TestDurations(t *testing.T) {
	dur := func(text string) tokenCase {
		d, err := time.ParseDuration(text)
		if err != nil {
			panic("error creating duration: " + err.Error())
		}
		return tokenCase{
			Token: Token{
				Kind:  TDuration,
				Raw:   []byte(text),
				Value: d,
			},
		}
	}

	_stmt := tokenCase{Token: Token{Kind: TWord, Raw: []byte("stmt"), Value: "stmt"}}

	t.Run("Valid", func(t *testing.T) {
		tokenSeq{
			_stmt,
			// Negative sign
			_ws, dur("-0s"),
			_ws, dur("-1ns"),
			_ws, dur("-0ns"),
			_ws, dur("-0.0s"),
			_ws, dur("-1h234m7s"),
			_ws, dur("-1h"),
			_ws, dur("-60m"),
			_ws, dur("-0.5s"),
			_ws, dur("-500ms"),
			_ws, dur("-0.5ms"),
			_ws, dur("-500us"),
			_ws, dur("-500μs"),
			// Positive sign
			_ws, dur("+0s"),
			_ws, dur("+1ns"),
			_ws, dur("+0ns"),
			_ws, dur("+0.0s"),
			_ws, dur("+1h234m7s"),
			_ws, dur("+1h"),
			_ws, dur("+60m"),
			_ws, dur("+0.5s"),
			_ws, dur("+500ms"),
			_ws, dur("+0.5ms"),
			_ws, dur("+500us"),
			_ws, dur("+500μs"),
			// No sign
			_ws, dur("0s"),
			_ws, dur("1ns"),
			_ws, dur("0ns"),
			_ws, dur("0.0s"),
			_ws, dur("1h234m7s"),
			_ws, dur("1h"),
			_ws, dur("60m"),
			_ws, dur("0.5s"),
			_ws, dur("500ms"),
			_ws, dur("0.5ms"),
			_ws, dur("500us"),
			_ws, dur("500μs"),
			_ws, dur("1h0.25m"),
			_ws, dur("1h0m0.0s"),
			_ws, _semicolon,
			_eof,
		}.Run(t, `stmt
			-0s -1ns -0ns -0.0s
			-1h234m7s -1h -60m
			-0.5s -500ms
			-0.5ms -500us -500μs
			+0s +1ns +0ns +0.0s
			+1h234m7s +1h +60m
			+0.5s +500ms
			+0.5ms +500us +500μs
			0s 1ns 0ns 0.0s
			1h234m7s 1h 60m
			0.5s 500ms
			0.5ms 500us 500μs
			1h0.25m
			1h0m0.0s
		;`)
	})

	// Check invalid cases...

	// ... with no leading word
	tokenSeq{_error}.Run(t, `5ms;`)
	tokenSeq{_error}.Run(t, `5h;`)

	// ... after a leading word
	bad := tokenSeq{_stmt, _ws, _error}
	badValues := []string{
		`1m`,
		`1mz`,
		`1h0`,
		`1h00`,
		`1h0z`,
		`1h0.0`,
		`1h0.1`,
		`1h0;`,
		`1h0.`,
		`1h0.5`,
		`1h0.5n`,
		`1h0.;`,
		`1h0.5;`,
		`1h0.5n;`,
		`1h0u`,
		`1h0u;`,
		`1h0.5`,
		`1h0.0z`,
	}
	for _, c := range badValues {
		t.Run(c, func(t *testing.T) {
			bad.Run(t, "stmt "+c)
		})
	}
}
