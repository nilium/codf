package codf

import (
	"bytes"
	"fmt"
	"math/big"
	"regexp"
	"strconv"
	"testing"
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
			want.Value, want.Value,
			got.Value, got.Value)
	}
}

type tokenCase struct {
	Token
	Err bool
}

var (
	ws        = tokenCase{Token: Token{Kind: TWhitespace}}
	semicolon = tokenCase{Token: Token{Kind: TSemicolon}}
)

type tokenSeq []tokenCase

func (seq tokenSeq) Run(t *testing.T, input string) {
	buf := reader(input)
	lex := NewLexer(buf)

	for i, want := range seq {
		prefix := fmt.Sprintf("%d: ", i+1)

		tok, err := lex.ReadToken()
		if want.Err && err == nil {
			t.Fatalf("%sgot error = nil; want error", prefix)
		} else if !want.Err && err != nil {
			t.Fatalf("%sgot error = %v; want %v", prefix, err, want.Kind)
		}

		if want.Err {
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
		{Token: Token{
			Kind: TComment,
			Raw:  []byte(""),
		}},
		{Token: Token{
			Kind: TComment,
			Raw:  []byte(" foo bar baz"),
		}},
		{Token: Token{Kind: TEOF}},
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
		{Token: Token{Kind: TSemicolon}},
		{Token: Token{Kind: TWhitespace}},
		{Token: Token{
			Kind: TComment,
			Raw:  []byte(" foo"),
		}},
		{Token: Token{Kind: TWhitespace}},
		{Token: Token{Kind: TEOF}},
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
		{
			Token: Token{
				Kind:  TWord,
				Raw:   []byte("TRUE"),
				Value: "TRUE",
			},
		},
		ws,
		{
			Token: Token{
				Kind:  TBoolean,
				Raw:   []byte("true"),
				Value: true,
			},
		},
		ws,
		{
			Token: Token{
				Kind:  TBoolean,
				Raw:   []byte("Yes"),
				Value: true,
			},
		},
		ws,
		{
			Token: Token{
				Kind:  TBoolean,
				Raw:   []byte("FALSE"),
				Value: false,
			},
		},
		{Token: Token{Kind: TCurlOpen}},
		{Token: Token{Kind: TCurlClose}},
		{Token: Token{Kind: TEOF}},
	}.Run(t, "TRUE true Yes FALSE{}")
}

func TestStatement(t *testing.T) {
	tokenSeq{
		{
			Token: Token{
				Kind:  TWord,
				Raw:   []byte("stmt"),
				Value: "stmt",
			},
		},
		ws,
		{
			Token: Token{
				Kind:  TInteger,
				Raw:   []byte("-1234"),
				Value: big.NewInt(-1234),
			},
		},
		ws,
		{
			Token: Token{
				Kind:  TOctal,
				Raw:   []byte("+0600"),
				Value: big.NewInt(0600),
			},
		},
		ws,
		{
			Token: Token{
				Kind:  THex,
				Raw:   []byte("-0xf"),
				Value: big.NewInt(-15),
			},
		},
		ws,
		{
			Token: Token{
				Kind:  THex,
				Raw:   []byte("0x12f"),
				Value: big.NewInt(303),
			},
		},
		semicolon,
		ws,
	}.Run(t, "stmt -1234 +0600 -0xf 0x12f;\n")
}

func TestSectionDoubleClose(t *testing.T) {
	tokenSeq{
		{
			Token: Token{
				Kind:  TWord,
				Raw:   []byte("stmt"),
				Value: "stmt",
			},
		},
		ws,
		{
			Token: Token{
				Kind:  TWord,
				Raw:   []byte("foo"),
				Value: "foo",
			},
		},
		ws,
		{Token: Token{Kind: TBracketOpen}},
		{
			Token: Token{
				Kind:  TInteger,
				Raw:   []byte("1"),
				Value: big.NewInt(1),
			},
		},
		ws,
		{
			Token: Token{
				Kind:  TInteger,
				Raw:   []byte("2"),
				Value: big.NewInt(2),
			},
		},
		ws,
		{
			Token: Token{
				Kind:  TInteger,
				Raw:   []byte("3"),
				Value: big.NewInt(3),
			},
		},
		{Token: Token{Kind: TBracketClose}},
		ws,
		{Token: Token{Kind: TMapOpen}},
		{Token: Token{Kind: TCurlClose}},
		ws,
		{Token: Token{Kind: TCurlOpen}},
		ws,
		semicolon,
		ws,
		{Token: Token{Kind: TCurlClose}},
		ws,
		{Err: true},
	}.Run(t, "stmt foo [1 2 3] #{} { ; } }")
}

func TestRegexp(t *testing.T) {
	tokenSeq{
		{Token: Token{
			Kind:  TWord,
			Raw:   []byte("stmt"),
			Value: "stmt",
		}},
		ws,
		{Token: Token{
			Kind:  TRegexp,
			Raw:   []byte("#/foo\\/bar\n/"),
			Value: regexp.MustCompile("foo/bar\n"),
		}},
		ws,
		{Token: Token{
			Kind:  TRegexp,
			Raw:   []byte("#//"),
			Value: regexp.MustCompile(""),
		}},
		ws,
		{Token: Token{
			Kind:  TRegexp,
			Raw:   []byte("#/\\./"),
			Value: regexp.MustCompile("\\."),
		}},
		semicolon,
		{Token: Token{Kind: TEOF}},
	}.Run(t, "stmt #/foo\\/bar\n/ #// #/\\./;")
}

func TestString(t *testing.T) {
	tokenSeq{
		{Token: Token{
			Kind:  TWord,
			Raw:   []byte("stmt"),
			Value: "stmt",
		}},
		ws,
		{Token: Token{
			Kind:  TString,
			Raw:   []byte(`""`),
			Value: "",
		}},
		ws,
		{Token: Token{
			Kind:  TString,
			Raw:   []byte(`"simple string"`),
			Value: "simple string",
		}},
		ws,
		{Token: Token{
			Kind:  TString,
			Raw:   []byte(`"\a\b\f\n\r\t\v\\\""`),
			Value: "\a\b\f\n\r\t\v\\\"",
		}},
		ws,
		{Token: Token{
			Kind:  TString,
			Raw:   []byte(`"\123\xff\u7fff\U00001234"`),
			Value: "\123\xff\u7fff\U00001234",
		}},
		ws,
		semicolon,
		{Token: Token{Kind: TEOF}},
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
				ws,
				{Token: tok},
				ws,
				{Token: Token{Kind: TWord, Value: "foo"}},
				ws,
				{Token: Token{Kind: TInteger, Value: big.NewInt(0)}},
				semicolon,
				{Token: Token{Kind: TEOF}},
			},
		}).Run(t)
	}
}

func TestInvalidStrings(t *testing.T) {
	invalid := tokenSeq{
		{Token: Token{
			Kind:  TWord,
			Raw:   []byte("stmt"),
			Value: "stmt",
		}},
		ws,
		{Err: true},
	}

	cases := []tokenSeqTest{
		{Name: "EOF", Input: `stmt "`},
		{Name: "BadContext", Input: ` ""`, Seq: tokenSeq{ws, {Err: true}}},
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
