package codf

import (
	"fmt"
	"math/big"
	"reflect"
	"regexp"
	"strings"
	"testing"
	"time"
)

func parse(in string) (*Document, error) {
	r := strings.NewReader(in)
	l := NewLexer(r)
	p := NewParser()
	if err := p.Parse(l); err != nil {
		return nil, err
	}
	return p.Document(), nil
}

func mustParse(t *testing.T, in string) *Document {
	doc, err := parse(in)
	if err != nil {
		t.Fatalf("Parse(..) error = %v; want nil", err)
	}
	return doc
}

func mustNotParse(t *testing.T, in string) *Document {
	doc, err := parse(in)
	if err == nil {
		t.Fatalf("Parse(..) error = %v; want error", err)
	}
	t.Logf("Parse(..) error = %v", err)
	return doc
}

func TestParseEmpty(t *testing.T) {
	t.Run("Empty", func(t *testing.T) {
		objectsEqual(t, "",
			mustParse(t, ""),
			new(Document),
		)
	})
	t.Run("Whitespace", func(t *testing.T) {
		objectsEqual(t, "",
			mustParse(t, " "),
			new(Document),
		)
	})
	t.Run("Whitespaces", func(t *testing.T) {
		objectsEqual(t, "",
			mustParse(t, "\t \t\n\r\n\r\n \n "),
			new(Document),
		)
	})
	t.Run("Semicolon", func(t *testing.T) {
		objectsEqual(t, "",
			mustParse(t, ";"),
			new(Document),
		)
	})
	t.Run("Semicolons", func(t *testing.T) {
		objectsEqual(t, "",
			mustParse(t, ";;;;;;;"),
			new(Document),
		)
	})
	t.Run("Mixed", func(t *testing.T) {
		objectsEqual(t, "",
			mustParse(t, "   ;\n\t; ;\n;"),
			new(Document),
		)
	})
}

type parseTestCase struct {
	Name string
	Src  string
	Doc  *Document
	Fun  func(*testing.T, string) *Document
}

func (p parseTestCase) Run(t *testing.T) {
	t.Run(p.Name, func(t *testing.T) {
		fn := p.Fun
		if fn == nil {
			fn = mustParse
		}
		doc := fn(t, p.Src)
		objectsEqual(t, "", doc, p.Doc)
	})
}

func TestParseAST(t *testing.T) {
	cases := []parseTestCase{
		{
			Name: "Empty",
			Src:  "",
			Doc:  doc(),
		},
		{
			Name: "EmptyArrayMap",
			Src:  `foo 1 { bar [] #{}; } map #{} [];' eof`,
			Doc: doc().section("foo", 1).
				statement("bar", []ExprNode{}, mkmap()).
				up().statement("map", mkmap(), []ExprNode{}).
				Doc(),
		},
		{
			Name: "NestedArrayMaps",
			Src: `foo [' Nested map inside array
					#{ k [  1       ' integer
						"2"     ' string
						three   ' bareword (string)
						true    ' bool (bareword->bool)
						` + "`true`" + ` ' raw quote bool
						]
					   ` + "`raw`" + ` bare
					}
				];`,
			Doc: doc().statement("foo", mkexpr([]ExprNode{
				mkmap(
					"k", mkexpr(mkexprs(1, "2", "three", true, "true")),
					"raw", "bare",
				),
			})).Doc(),
		},
		{
			Name: "MinimalSpace",
			Src:  `sect[]#{}{stmt #{k[2]"p"#{}}true[false];}`,
			Doc: doc().section("sect", mkexprs(), mkmap()).
				statement("stmt", mkmap("k", mkexprs(2), "p", mkmap()), true, mkexprs(false)).
				Doc(),
		},
		{
			Name: "AllLiterals",
			Src: `
				stmt
					yes no
					true false
					1234 -1.234
					0600 0b101
					16#ffff 0xFFFF
					"\u1234` + "\n" + `\x00"
					"foo" bar
					0.5h30s0.5s500us0.5us1ns
					#/foobar/ #//
					0/1 120/4
				{
					inside Yes YES yes yeS ' Last is always a bareword here
						No NO no nO
						True TRUE true truE
						False FALSE false falsE;
				}
			`,
			Doc: doc().section("stmt",
				true, false,
				true, false,
				1234, mkdec("-1.234"),
				0600, 5,
				0xffff, 0xffff,
				"\u1234\n\x00",
				"foo", "bar",
				time.Hour/2+time.Minute/2+time.Second/2+time.Millisecond/2+time.Microsecond/2+time.Nanosecond,
				mkregex("foobar"), mkregex(""),
				mkrat(0, 1), mkrat(30, 1),
			).statement("inside",
				true, true, true, "yeS",
				false, false, false, "nO",
				true, true, true, "truE",
				false, false, false, "falsE",
			).Doc(),
		},
		{Fun: mustNotParse, Name: "BadMapClose", Src: `src #{;};`},
		{Fun: mustNotParse, Name: "BadMapClose", Src: `src #{ k };`},
		{Fun: mustNotParse, Name: "BadMapClose", Src: `src #{ 1234 five };`},
		{Fun: mustNotParse, Name: "BadMapClose", Src: `src #{ k ];`},
		{Fun: mustNotParse, Name: "BadMapClose", Src: `src #{];`},
		{Fun: mustNotParse, Name: "BadArrayClose", Src: `src [;];`},
		{Fun: mustNotParse, Name: "BadArrayClose", Src: `src [};`},
		{Fun: mustNotParse, Name: "BadStatementClose", Src: `src };`},
		{Fun: mustNotParse, Name: "BadStatementClose", Src: `src ];`},
		{Fun: mustNotParse, Name: "BadStatementClose", Src: `src`},
		{Fun: mustNotParse, Name: "BadSectionClose", Src: `src {`},
		{Fun: mustNotParse, Name: "BadSectionClose", Src: `src {]`},
		{Fun: mustNotParse, Name: "BadSectionClose", Src: `src { ' comment`},
		{Fun: mustNotParse, Name: "BadSectionClose", Src: `}`},
		{Fun: mustNotParse, Name: "BadSectionClose", Src: `]`},
	}

	for _, c := range cases {
		c.Run(t)
	}
}

func mkdec(str string) *Literal {
	f, ok := new(big.Float).SetPrec(DefaultPrecision).SetString(str)
	if !ok {
		panic(fmt.Errorf("error parsing float string: %q", str))
	}
	return &Literal{
		Tok: Token{
			Kind:  TDecimal,
			Value: f,
		},
	}
}

func mkrat(a, b int64) *Literal {
	return &Literal{
		Tok: Token{
			Kind:  TRational,
			Value: big.NewRat(a, b),
		},
	}
}

func mkregex(str string) *Literal {
	return &Literal{
		Tok: Token{
			Kind:  TRegexp,
			Value: regexp.MustCompile(str),
		},
	}
}

func mkexpr(arg interface{}) ExprNode {
	switch arg := arg.(type) {
	case ExprNode:
		return arg
	case bool:
		return &Literal{Tok: Token{Kind: TBoolean, Value: arg}}
	case time.Duration:
		return &Literal{Tok: Token{Kind: TBoolean, Value: arg}}
	case int:
		return &Literal{Tok: Token{Kind: TInteger, Value: big.NewInt(int64(arg))}}
	case int64:
		return &Literal{Tok: Token{Kind: TInteger, Value: big.NewInt(arg)}}
	case float64:
		return &Literal{Tok: Token{Kind: TInteger, Value: new(big.Float).SetPrec(DefaultPrecision).SetFloat64(arg)}}
	case string:
		return &Literal{Tok: Token{Kind: TString, Value: arg}}
	case []ExprNode:
		return &Array{Elems: arg}
	case []interface{}:
		return &Array{Elems: mkexprs(arg...)}
	}
	panic(fmt.Errorf("unsupported type %T", arg))
}

func mkmap(args ...interface{}) *Map {
	if len(args)%2 == 1 {
		panic("mkmap must receive arguments in pairs of (string, value); got valueless key")
	}
	elems := map[string]*MapEntry{}
	for i := 0; i < len(args); i += 2 {
		k := args[i].(string)
		v := mkexpr(args[i+1])
		mp := &MapEntry{
			Ord: uint(i / 2),
			Key: mkexpr(k),
			Val: v,
		}
		elems[k] = mp
	}
	return &Map{Elems: elems}
}

func mkexprs(args ...interface{}) []ExprNode {
	e := make([]ExprNode, len(args))
	for i, arg := range args {
		e[i] = mkexpr(arg)
	}
	return e
}

func doc() *Document {
	return &Document{
		Children: []Node{},
	}
}

type testNode interface {
	statement(name string, args ...interface{}) testNode
	section(name string, args ...interface{}) testNode
	Doc() *Document
	up() testNode
}

type childSection struct {
	parent testNode
	doc    *Document
	*Section
}

func (s *childSection) Doc() *Document {
	return s.doc
}

func (s *childSection) up() testNode {
	return s.parent
}

func (s *childSection) statement(name string, args ...interface{}) testNode {
	ch := &Statement{
		NameTok: mkexpr(name).(*Literal),
		Params:  mkexprs(args...),
	}
	s.Children = append(s.Children, ch)
	return s
}

func (s *childSection) section(name string, args ...interface{}) testNode {
	ch := &Section{
		NameTok:  mkexpr(name).(*Literal),
		Params:   mkexprs(args...),
		Children: []Node{},
	}
	s.Children = append(s.Children, ch)
	return &childSection{
		parent:  s,
		doc:     s.doc,
		Section: ch,
	}
}

func (d *Document) Doc() *Document {
	return d
}

func (d *Document) up() testNode {
	return d
}

func (d *Document) statement(name string, args ...interface{}) testNode {
	ch := &Statement{
		NameTok: mkexpr(name).(*Literal),
		Params:  mkexprs(args...),
	}
	d.Children = append(d.Children, ch)
	return d
}

func (d *Document) section(name string, args ...interface{}) testNode {
	ch := &Section{
		NameTok:  mkexpr(name).(*Literal),
		Params:   mkexprs(args...),
		Children: []Node{},
	}
	d.Children = append(d.Children, ch)
	return &childSection{
		parent:  d,
		doc:     d,
		Section: ch,
	}
}

func objectsEqual(t *testing.T, prefix string, got, want interface{}) (ok bool) {
	fail := func(format string, args ...interface{}) {
		ok = false
		t.Error(prefix + ": " + fmt.Sprintf(format, args...))
	}

	if got == nil && want == nil {
		return true
	}

	if reflect.TypeOf(got) != reflect.TypeOf(want) {
		fail("got = %T(%v); want %T(%v)", got, got, want, want)
		return false
	}

	prefix += "/" + reflect.TypeOf(want).String()

	ok = true

	switch want := want.(type) {
	case *Document:
		got := got.(*Document)

		if want == nil && got == nil {
			return true
		}

		cg, cw := got.Children, want.Children
		if lg, lw := len(cg), len(cw); lg != lw {
			fail(".Children len() = %d; want %d", lg, lw)
		}
		for i, eg := range cg {
			if i >= len(cw) {
				return
			}
			ew := cw[i]
			kp := fmt.Sprintf("%s[%d]", prefix, i)
			if !objectsEqual(t, kp, eg, ew) {
				return
			}
		}

	case *Statement:
		got := got.(*Statement)
		if got.Name() != want.Name() {
			fail("name = %q; want %q", got.Name(), want.Name())
			return
		}

		pg, pw := got.Params, want.Params
		if lg, lw := len(pg), len(pw); lg != lw {
			fail(".Params len() = %d; want %d", lg, lw)
		}

		for i, eg := range pg {
			if i >= len(pw) {
				return
			}
			ew := pw[i]
			kp := fmt.Sprintf("%s[%d]", prefix, i)
			if !objectsEqual(t, kp, eg, ew) {
				return
			}
		}

	case *Section:
		got := got.(*Section)
		if got.Name() != want.Name() {
			fail("name = %q; want %q", got.Name(), want.Name())
			return
		}

		pg, pw := got.Params, want.Params
		if lg, lw := len(pg), len(pw); lg != lw {
			fail(".Params len() = %d; want %d", lg, lw)
		}
		for i, eg := range pg {
			if i >= len(pw) {
				return
			}
			ew := pw[i]
			kp := fmt.Sprintf("%s[%d]", prefix, i)
			if !objectsEqual(t, kp, eg, ew) {
				return
			}
		}

		cg, cw := got.Children, want.Children
		if lg, lw := len(cg), len(cw); lg != lw {
			fail(".Children len() = %d; want %d", lg, lw)
		}
		for i, eg := range cg {
			if i >= len(cw) {
				return
			}
			ew := cw[i]
			kp := fmt.Sprintf("%s[%d]", prefix, i)
			if !objectsEqual(t, kp, eg, ew) {
				return
			}
		}

	case *Literal:
		got := got.(*Literal)
		if !compareValue(want.Value(), got.Value()) {
			fail("got = %#v; want %#v", want.Value(), got.Value())
		}

	case *Array:
		got := got.(*Array)
		if lg, lw := len(got.Elems), len(want.Elems); lg != lw {
			fail("len() = %d; want %d", lg, lw)
		}
		for i, eg := range got.Elems {
			if i >= len(want.Elems) {
				return
			}
			ew := want.Elems[i]
			if !objectsEqual(t, fmt.Sprintf("%s[%d]", prefix, i), eg, ew) {
				return
			}
		}

	case *Map:
		got := got.(*Map)
		pg, pw := got.Pairs(), want.Pairs()
		if lg, lw := len(got.Elems), len(want.Elems); lg != lw {
			fail("len() = %d; want %d", lg, lw)
		}
		for i, eg := range pg {
			if i >= len(pw) {
				return
			}
			ew := pw[i]
			kp := fmt.Sprintf("%s[%q]", prefix, ew.Key.Value())
			if !objectsEqual(t, kp+".Key", eg.Key, ew.Key) {
				fail("key = %#v; want %#v", eg.Key, ew.Key)
			}
			if !objectsEqual(t, kp+".Val", eg.Val, ew.Val) {
				fail("val = %#v; want %#v", eg.Val, ew.Val)
				return
			}
		}

	default:
		fail("unsupported comparable type %v", reflect.TypeOf(want))
	}

	return
}
