package codf

import (
	"fmt"
	"math/big"
	"reflect"
	"regexp"
	"testing"
	"time"
)

func mkdec(str string) *Literal {
	f, ok := new(big.Float).SetPrec(DefaultPrecision).SetString(str)
	if !ok {
		panic(fmt.Errorf("error parsing float string: %q", str))
	}
	return &Literal{
		Tok: Token{
			Kind:  TFloat,
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
