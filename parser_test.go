package codf

import (
	"fmt"
	"reflect"
	"strings"
	"testing"
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

func TestEmptyArray(t *testing.T) {
	objectsEqual(t, "",
		mustParse(t, "stmt YES [];"),
		&Document{
			Children: []Node{
				&Statement{
					NameTok: &Literal{
						Tok: Token{Kind: TWord, Value: "stmt"},
					},
					Params: []ExprNode{
						&Literal{
							Tok: Token{Kind: TBoolean, Value: true},
						},
						&Array{},
					},
				},
			},
		},
	)
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
		if !reflect.DeepEqual(want.Value(), got.Value()) {
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
