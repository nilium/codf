package codf // import "go.spiff.io/codf"

import (
	"reflect"
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
	t.Logf("-------- DOCUMENT --------\n%s\n------ END DOCUMENT ------", doc)
	return doc
}

func mustParseNamed(t *testing.T, name string, in string) *Document {
	doc := mustParse(t, in)
	doc.Name = name
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

// parseTestCase is used to describe and run a parser test, optionally as a subtest.
type parseTestCase struct {
	Name string
	Src  string
	Doc  *Document
	Fun  func(*testing.T, string) *Document
}

func (p parseTestCase) RunSubtest(t *testing.T) {
	t.Run(p.Name, p.Run)

}

func (p parseTestCase) Run(t *testing.T) {
	fn := p.Fun
	if fn == nil {
		fn = mustParse
	}
	doc := fn(t, p.Src)
	objectsEqual(t, "", doc, p.Doc)
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
			Src:  `foo 1 { bar [] #{}; } map #{} [];// eof`,
			Doc: doc().section("foo", 1).
				statement("bar", []ExprNode{}, mkmap()).
				up().statement("map", mkmap(), []ExprNode{}).
				Doc(),
		},
		{
			Name: "NestedArrayMaps",
			Src: `foo [// Nested map inside array
					#{ k [  1       // integer
						"2"     // string
						three   // bareword (string)
						true    // bool (bareword->bool)
						` + "`true`" + ` // raw quote bool
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
			Src:  `sect []#{}{stmt #{k [2]"p"#{}}true [false];}`,
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
					#{} #{k v}
					[] [v1 v2]
				{
					inside Yes YES yes yeS // Last is always a bareword here
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
				mkmap(), mkmap("k", "v"),
				mkexprs(), mkexprs("v1", "v2"),
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
		{Fun: mustNotParse, Name: "BadSectionClose", Src: `src { // comment`},
		{Fun: mustNotParse, Name: "BadSectionClose", Src: `}`},
		{Fun: mustNotParse, Name: "BadSectionClose", Src: `]`},
	}

	for _, c := range cases {
		c.RunSubtest(t)
	}
}

func TestParseExample(t *testing.T) {
	const exampleSource = `server go.spiff.io {
    listen 0.0.0.0:80;
    control unix:///var/run/httpd.sock;
    proxy unix:///var/run/go-redirect.sock {
        strip-x-headers yes;
        log-access no;
    }
    // keep caches in 64mb of memory
    cache memory 64mb {
         expire 10m 404;
         expire 1h  301 302;
         expire 5m  200;
    }
}`

	parseTestCase{
		Src: exampleSource,
		Doc: doc().
			section("server", "go.spiff.io").
			/* server */ statement("listen", "0.0.0.0:80").
			/* server */ statement("control", "unix:///var/run/httpd.sock").
			/* server */ section("proxy", "unix:///var/run/go-redirect.sock").
			/* server */ /* proxy */ statement("strip-x-headers", true).
			/* server */ /* proxy */ statement("log-access", false).
			/* server */ up().
			/* server */ section("cache", "memory", "64mb").
			/* server */ /* cache */ statement("expire", time.Minute*10, 404).
			/* server */ /* cache */ statement("expire", time.Hour, 301, 302).
			/* server */ /* cache */ statement("expire", time.Minute*5, 200).
			Doc(),
	}.Run(t)
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

func TestParseExpr(t *testing.T) {
	type testCase struct {
		name    string
		in      string
		want    ExprNode
		wantErr bool
	}

	cases := []testCase{
		{
			name: "QuotedString",
			in:   `  "quoted string"  // comment // `,
			want: mkexpr("quoted string"),
		},
		{
			name: "Raw String",
			in:   "  `raw string`  // comment //",
			want: mkexpr("raw string"),
		},
		{
			name: "Positive Digit",
			in:   "5",
			want: mkexpr(5),
		},
		{
			name: "Positive Int",
			in:   "123",
			want: mkexpr(123),
		},
		{
			name: "Negative Digit",
			in:   "-1",
			want: mkexpr(-1),
		},
		{
			name: "Negative Int",
			in:   "-123",
			want: mkexpr(-123),
		},
		{
			name: "Zero",
			in:   "0",
			want: mkexpr(0),
		},
		{
			name: "Float Zero",
			in:   "0.0",
			want: mkdec("0.0"),
		},
		{
			name: "Float Positive",
			in:   "+1.234",
			want: mkdec("1.234"),
		},
		{
			name: "Float Negative",
			in:   "-1.234",
			want: mkdec("-1.234"),
		},
		{
			name: "Float Negative Exp",
			in:   "-1.234e2",
			want: mkdec("-123.4"),
		},
		{
			name: "Float Bad Exp",
			in:   "-1.234E",
			want: mkword("-1.234E"),
		},
		{
			name: "Yes",
			in:   "yes",
			want: mkexpr(true),
		},
		{
			name: "No",
			in:   "no",
			want: mkexpr(false),
		},
		{
			name: "True",
			in:   "TRUE",
			want: mkexpr(true),
		},
		{
			name: "False",
			in:   "FALSE",
			want: mkexpr(false),
		},
		{
			name: "EmptyArray",
			in:   "[]",
			want: mkexpr([]ExprNode{}),
		},
		{
			name: "Array",
			in:   "\n[ 1 2 3 ]\n",
			want: mkexpr(mkexprs(1, 2, 3)),
		},
		{
			name: "Map",
			in:   "#{ foo 5 }",
			want: mkmap("foo", 5),
		},
		{
			name:    "Semicolon",
			in:      "1;",
			wantErr: true,
		},
		{
			name:    "MapDoubleClose",
			in:      "#{ foo 5 }}",
			wantErr: true,
		},
		{
			name:    "ArrayDoubleClose",
			in:      "[1 2 3]]",
			wantErr: true,
		},
		{
			name:    "MultipleLiterals",
			in:      "1\n2\n3\n",
			wantErr: true,
		},
		{
			name:    "Empty",
			in:      "",
			wantErr: true,
		},
		{
			name:    "Whitespace",
			in:      "  \n  ",
			wantErr: true,
		},
		{
			name:    "SpaceComment",
			in:      "  \n  // foobar\n   //bar\n\n\t\n",
			wantErr: true,
		},
		{
			name:    "Comment",
			in:      "// comment",
			wantErr: true,
		},
		{
			name:    "KeyWithoutValue",
			in:      "#{key}",
			wantErr: true,
		},
		{
			name:    "BadMapClosing",
			in:      "#{key value]",
			wantErr: true,
		},
		{
			name:    "BadMapKey",
			in:      "#{1 value}",
			wantErr: true,
		},
	}

	parser := NewParser()
	init := *parser
	init.doc = &Document{
		Children: []Node{},
	}

	for _, c := range cases {
		c := c
		t.Run(c.name, func(t *testing.T) {
			lexer := NewLexer(strings.NewReader(c.in))
			got, err := parser.ParseExpr(lexer)
			if (err != nil) != c.wantErr {
				t.Fatalf("error parsing %q; expected err = %t; got %v", c.in, c.wantErr, err)
			} else if c.wantErr {
				return
			}
			objectsEqual(t, "", got, c.want)
		})
	}

	t.Run("EnsureParserUnchanged", func(t *testing.T) {
		objectsEqual(t, "", init.doc, parser.doc)
		if !reflect.DeepEqual(parser, &init) {
			t.Fatalf("parser state changed\ngot\t%#+ v\nwant\t%#+ v", parser, &init)
		}
	})
}
