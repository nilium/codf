package codf

import (
	"errors"
	"fmt"
	"reflect"
	"testing"
)

type mapFunc func(Node) (Node, error)

var _ WalkMapper = mapFunc(nil)

func (f mapFunc) Map(n Node) (Node, error) {
	return f(n)
}

func (f mapFunc) Statement(*Statement) error            { return nil }
func (f mapFunc) EnterSection(*Section) (Walker, error) { return f, nil }

type docWalker struct {
	name       string
	statements map[string]struct{}
	sections   map[string]*docWalker
}

func (d *docWalker) Statement(stmt *Statement) error {
	if _, ok := d.statements[stmt.Name()]; !ok {
		return fmt.Errorf("invalid statement: %s", stmt.Name())
	}
	return nil
}

func (d *docWalker) EnterSection(sec *Section) (Walker, error) {
	if sub := d.sections[sec.Name()]; sub != nil {
		return sub, nil
	}
	return nil, fmt.Errorf("invalid section: %s", sec.Name())
}

func (d *docWalker) ExitSection(_ Walker, sec *Section, parent ParentNode) error {
	if sec.Name() != d.name {
		return fmt.Errorf("ExitSection called with invalid section named %s", sec.Name())
	}
	return nil
}

func sectionWalker(name string, children ...interface{}) *docWalker {
	w := &docWalker{
		name:       name,
		statements: map[string]struct{}{},
		sections:   map[string]*docWalker{},
	}

	for _, child := range children {
		switch child := child.(type) {
		case *docWalker:
			w.sections[child.name] = child
		case string:
			w.statements[child] = struct{}{}
		default:
			panic(fmt.Errorf("invalid walker child type: %T", child))
		}
	}
	return w
}

func TestWalk(t *testing.T) {
	defer setlogf(t)()
	const DocSource = `
	user http;
	daemon no;

	http {
		server {
			server_name go.spiff.io;
			listen 80;
			listen 443 http2 ssl;

			location / {
				root /var/www/public;
				index index.html index.htm;
			}
		}

		sendfile yes;
		keepalive_timeout 65s;
	}
	`
	doc := mustParseNamed(t, "root.conf", DocSource)
	doc.addChild(mustParseNamed(t, "empty.conf", ``))
	doc.addChild(mustParseNamed(t, "include_root.conf", `
		worker_processes 1;
		user httpd;
		user www-data;
		user nobody;
		daemon yes;
		daemon true;
		daemon false;
	`))
	doc.Nodes()[2].(*Section).addChild(mustParseNamed(t, "include_nested.conf", `
		sendfile no;
		keepalive_timeout 10s;
		cache /var/run/sv/cache 3;
		cache_proxy [200 301 302 404] 5m; ' defaults to cache_proxy [] 0
	`))

	t.Run("Valid", func(t *testing.T) {
		defer setlogf(t)()
		def := sectionWalker("main",
			"user",
			"daemon",
			"worker_processes",
			sectionWalker("http",
				"sendfile",
				"keepalive_timeout",
				"cache",
				"cache_proxy",

				sectionWalker("server",
					"server_name",
					"listen",
					sectionWalker("location",
						"root",
						"index",
					),
				),
			),
		)

		if err := Walk(doc, def); err != nil {
			t.Fatal(err)
		}
	})

	t.Run("InvalidNestedSection", func(t *testing.T) {
		defer setlogf(t)()
		def := sectionWalker("main",
			"user",
			"daemon",
			"worker_processes",
			sectionWalker("http",
				"sendfile",
			),
		)

		if err := Walk(doc, def); err == nil {
			t.Fatal("expected error on server section")
		} else {
			t.Log(err)
		}
	})

	checkStatementErr := func(t *testing.T, err error, wantDoc string, wantName string) {
		fail := func() {
			t.Fatalf("expected error on %s statement; got %v", wantName, err)
		}
		we, ok := err.(*WalkError)
		if !ok {
			fail()
		}
		if stmt, ok := we.Node.(*Statement); !ok || stmt.Name() != wantName {
			fail()
		}
		t.Log(err)

		docName := ""
		if we.Document != nil {
			docName = we.Document.Name
		}
		if docName != wantDoc {
			t.Errorf("expected error in document %q; got %q", wantDoc, docName)
		}
	}

	t.Run("InvalidNestedStatement", func(t *testing.T) {
		defer setlogf(t)()
		def := sectionWalker("main",
			"user",
			"daemon",
			"worker_processes",
			sectionWalker("http",
				"sendfile",
				"cache",
				"cache_proxy",

				sectionWalker("server",
					"server_name",
					"listen",
					sectionWalker("location",
						"root",
						"index",
					),
				),
			),
		)

		err := Walk(doc, def)
		checkStatementErr(t, err, "root.conf", "keepalive_timeout")
	})

	t.Run("InvalidNestedDocumentInDocument", func(t *testing.T) {
		defer setlogf(t)()
		def := sectionWalker("main",
			"user",
			"daemon",
			sectionWalker("http",
				"sendfile",
				"keepalive_timeout",
				"cache",
				"cache_proxy",

				sectionWalker("server",
					"server_name",
					"listen",
					sectionWalker("location",
						"root",
						"index",
					),
				),
			),
		)

		err := Walk(doc, def)
		checkStatementErr(t, err, "include_root.conf", "worker_processes")
	})

	t.Run("InvalidNestedDocumentInSection", func(t *testing.T) {
		defer setlogf(t)()
		def := sectionWalker("main",
			"user",
			"daemon",
			"worker_processes",
			sectionWalker("http",
				"sendfile",
				"keepalive_timeout",
				"cache",

				sectionWalker("server",
					"server_name",
					"listen",
					sectionWalker("location",
						"root",
						"index",
					),
				),
			),
		)

		err := Walk(doc, def)
		checkStatementErr(t, err, "include_nested.conf", "cache_proxy")
	})
}

type walkDeleteNested struct {
	statements []string
}

func (w *walkDeleteNested) Statement(s *Statement) error {
	w.statements = append(w.statements, s.Name())
	return nil
}

func (w *walkDeleteNested) EnterSection(s *Section) (Walker, error) {
	return nil, fmt.Errorf("encountered section %s", s.Name())
}

func (w *walkDeleteNested) Map(n Node) (Node, error) {
	switch n.(type) {
	case *Section:
		return nil, nil
	}
	return n, nil
}

func TestWalkMapper(t *testing.T) {
	defer setlogf(t)()
	const DocSource = `
	user http;
	daemon no;

	http {
		server {
			server_name go.spiff.io;
			listen 80;
			listen 443 http2 ssl;

			location / {
				root /var/www/public;
				index index.html index.htm;
			}
		}

		sendfile yes;
		keepalive_timeout 65s;
	}
	`
	doc := mustParseNamed(t, "root.conf", DocSource)
	doc.addChild(mustParseNamed(t, "child.conf", `
		sub-section {}
		statement true;
	`))
	t.Logf("-- BEFORE --\n%v", doc)

	want := []Node{doc.Children[0], doc.Children[1], doc.Children[3]}
	walker := new(walkDeleteNested)
	err := Walk(doc, walker)

	if err != nil {
		t.Fatal(err)
	}

	if !reflect.DeepEqual(doc.Children, want) {
		t.Errorf("children = %#+v; want %#+v", doc.Children, want)
	}
	t.Logf("-- AFTER --\n%v", doc)

	wantStatements := []string{
		"user",
		"daemon",
		"statement",
	}

	if !reflect.DeepEqual(walker.statements, wantStatements) {
		t.Errorf("statements = %q; want %q", walker.statements, wantStatements)
	}
}

func TestMapDelete(t *testing.T) {
	const DocSource = `stmt 1; section arg {}`

	defer setlogf(t)()

	doc := mustParseNamed(t, "root.conf", DocSource)
	err := Walk(doc, mapFunc(func(Node) (Node, error) { return nil, nil }))
	if err != nil {
		t.Fatalf("err = %v; want nil", err)
	}

	if got := doc.Children; len(got) != 0 {
		t.Fatalf("children = %#v; want an empty slice", got)
	}
}

func TestMapError(t *testing.T) {
	const wantErrMessage = `[1:1:0] root.conf: stmt in main: expected error`
	const DocSource = `stmt 1; section arg {}`

	defer setlogf(t)()

	doc := mustParseNamed(t, "root.conf", DocSource)
	doc.Children = append([]Node{nil}, doc.Children...)

	wantErr := errors.New("expected error")
	err := Walk(doc, mapFunc(func(Node) (Node, error) { return nil, wantErr }))

	if got := err.Error(); got != wantErrMessage {
		t.Fatalf("err = %q; want %q", got, wantErrMessage)
	}
}
