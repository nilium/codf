package codf

import (
	"fmt"
	"testing"
)

type docWalker struct {
	name       string
	statements map[string]struct{}
	sections   map[string]*docWalker
}

func (d *docWalker) Statement(stmt *Statement) error {
	logf("STATEMENT: %v", stmt)
	if _, ok := d.statements[stmt.Name()]; !ok {
		return fmt.Errorf("invalid statement: %s", stmt.Name())
	}
	return nil
}

func (d *docWalker) EnterSection(sec *Section) (Walker, error) {
	logf("ENTER: %v", sec.Name())
	if sub := d.sections[sec.Name()]; sub != nil {
		return sub, nil
	}
	return nil, fmt.Errorf("invalid section: %s", sec.Name())
}

func (d *docWalker) ExitSection(_ Walker, sec *Section, parent ParentNode) error {
	logf("EXIT: %v", sec.Name())
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
	doc := mustParse(t, DocSource)
	doc.addChild(mustParse(t, ``))
	doc.addChild(mustParse(t, `
		worker_processes auto;
	`))
	doc.addChild(mustParse(t, `
		worker_processes 1;
		user httpd;
		user www-data;
		user nobody;
		daemon yes;
		daemon true;
		daemon false;
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

	t.Run("InvalidNestedStatement", func(t *testing.T) {
		defer setlogf(t)()
		def := sectionWalker("main",
			"user",
			"daemon",
			"worker_processes",
			sectionWalker("http",
				"sendfile",

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

		if err := Walk(doc, def); err == nil {
			t.Fatal("expected error on keepalive_timeout statement")
		} else {
			t.Log(err)
		}
	})

	t.Run("InvalidNestedDocument", func(t *testing.T) {
		defer setlogf(t)()
		def := sectionWalker("main",
			"user",
			"daemon",
			sectionWalker("http",
				"sendfile",
				"keepalive_timeout",

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

		if err := Walk(doc, def); err == nil {
			t.Fatal("expected error on worker_processes statement")
		} else {
			t.Log(err)
		}
	})
}
