// +build gofuzz

package codf // import "go.spiff.io/codf"

import (
	"bytes"
	"fmt"
)

func Fuzz(b []byte) (rc int) {
	lex := NewLexer(bytes.NewReader(b))
	p := NewParser()
	p.Parse(lex)
	return 0
}

func FuzzLexer(b []byte) (rc int) {
	lex := NewLexer(bytes.NewReader(b))
	for {
		t, err := lex.ReadToken()
		if err != nil || t.Kind == TEOF {
			return
		}
	}
}

type fuzzWalker struct{}

func (w *fuzzWalker) Statement(st *Statement) error {
	switch st.Name() {
	case "server_name",
		"listen",
		"proxy_pass",
		"add_header",
		"user",
		"daemon",
		"log_format",
		"acces_log",
		"error_log":
		return nil
	default:
		return fmt.Errorf("invalid statement name: %s", st.Name())
	}
}

func (w *fuzzWalker) EnterSection(sec *Section) (Walker, error) {
	switch sec.Name() {
	case "http",
		"server",
		"location",
		"upstream",
		"stream":
		return w, nil
	default:
		return nil, fmt.Errorf("invalid section name: %s", sec.Name())
	}
}

func FuzzWalker(b []byte) (rc int) {
	lex := NewLexer(bytes.NewReader(b))
	p := NewParser()
	if err := p.Parse(lex); err != nil {
		// No panic, but can't use the document
		return 0
	}
	Walk(p.Document(), new(fuzzWalker))
	return 0
}
