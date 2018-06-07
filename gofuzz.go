// +build gofuzz

package codf

import (
	"bytes"
)

func Fuzz(b []byte) (rc int) {
	lex := NewLexer(bytes.NewReader(b))
	p := NewParser()
	p.Parse(lex)
	return 0
}
