package codx

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"sort"

	"go.spiff.io/codf"
	"golang.org/x/xerrors"
)

type IncludeFunc func(path string) (*codf.Document, error)

func IncludeFromFilesystem(flags codf.LexerFlag) IncludeFunc {
	return func(path string) (*codf.Document, error) {
		f, err := os.Open(path)
		if err != nil {
			return nil, xerrors.Errorf("error including file: %s: %v", path, err)
		}
		defer f.Close()

		br := bufio.NewReader(f)
		l := codf.NewLexer(br)
		l.Flags = flags
		l.Name = filepath.Clean(path)

		p := codf.NewParser()
		doc := p.Document()

		if err := p.Parse(l); err != nil {
			return nil, err
		}

		return doc, nil
	}
}

func IncludeMapper(include IncludeFunc) MapFunc {
	if include == nil {
		panic("include must not be nil")
	}

	return func(node codf.Node) (codf.Node, error) {
		const kwInclude = "include"

		stmt, ok := node.(*codf.Statement)
		if !ok || stmt.Name() != kwInclude {
			return node, nil
		}

		var glob string
		args := stmt.Parameters()
		if len(args) != 1 {
			return node, xerrors.Errorf("expected one (1) argument to include statment, got %d", len(args))
		} else if glob, ok = codf.String(args[0]); !ok {
			return node, xerrors.Errorf("expected string argument to include statement, got %v", args[0].Token().Kind)
		}

		paths, err := filepath.Glob(glob)
		if err != nil {
			return nil, fmt.Errorf("error including %s: %v", glob, err)
		}

		sort.Strings(paths)
		docs := make([]codf.Node, len(paths))
		root := &codf.Document{
			Name:     glob,
			Children: docs,
		}
		for i, path := range paths {
			doc, err := include(path)
			if err != nil {
				return nil, err
			}
			docs[i] = doc
		}

		return root, nil
	}
}
