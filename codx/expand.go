package codx

import (
	"os"
	"strings"

	"go.spiff.io/codf"
)

func ExpandMapper(fn ExpandFunc, flags codf.LexerFlag) MapFunc {
	if fn == nil {
		fn = os.LookupEnv
	}
	return (&expander{
		load:  fn,
		flags: flags,
	}).mapNode
}

type ExpandFunc func(name string) (value string, ok bool)

type expander struct {
	load  ExpandFunc
	flags codf.LexerFlag
}

func (e *expander) mapNode(node codf.Node) (codf.Node, error) {
	switch node := node.(type) {
	case *codf.Statement:
		node.Params = e.expandArgs(node.Params)
	case *codf.Section:
		node.Params = e.expandArgs(node.Params)
	}
	return node, nil
}

func (e *expander) expandArgs(args []codf.ExprNode) []codf.ExprNode {
	args = append(make([]codf.ExprNode, 0, len(args)), args...)
	for i, arg := range args {
		args[i] = e.expandArg(arg)
	}
	return args
}

func (e *expander) expandArg(arg codf.ExprNode) codf.ExprNode {
	switch ex := arg.(type) {
	case *codf.Literal:
		return e.expandLiteral(ex)
	case *codf.Array:
		dup := *ex
		dup.Elems = e.expandArgs(dup.Elems)
		return &dup
	case *codf.Map:
		dup := *ex
		pairs := dup.Pairs()
		next := make(map[string]*codf.MapEntry, len(pairs))
		for _, p := range pairs {
			pd := *p
			keyexp := e.expandArg(pd.Key)
			switch keyexp.Token().Kind {
			case codf.TWord, codf.TString, codf.TRawString:
				pd.Key = keyexp
			}
			pd.Val = e.expandArg(pd.Val)
			next[p.Key.Value().(string)] = &pd
		}
		dup.Elems = next
		return &dup
	}
	return arg
}

func (e *expander) expandLiteral(arg *codf.Literal) codf.ExprNode {
	switch arg.Tok.Kind {
	case codf.TString, codf.TRawString:
		cur := arg.Tok.Value.(string)
		if strings.ContainsAny(cur, "\r\n") {
			return arg
		}
		dup := *arg
		if v, ok := e.load(cur); ok {
			dup.Tok.Value = v
			return &dup
		} else {
			return arg
		}
	case codf.TWord:
		raw := arg.Tok.Value.(string)
		after, ok := e.load(raw)
		if !ok || raw == after {
			return arg
		} else if next, err := e.parseExpr(after); err == nil {
			copyTokenLocation(next, arg.Token())
			return next
		}
	}
	return arg
}

func (e *expander) parseExpr(raw string) (codf.ExprNode, error) {
	lexer := codf.NewLexer(strings.NewReader(raw))
	lexer.Flags = e.flags
	parser := codf.NewParser()
	return parser.ParseExpr(lexer)
}

func copyTokenLocation(dst codf.Node, tok codf.Token) {
	dtok := &tok
	switch dst := dst.(type) {
	case *codf.Statement:
		dtok = &dst.NameTok.Tok
	case *codf.Section:
		dtok = &dst.NameTok.Tok
	case *codf.Literal:
		dtok = &dst.Tok
	case *codf.Array:
		dtok = &dst.StartTok
	case *codf.Map:
		dtok = &dst.StartTok
	}
	dtok.Start, dtok.End = tok.Start, tok.End
}
