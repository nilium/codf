package codf

import (
	"fmt"
)

type tokenConsumer func(Token) (tokenConsumer, error)

// TokenReader is anything capable of reading a token and returning either it or an error.
type TokenReader interface {
	ReadToken() (Token, error)
}

// Parser consumes tokens from a TokenReader and constructs a codf *Document from it.
//
// The Document produced by the Parser is kept for the duration of the parser's lifetime, so it is
// possible to read multiple TokenReaders into a Parser and produce a combined document.
type Parser struct {
	doc  *Document
	next tokenConsumer

	lastToken Token
	lastErr   error

	// parseErr is the last error returned by Parse() -- if any error occurs during Parse,
	// subsequent calls to Parse will return this.
	parseErr error

	ctx  []parseNode
	_ctx [6]parseNode
}

// NewParser allocates a new *Parser and returns it.
func NewParser() *Parser {
	doc := &Document{
		Children: []Node{},
	}
	p := &Parser{
		doc:  doc,
		next: nil,
	}

	p.ctx = p._ctx[:0]

	return p
}

func (p *Parser) nextToken(tr TokenReader) (tok Token, err error) {
	tok, err = tr.ReadToken()
	p.lastToken, p.lastErr = tok, err
	return tok, err
}

// Parse consumes tokens from a TokenReader and constructs a Document from its tokens.
//
// If an error occurs during parsing, Parse will return that error for all subsequent calls to
// Parse, as the parser has been left in a middle-of-parsing state.
func (p *Parser) Parse(tr TokenReader) (err error) {
	if p.parseErr != nil {
		return p.parseErr
	}

	defer func() {
		if err != nil {
			p.parseErr = err
		}
	}()

	if p.next == nil {
		p.next = p.beginSegment
	}

	var tok Token
	for p.next != nil {
		tok, err = p.nextToken(tr)
		if err != nil {
			return err
		}
		if p.next, err = p.next(tok); err != nil {
			return err
		}
	}

	return nil
}

// Document returns the document constructed by Parser.
// Each call to Parse() modifies the Document, so it is unsafe to use the Document from multiple
// goroutines during parsing.
func (p *Parser) Document() *Document {
	return p.doc
}

// TODO: Add ParseInContext() method to begin parsing while inside of a specific section or
// document. Useful for handling, for example, `include file.conf;` inside of a config file as
// a part of walking an AST.

// pushContext pushes a new node-parsing context onto the parser stack.
func (p *Parser) pushContext(node parseNode) {
	p.ctx = append(p.ctx, node)
}

// popContext pops the current node-parsing context from the parser stack.
// The previous context on the stack takes its place.
//
// Calling this while the stack is empty will panic.
func (p *Parser) popContext() parseNode {
	n := len(p.ctx) - 1
	if n < 0 {
		panic("cannot pop document from parsing stack")
	}
	ctx := p.ctx[n]
	p.ctx[n] = nil
	p.ctx = p.ctx[:n]
	return ctx
}

// context returns the current node-parsing context on the stack.
// If the stack is empty, this returns the document, since it is the implied root of the stack.
func (p *Parser) context() parseNode {
	n := len(p.ctx) - 1
	if n < 0 {
		return p.doc
	}
	return p.ctx[n]
}

func (p *Parser) closeError(tok Token) error {
	switch ctx := p.context().(type) {
	case *Statement:
		return unexpected(tok, "expected end of statement %q beginning at %v",
			ctx.Name(), ctx.Token().Start)
	case *Section:
		return unexpected(tok, "expected end of section %q beginning at %v",
			ctx.Name(), ctx.Token().Start)
	case *Array:
		return unexpected(tok, "expected end of array beginning at %v",
			ctx.Token().Start)
	case *mapBuilder:
		if ctx.k != nil {
			return unexpected(tok, "expected value for key %q at %v",
				ctx.k.Token().Value, ctx.k.Token().Start)
		}
		return unexpected(tok, "expected end of map beginning at %v",
			ctx.m.Token().Start)
	case *Document:
		if tok.Kind != TEOF {
			return unexpected(tok, "expected statement, section, or EOF")
		}
		return nil
	}
	panic("unreachable")
}

func (p *Parser) beginSegment(tok Token) (tokenConsumer, error) {
	switch tok.Kind {
	case TSemicolon, TWhitespace, TComment:
		return p.beginSegment, nil
	case TCurlClose:
		if sect, ok := p.context().(*Section); ok {
			sect.EndTok = tok
			p.popContext()
			p.context().(parentNode).addChild(sect)
			return p.beginSegment, nil
		}
		return nil, p.closeError(tok)
	case TEOF:
		return nil, p.closeError(tok)
	case TWord:
		// Start statement
		stmt := &Statement{NameTok: &Literal{tok}}
		p.pushContext(stmt)
		return skipWhitespace(p.parseStatement), nil
	}
	return nil, unexpected(tok, "expected statement or section name")
}

func skipWhitespace(next tokenConsumer) (consumer tokenConsumer) {
	consumer = func(tok Token) (tokenConsumer, error) {
		switch tok.Kind {
		case TWhitespace, TComment:
			return consumer, nil
		}
		return next(tok)
	}
	return consumer
}

func (p *Parser) parseStatementSentinel(tok Token) (tokenConsumer, error) {
	switch tok.Kind {
	case TEOF:
		return nil, p.closeError(tok)

	case TSemicolon:
		if stmt, ok := p.context().(*Statement); ok {
			p.popContext()
			stmt.EndTok = tok
			p.context().(parentNode).addChild(stmt)
			return p.beginSegment, nil
		}
		return nil, p.closeError(tok)

	case TBracketClose:
		if ary, ok := p.context().(*Array); ok {
			p.popContext()
			ary.EndTok = tok
			if err := p.context().(segmentNode).addExpr(ary); err != nil {
				return nil, err
			}
			return skipWhitespace(p.parseStatement), nil
		}
		return nil, p.closeError(tok)

	case TCurlClose:
		if mb, ok := p.context().(*mapBuilder); ok {
			if mb.k != nil {
				return nil, unexpected(tok, "expected value for key %q at %v",
					mb.m.Token().Value, mb.m.Token().Start)
			}
			p.popContext()
			m := mb.m
			m.EndTok = tok
			if err := p.context().(segmentNode).addExpr(m); err != nil {
				return nil, err
			}
			return skipWhitespace(p.parseStatement), nil
		}
		return nil, p.closeError(tok)

	case TCurlOpen:
		if stmt, ok := p.context().(*Statement); ok {
			p.popContext()
			sect := stmt.promote()
			sect.StartTok = tok
			p.pushContext(sect)
			return p.beginSegment, nil
		}
		return nil, p.closeError(tok)
	}
	return nil, unexpected(tok, "expected statement body")
}

func (p *Parser) beginArray(tok Token) (tokenConsumer, error) {
	p.pushContext(&Array{
		StartTok: tok,
		Elems:    []ExprNode{},
	})
	return skipWhitespace(p.parseStatement), nil
}

func (p *Parser) beginMap(tok Token) (tokenConsumer, error) {
	m := newMapBuilder()
	m.m.StartTok = tok
	p.pushContext(m)
	return skipWhitespace(p.parseStatement), nil
}

func (p *Parser) parseStatement(tok Token) (tokenConsumer, error) {
	switch tok.Kind {
	case TBracketOpen:
		return p.beginArray(tok)
	case TMapOpen:
		return p.beginMap(tok)
	case TInteger,
		TBaseInt,
		TBinary,
		TOctal,
		THex,
		TDecimal,
		TDuration,
		TRational,
		TString,
		TWord,
		TBoolean,
		TRegexp:

		if tok.Kind == TWord {
			tok = wordToBool(tok)
		}

		if err := p.context().(segmentNode).addExpr(&Literal{tok}); err != nil {
			return nil, err
		}
		return skipWhitespace(p.parseStatement), nil
	}

	return p.parseStatementSentinel(tok)
}

// ExpectedError is returned when a token, Tok, is encountered that does not meet expectations.
type ExpectedError struct {
	// Tok is the token that did not meet expectations.
	Tok Token
	// Msg is a message describing the expected token(s).
	Msg string
}

func unexpected(tok Token, msg string, args ...interface{}) *ExpectedError {
	return &ExpectedError{
		Tok: tok,
		Msg: fmt.Sprintf(msg, args...),
	}
}

// Error is an implementation of error.
func (e *ExpectedError) Error() string {
	return "[" + e.Tok.Start.String() + "] unexpected " + e.Tok.Kind.String() + ": " + e.Msg
}

type mapBuilder struct {
	ord uint
	m   *Map
	k   ExprNode
}

func newMapBuilder() *mapBuilder {
	return &mapBuilder{
		m: &Map{
			Elems: map[string]*MapEntry{},
		},
	}
}

var _ segmentNode = (*mapBuilder)(nil)

func (*mapBuilder) astparse() {}

func (m *mapBuilder) addExpr(expr ExprNode) error {
	if m.k == nil {
		switch expr.Token().Kind {
		case TWord, TString:
			m.k = expr
			return nil
		}
		return unexpected(expr.Token(), "bad key; expected word or string")
	}

	ks, ok := String(m.k)
	if !ok {
		return fmt.Errorf("key token %q at %v invalid: value must be a string",
			m.k.Token().Raw, m.k.Token().Start)
	}

	entry := m.m.Elems[ks]
	if entry == nil {
		entry = &MapEntry{}
		m.m.Elems[ks] = entry
	}
	*entry = MapEntry{
		Ord: m.ord,
		Key: m.k,
		Val: expr,
	}
	m.k = nil
	m.ord++

	return nil
}

func wordToBool(tok Token) Token {
	if tok.Kind != TWord {
		return tok
	}
	s, ok := tok.Value.(string)
	if !ok {
		return tok
	}
	switch s {
	case "TRUE", "True", "true", "YES", "Yes", "yes":
		tok.Kind, tok.Value = TBoolean, true
	case "FALSE", "False", "false", "NO", "No", "no":
		tok.Kind, tok.Value = TBoolean, false
	}
	return tok
}
