package codf

import (
	"fmt"
	"math/big"
	"reflect"
	"strconv"
)

func signedText(text string, sign int) string {
	switch sign {
	case -1:
		return "-" + text
	case 1:
		return text
	}
	panic("invalid sign: " + strconv.Itoa(sign))
}

func (p *Parser) init() {
	if p.root == nil {
		p.root = &Root{}
	}
	p.consumers = []consumer{p.root}
}

func (p *Parser) Root() *Root {
	return p.root
}

var (
	typeString = reflect.TypeOf(String(""))
	typeSymbol = reflect.TypeOf(Symbol(""))
	typeRegexp = reflect.TypeOf(Regexp{})
)

type consumer interface {
	consume(...Expr)
	peek() Expr
}

type closer interface {
	consumer
	close(parser *Parser, next consumer)
}

// type stage interface {
// 	next(string) // Consume a string
// 	expr(Expr)   // Consume an Expr
// 	close()
// }

type StrError struct {
	Type reflect.Type
	Str  string
	Err  error
}

func (e *StrError) Error() string {
	return fmt.Sprintf("error parsing %s %q: %v", e.Type.Name(), e.Str, e.Err)
}

func unquote(kind reflect.Type, text string) string {
	str, err := strconv.Unquote(text)
	if err != nil {
		panic(&StrError{
			Type: kind,
			Str:  text,
			Err:  err,
		})
	}
	return str
}

func (p *Parser) Run() (err error) {
	if p.err != nil {
		return p.err
	}
	defer func() {
		if rc := recover(); rc == nil {
		} else if perr, ok := rc.(error); ok {
			err = perr
		} else {
			panic(rc)
		}
		p.err = err
	}()
	p.Execute()
	return p.err
}

func (p *Parser) peek() Expr {
	if ith := len(p.consumers) - 1; ith >= 0 {
		return p.consumers[ith].peek()
	}
	panic("no consumer")
}

func (p *Parser) beginStatement(name string) {
	p.pushConsumer(&Statement{name: name})
}

func (p *Parser) closeStatement() {
	p.popConsumer()
}

func (p *Parser) beginSection(name string) {
	p.pushConsumer(&Section{name: name})
}

func (p *Parser) closeSection() {
	p.popConsumer()
}

func (p *Parser) consume(exprs ...Expr) {
	if ith := len(p.consumers) - 1; ith >= 0 {
		p.consumers[ith].consume(exprs...)
		return
	}
	panic("no consumer")
}

func (p *Parser) tip() consumer {
	return p.consumers[len(p.consumers)-1]
}

func (p *Parser) pushConsumer(c consumer) {
	p.consumers = append(p.consumers, c)
}

func (p *Parser) beginArray() {
	p.pushConsumer(&arrayBuilder{ary: Array{}})
}

func (p *Parser) popConsumer() {
	ith := len(p.consumers) - 1
	pred := p.consumers[ith]
	p.consumers[ith], p.consumers = nil, p.consumers[:ith]

	next := consumer(p)
	if ith > 0 {
		next = p.consumers[ith-1]
	}

	if pred, ok := pred.(closer); ok {
		pred.close(p, next)
	}
}

func (p *Parser) closeArray() {
	p.popConsumer()
}

func (p *Parser) beginRegexp() {
	p.pushConsumer(&regexpBuilder{})
}

func (p *Parser) closeRegexp() {
	p.popConsumer()
}

func (p *Parser) beginMap() {
	p.pushConsumer(newMapBuilder())
}

func (p *Parser) closeMap() {
	p.popConsumer()
}

func (p *Parser) consumeInteger(str string, base int) {
	var x big.Int
	if _, ok := x.SetString(signedText(str, p.sign), 8); !ok {
		panic(fmt.Errorf("malformed base-%d integer: %q", base, str))
	}
	p.consume(Integer{&x})
}

func (p *Parser) consumeFloat(str string) {
	var x big.Float
	if _, ok := x.SetString(signedText(str, p.sign)); !ok {
		panic(fmt.Errorf("malformed decimal: %q", str))
	}
	p.consume(Decimal{&x})
}

func (p *Parser) consumeRational(str string) {
	var x big.Rat
	if _, ok := x.SetString(signedText(str, p.sign)); !ok {
		p.err = fmt.Errorf("malformed rational: %q", str)
		return
	}
	p.consume(Rational{&x})
}

func (p *Parser) parseBase(text string) {
	if p.baseIntBase, p.err = strconv.Atoi(text); p.err != nil {
		panic(fmt.Errorf("invalid integer base %q: %v", text, p.err))
	}
}
