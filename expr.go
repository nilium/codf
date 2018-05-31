package codf

import (
	"fmt"
	"math/big"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

type segmentBox interface {
	addSegment(Segment)
}

type Root struct {
	Children []Segment
}

func (r *Root) close() {
	panic("attempt to close root scope")
}

func (r *Root) consume(...Expr) {
	panic("attempt to add expressions to root scope")
}

func (r *Root) peek() Expr {
	panic("attempt to peek expression at root scope")
}

func (r *Root) String() string {
	pieces := make([]string, len(r.Children))
	for i, seg := range r.Children {
		pieces[i] = seg.String()
	}
	return strings.Join(pieces, " ")
}

func (r *Root) addSegment(seg Segment) {
	r.Children = append(r.Children, seg)
}

type Expr interface {
	expr()

	fmt.Stringer
}

func bind(ex Expr) Expr {
	type lateBindingExpr interface {
		bind() Expr
	}

	if be, ok := ex.(lateBindingExpr); ok {
		return be.bind()
	}
	return ex
}

type Segment interface {
	Name() string
	Params() Array
	Children() []Segment

	fmt.Stringer
}

type Section struct {
	name   string
	params Array
	segs   []Segment
}

func (s *Section) String() string {
	pieces := make([]string, 1, 3+len(s.params)+len(s.segs))
	pieces[0] = s.name
	for _, par := range s.params {
		pieces = append(pieces, par.String())
	}

	pieces = append(pieces, "{")
	for _, seg := range s.segs {
		pieces = append(pieces, seg.String())
	}
	pieces = append(pieces, "}")

	return strings.Join(pieces, " ")
}

var _ Segment = (*Section)(nil)

func (s *Section) Name() string {
	return s.name
}

func (s *Section) Params() Array {
	return s.params
}

func (s *Section) Children() []Segment {
	return s.segs
}

var _ closer = (*Section)(nil)

func (s *Section) addSegment(seg Segment) {
	s.segs = append(s.segs, seg)
}

func (s *Section) close(_ *Parser, c consumer) {
	c.(segmentBox).addSegment(s)
}

func (s *Section) consume(exprs ...Expr) {
	s.params = append(s.params, exprs...)
}

func (s *Section) peek() Expr {
	return s.params[len(s.params)-1]
}

type Statement struct {
	name   string
	params Array
}

func (s *Statement) String() string {
	pieces := make([]string, 1, 1+len(s.params))
	pieces[0] = s.name
	for _, par := range s.params {
		pieces = append(pieces, par.String())
	}
	return strings.Join(pieces, " ") + ";"
}

var _ Segment = (*Statement)(nil)

func (s *Statement) Name() string {
	return s.name
}

func (s *Statement) Params() Array {
	return s.params
}

func (s *Statement) Children() []Segment {
	return nil
}

var _ closer = (*Statement)(nil)

func (s *Statement) close(_ *Parser, c consumer) {
	c.(segmentBox).addSegment(s)
}

func (s *Statement) consume(exprs ...Expr) {
	s.params = append(s.params, exprs...)
}

func (s *Statement) peek() Expr {
	return s.params[len(s.params)-1]
}

type Integer struct {
	*big.Int
}

func (i Integer) Value() *big.Int {
	return i.Int
}

func (Integer) expr() {}

type Rational struct {
	*big.Rat
}

func (i Rational) Value() *big.Rat {
	return i.Rat
}

func (Rational) expr() {}

type Decimal struct {
	*big.Float
}

func (d Decimal) Value() *big.Float {
	return d.Float
}

func (d Decimal) Float32() float32 {
	f, _ := d.Float.Float32()
	return f
}

func (d Decimal) Float64() float64 {
	f, _ := d.Float.Float64()
	return f
}

func (Decimal) expr() {}

type String string

func (String) mapkey() {}

func (s String) Value() string {
	return string(s)
}

func (String) expr() {}

func (s String) String() string {
	return strconv.Quote(string(s))
}

type regexpBuilder struct {
	parts []string
}

func (r *regexpBuilder) bind() Expr {
	rs := strings.Join(r.parts, "")
	rx, err := regexp.Compile(rs)
	if err != nil {
		panic(&StrError{
			Type: typeRegexp,
			Str:  rs,
			Err:  err,
		})
	}
	return Regexp{rx}
}

func (r *regexpBuilder) add(s string) {
	r.parts = append(r.parts, s)
}

func (*regexpBuilder) consume(...Expr) {
	panic("invalid consume on regexp")
}

func (*regexpBuilder) peek() Expr {
	panic("invalid peek on regexp")
}

func (r *regexpBuilder) close(_ *Parser, next consumer) {
	next.consume(r.bind())
}

type Regexp struct {
	*regexp.Regexp
}

func (Regexp) expr() {}

func (r Regexp) Value() *regexp.Regexp {
	return r.Regexp
}

func (r Regexp) String() string {
	rs := r.Regexp.String()
	return "#/" + strings.Replace(rs, `/`, `\/`, -1) + "/"
}

type arrayBuilder struct {
	ary Array
}

func (*arrayBuilder) expr() {}

func (a *arrayBuilder) bind() Expr {
	return a.ary
}

func (a *arrayBuilder) consume(exprs ...Expr) {
	a.ary = append(a.ary, exprs...)
}

func (a *arrayBuilder) close(_ *Parser, next consumer) {
	next.consume(a.ary)
}

func (a *arrayBuilder) peek() Expr {
	if n := len(a.ary) - 1; n >= 0 {
		return a.ary[n]
	}
	panic("peek on empty array stack")
}

func (a *arrayBuilder) pop() Expr {
	if n := len(a.ary) - 1; n >= 0 {
		expr := a.ary[n]
		a.ary = a.ary[:n]
		return expr
	}
	panic("pop on empty array stack")
}

type Array []Expr

func (Array) expr() {}

func (a Array) Value() []Expr {
	return a
}

func (a Array) String() string {
	if a == nil {
		return ""
	}
	strs := make([]string, len(a))
	for i, v := range a {
		strs[i] = v.String()
	}
	return "[" + strings.Join(strs, " ") + "]"
}

type Symbol string

func (Symbol) mapkey() {}

func (s Symbol) Value() string {
	return string(s)
}

func (Symbol) expr() {}

func (s Symbol) String() string {
	return "#" + String(s).String()
}

type Bool bool

func (b Bool) Value() bool {
	return bool(b)
}

func (Bool) expr() {}

func (b Bool) String() string {
	return strconv.FormatBool(bool(b))
}

type Key interface {
	Expr

	mapkey()
	Value() string
}

type mapBuilder struct {
	key Key
	m   Map
}

func newMapBuilder() *mapBuilder {
	return &mapBuilder{
		m: Map{},
	}
}

func (m *mapBuilder) bind() Expr {
	if m.key != nil {
		panic(fmt.Errorf("malformed map: key %s does not have a value", m.key))
	}
	return m.m
}

func (m *mapBuilder) consume(exprs ...Expr) {
	if m.key != nil {
		m.key, m.m[m.key] = nil, exprs[0]
		return
	}
	m.key = exprs[0].(Key)
}

func (m *mapBuilder) peek() Expr {
	panic("peek on map builder")
}

func (m *mapBuilder) close(_ *Parser, next consumer) {
	next.consume(m.bind())
}

type Map map[Key]Expr

func (Map) expr() {}

func (m Map) String() string {
	parts := make([]string, 1, 2+len(m))
	parts[0] = "#{"
	for k, v := range m {
		parts = append(parts, k.String()+" "+v.String())
	}
	sort.Strings(parts[1:])
	parts = append(parts, "}")
	return strings.Join(parts, " ")
}
