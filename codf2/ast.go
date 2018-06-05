package codf

import (
	"math/big"
	"regexp"
	"sort"
	"strconv"
	"time"
)

type parseNode interface {
	astparse()
}

type parentNode interface {
	parseNode
	addChild(node Node)
}

type Document struct {
	Children []Node
}

func (d *Document) addChild(node Node) {
	d.Children = append(d.Children, node)
}

func (*Document) astparse() {}

type Node interface {
	Token() Token
	astnode()
}

type segmentNode interface {
	parseNode
	addExpr(ExprNode) error
}

type ExprNode interface {
	Node
	Value() interface{}
}

// Statement is any single word followed by literals (Params).
type Statement struct {
	NameTok *Literal
	Params  []ExprNode

	EndTok Token
}

func (*Statement) astparse() {}

func (s *Statement) addExpr(node ExprNode) error {
	s.Params = append(s.Params, node)
	return nil
}

func (s *Statement) Name() string {
	str, _ := String(s.NameTok)
	return str
}

func (s *Statement) astnode() {}

func (s *Statement) Token() Token {
	return s.NameTok.Token()
}

func (s *Statement) promote() *Section {
	return &Section{
		NameTok:  s.NameTok,
		Params:   s.Params,
		Children: []Node{},
	}
}

// Section is a single word follow by zero or more literals.
// A Section may contain children Statements and Sections.
type Section struct {
	NameTok  *Literal
	Params   []ExprNode
	Children []Node

	StartTok Token
	EndTok   Token
}

func (*Section) astparse() {}

func (s *Section) addExpr(node ExprNode) error {
	s.Params = append(s.Params, node)
	return nil
}

func (s *Section) Name() string {
	str, _ := String(s.NameTok)
	return str
}

func (s *Section) astnode() {}

func (s *Section) Token() Token {
	return s.NameTok.Token()
}

func (s *Section) addChild(node Node) {
	s.Children = append(s.Children, node)
}

type Map struct {
	StartTok Token
	EndTok   Token
	// Elems is a map of the string keys to their key-value pairs.
	Elems map[string]*MapEntry
}

func (m *Map) astnode() {}

func (m *Map) Token() Token {
	return m.StartTok
}

func (m *Map) Value() interface{} {
	return m.Elems
}

func (m *Map) Pairs() []*MapEntry {
	entries := make([]*MapEntry, 0, len(m.Elems))
	for _, p := range m.Elems {
		entries = append(entries, p)
	}
	sortfn := func(i, j int) bool {
		return entries[i].Ord < entries[j].Ord
	}
	sort.Slice(entries, sortfn)
	return entries
}

type Array struct {
	StartTok Token
	EndTok   Token
	Elems    []ExprNode
}

func (*Array) astparse() {}

func (a *Array) addExpr(node ExprNode) error {
	a.Elems = append(a.Elems, node)
	return nil
}

func (a *Array) astnode() {}

func (a *Array) Token() Token {
	return a.StartTok
}

func (a *Array) Value() interface{} {
	return a.Elems
}

type MapEntry struct {
	// Ord is an integer for ordering entries in the map.
	// There can be gaps in Ord for a range. Duplicate keys
	// increase Ord and replace the conflicting MapEntry.
	Ord uint
	// Key and Val are the key-value pair.
	Key ExprNode
	Val ExprNode
}

func (m *MapEntry) astnode() {}

func (m *MapEntry) Token() Token {
	return m.Key.Token()
}

func (m *MapEntry) Value() interface{} {
	return m.Val.Value()
}

type Literal struct {
	Tok Token
}

func (l *Literal) astnode() {}

func (l *Literal) Token() Token {
	return l.Tok
}

func (l *Literal) Value() interface{} {
	return l.Tok.Value
}

func Value(node Node) interface{} {
	switch node := node.(type) {
	case ExprNode:
		return node.Value()
	default:
		return node.Token().Value
	}
}

func Regexp(node Node) (v *regexp.Regexp) {
	v, _ = Value(node).(*regexp.Regexp)
	return
}

func Duration(node Node) (v time.Duration, ok bool) {
	v, ok = Value(node).(time.Duration)
	return
}

func Bool(node Node) (v, ok bool) {
	v, ok = Value(node).(bool)
	return
}

func String(node Node) (str string, ok bool) {
	str, ok = Value(node).(string)
	return
}

func BigRat(node Node) (v *big.Rat) {
	v, _ = Value(node).(*big.Rat)
	return
}

func BigInt(node Node) (v *big.Int) {
	v, _ = Value(node).(*big.Int)
	return
}

func BigFloat(node Node) (v *big.Float) {
	v, _ = Value(node).(*big.Float)
	return
}

func Float64(node Node) (v float64, ok bool) {
	switch vi := Value(node).(type) {
	case *big.Int:
		return float64(vi.Int64()), vi.IsInt64()
	case *big.Rat:
		f, _ := vi.Float64()
		return f, true
	case *big.Float:
		v, _ = vi.Float64()
		return v, true
	case string:
		var err error
		v, err = strconv.ParseFloat(vi, 64)
		return v, err == nil
	}
	return 0, false
}

func Int64(node Node) (v int64, ok bool) {
	switch vi := Value(node).(type) {
	case *big.Int:
		return vi.Int64(), vi.IsInt64()
	case *big.Rat:
		if vi.IsInt() {
			return vi.Num().Int64(), vi.Num().IsInt64()
		}
		f, _ := vi.Float64()
		return int64(f), true
	case *big.Float:
		v, _ = vi.Int64()
		return v, true
	case string:
		var err error
		v, err = strconv.ParseInt(vi, 0, 64)
		return v, err == nil
	}
	return 0, false
}
