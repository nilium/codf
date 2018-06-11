package codf // import "go.spiff.io/codf"

import (
	"math/big"
	"regexp"
	"sort"
	"strings"
	"time"
)

// Node is any parsed element of a codf document. This includes the Section, Statement, Literal,
// Array, Map, and other types.
type Node interface {
	Token() Token
	astnode()
	format(prefix string) string
}

// Each iterates over each child or parameter of the base node and passes its index and the Node to
// fn. If fn returns an error, it breaks and returns the error.
//
// Each does not recursively iterate over nodes.
func Each(base Node, fn func(int, Node) error) error {
	switch base := base.(type) {
	case ParentNode:
		for i, ch := range base.Nodes() {
			if err := fn(i, ch); err != nil {
				return err
			}
		}
	case ParamNode:
		for i, ch := range base.Parameters() {
			if err := fn(i, ch); err != nil {
				return err
			}
		}
	}
	return nil
}

// Select selects any child ParamNode of the base node whose name is present in names.
// This can be used to grab specific statements or sections as needed from the AST.
func Select(base Node, names ...string) []Node {
	if len(names) == 0 {
		return nil
	}

	par, ok := base.(ParentNode)
	if !ok {
		return nil
	}

	subs := par.Nodes()
	nodes := make([]Node, 0, len(subs))

	match := func(name string) bool { return name == names[0] }
	if len(names) > 1 {
		nameset := make(map[string]struct{}, len(names))
		for _, k := range names {
			nameset[k] = struct{}{}
		}
		match = func(name string) bool {
			_, ok := nameset[name]
			return ok
		}
	}

	for _, sub := range subs {
		if psub, ok := sub.(ParamNode); ok && match(psub.Name()) {
			nodes = append(nodes, sub)
		}
	}
	return nodes
}

// ParentNode is a node that has sub-nodes.
type ParentNode interface {
	Node
	Nodes() []Node
}

// ParamNode is a node that has ExprNode parameters.
type ParamNode interface {
	Node
	Name() string
	Parameters() []ExprNode
}

// segmentNode is any section- or statement-like parseNode that accepts parameters (values). This
// includes maps and arrays, as well, which also function as parser contexts.
type segmentNode interface {
	parseNode
	addExpr(ExprNode) error
}

// ExprNode is a Node that has a concrete value associated with itself, such as a string, bool,
// rational, or other parse-able value.
type ExprNode interface {
	Node
	Value() interface{}
}

// parseNode is any Node that can be used in parsing as a context.
type parseNode interface {
	// astparse is an empty function used to identify a parseNode.
	astparse()
}

// parentNode is any parseNode that accepts nodes as children during parsing.
type parentNode interface {
	parseNode
	addChild(node Node)
}

// Document is the root of a codf document -- it is functionally similar to a Section, but is
// unnamed and has no parameters.
type Document struct {
	Children []Node // Sections and Statements that make up the Document.
}

func (*Document) astnode() {}

func (d *Document) Nodes() []Node {
	return d.Children
}

// Token returns an empty token, as documents are the root node and only contain other nodes.
func (*Document) Token() Token {
	return noToken
}

func (d *Document) String() string {
	return d.format("")
}

func (d *Document) format(prefix string) string {
	strs := make([]string, len(d.Children))
	for i, n := range d.Children {
		strs[i] = n.format("")
	}
	return prefix + strings.Join(strs, "\n"+prefix)
}

// addChild adds a section or statement to the Document's children.
func (d *Document) addChild(node Node) {
	d.Children = append(d.Children, node)
}

func (*Document) astparse() {}

// Statement is any single word followed by an optional set of ExprNodes for parameters.
type Statement struct {
	NameTok *Literal
	Params  []ExprNode

	EndTok Token
}

func (s *Statement) Parameters() []ExprNode {
	return s.Params
}

func (s *Statement) String() string {
	return s.format("")
}

func (s *Statement) format(prefix string) string {
	pieces := make([]string, len(s.Params)+1)
	pieces[0] = s.Name()
	for i, p := range s.Params {
		pieces[i+1] = p.format(prefix)
	}
	return prefix + strings.Join(pieces, " ") + ";"
}

func (*Statement) astparse() {}

func (s *Statement) addExpr(node ExprNode) error {
	s.Params = append(s.Params, node)
	return nil
}

// Name returns the name of the Statement.
// For example, the statement "enable-gophers yes;" has the name "enable-gophers".
func (s *Statement) Name() string {
	str, _ := String(s.NameTok)
	return str
}

func (s *Statement) astnode() {}

// Token returns the first Token of the statement (its name token).
func (s *Statement) Token() Token {
	return s.NameTok.Token()
}

// promote is used in a parsing context to convert a statement to a section when a curly brace is
// encountered.
func (s *Statement) promote() *Section {
	return &Section{
		NameTok:  s.NameTok,
		Params:   s.Params,
		Children: []Node{},
	}
}

// Section is a single word follow by an optional set of ExprNodes for parameters.
// A Section may contain children Statements and Sections.
type Section struct {
	NameTok  *Literal
	Params   []ExprNode
	Children []Node

	StartTok Token
	EndTok   Token
}

func (s *Section) Nodes() []Node {
	return s.Children
}

func (s *Section) Parameters() []ExprNode {
	return s.Params
}

func (s *Section) String() string {
	return s.format("")
}

func (s *Section) format(prefix string) string {
	pieces := make([]string, len(s.Params)+2)
	pieces[0] = s.Name()
	for i, p := range s.Params {
		pieces[i+1] = p.format("")
	}
	pieces[len(pieces)-1] = "{"
	lead := prefix + strings.Join(pieces, " ")
	if len(s.Children) == 0 {
		return lead + "}"
	}
	inner := prefix + "\t"
	for _, ch := range s.Children {
		lead += "\n" + ch.format(inner)
	}

	return lead + "\n" + prefix + "}"
}

func (*Section) astparse() {}

// Name returns the name of the Section.
// For example, the section "proxy http { }" has the name "proxy".
func (s *Section) Name() string {
	str, _ := String(s.NameTok)
	return str
}

func (s *Section) astnode() {}

// Token returns the first token of the section (its name token).
func (s *Section) Token() Token {
	return s.NameTok.Token()
}

func (s *Section) addChild(node Node) {
	s.Children = append(s.Children, node)
}

// Map is an ExprNode for a '#{ key value }' map in a document.
type Map struct {
	StartTok Token
	EndTok   Token
	// Elems is a map of the string keys to their key-value pairs.
	Elems map[string]*MapEntry
}

func (m *Map) String() string {
	return m.format("")
}

func (m *Map) format(prefix string) string {
	pairs := m.Pairs()
	if len(pairs) == 0 {
		return "#{}"
	}
	pieces := make([]string, len(pairs))
	indent := prefix + "\t"
	for i, p := range pairs {
		pieces[i] = p.format(indent)
	}
	return "#{\n" + strings.Join(pieces, "\n") + "\n" + prefix + "}"
}

func (m *Map) astnode() {}

// Token returns the first Token of the map (its opening '#{' token).
func (m *Map) Token() Token {
	return m.StartTok
}

// Value returns the map's elements as its value.
// This is always a value of the type map[string]*MapEntry.
func (m *Map) Value() interface{} {
	return m.Elems
}

// Pairs returns the map's elements as a []*MapEntry.
// The returned slice ordered by each MapEntry's Ord field (i.e., the parsing order).
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

// Array is an ExprNode for a '[ value ]' array in a document.
type Array struct {
	StartTok Token
	EndTok   Token
	Elems    []ExprNode
}

func (a *Array) String() string {
	return a.format("")
}

func (a *Array) format(prefix string) string {
	if len(a.Elems) == 0 {
		return "[]"
	}
	pieces := make([]string, len(a.Elems))
	indent := prefix + "\t"
	for i, p := range a.Elems {
		pieces[i] = p.format(indent)
	}
	return "[" + strings.Join(pieces, " ") + "]"
}

func (*Array) astparse() {}

func (a *Array) addExpr(node ExprNode) error {
	a.Elems = append(a.Elems, node)
	return nil
}

func (a *Array) astnode() {}

// Token returns the first Token of the array (its opening bracket).
func (a *Array) Token() Token {
	return a.StartTok
}

// Value returns the elements of the array.
// This is always a value of the type []ExprNode.
func (a *Array) Value() interface{} {
	return a.Elems
}

// MapEntry is an entry in a codf map, containing the key, value, and an ord field -- an integer for
// determining the order of keys in the map as parsed. The order of keys in a map is unordered and
// information is only retained for writing tools.
type MapEntry struct {
	// Ord is an integer for ordering entries in the map.
	// There can be gaps in Ord for a range. Duplicate keys
	// increase Ord and replace the conflicting MapEntry.
	Ord uint
	// Key and Val are the key-value pair.
	Key ExprNode
	Val ExprNode
}

func (*MapEntry) astnode() {}

func (m *MapEntry) format(prefix string) string {
	return prefix + m.Key.format(prefix) + " " + m.Val.format(prefix)
}

func (m *MapEntry) String() string {
	return m.format("")
}

// Token returns the first token of the MapEntry's key-value pair (its key token).
func (m *MapEntry) Token() Token {
	return m.Key.Token()
}

// Name returns the MapEntry's key as a string.
func (m *MapEntry) Name() string {
	s, _ := String(m.Key)
	return s
}

// Value returns the MapEntry's value as a string.
// The entire AST is invalid if this returns nil.
func (m *MapEntry) Value() interface{} {
	return m.Val.Value()
}

// Literal is an ExprNode containing a value that is either a string, number (integer, float, or
// rational), regexp, duration, or boolean.
type Literal struct {
	Tok Token
}

func (l *Literal) format(prefix string) string {
	return string(l.Token().Raw)
}

func (l *Literal) astnode() {}

// Token returns the literal's corresponding Token.
func (l *Literal) Token() Token {
	return l.Tok
}

// Value returns the literal's value.
// Depending on the token, this can be a value of type string, boolean, *big.Int, *big.Float,
// *big.Rat, time.Duration, or *regexp.Regexp.
// The entire AST is invalid if this returns nil.
func (l *Literal) Value() interface{} {
	return l.Tok.Value
}

// Value returns the value of node.
// If node is an ExprNode, it will return that node's value.
// Otherwise, it will return any value associated with the node's token.
// It may be nil for nodes whose token is punctuation or an opening brace or bracket.
func Value(node Node) interface{} {
	switch node := node.(type) {
	case ExprNode:
		return node.Value()
	default:
		return node.Token().Value
	}
}

// Regexp returns the value held by node as a *regexp.Regexp.
// If the node doesn't hold a regexp, it returns nil.
func Regexp(node Node) (v *regexp.Regexp) {
	v, _ = Value(node).(*regexp.Regexp)
	return
}

// Duration returns the value held by node as a time.Duration and true.
// If the node doesn't hold a duration, it returns 0 and false.
func Duration(node Node) (v time.Duration, ok bool) {
	v, ok = Value(node).(time.Duration)
	return
}

// Bool returns the value held by node as a boolean and true.
// If the node doesn't hold a boolean, it returns false for both values (v and ok).
func Bool(node Node) (v, ok bool) {
	v, ok = Value(node).(bool)
	return
}

// String returns the value held by node as a string and true.
// If the node doesn't hold a string, it returns the empty string and false.
func String(node Node) (str string, ok bool) {
	str, ok = Value(node).(string)
	return
}

// BigRat returns the value held by node as a *big.Rat.
// If the node doesn't hold a rational, it returns nil.
func BigRat(node Node) (v *big.Rat) {
	v, _ = Value(node).(*big.Rat)
	return
}

// BigInt returns the value held by node as a *big.Int.
// If the node doesn't hold an integer, it returns nil.
func BigInt(node Node) (v *big.Int) {
	v, _ = Value(node).(*big.Int)
	return
}

// BigFloat returns the value held by node as a *big.Float.
// If the node doesn't hold a float, it returns nil.
func BigFloat(node Node) (v *big.Float) {
	v, _ = Value(node).(*big.Float)
	return
}

// Float64 returns the value held by node as a float64 and true.
// Integer and rational nodes are converted to floats.
// If the node doesn't hold a float, integer, or rational, it returns 0 and false.
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
	}
	return 0, false
}

// Int64 returns the value held by node as an int64 and true.
// Float and rational nodes are converted to floats.
// If the node is a rational and it is not an integer already, it is converted to a float and
// truncated to an integer.
// If the node doesn't hold an integer, float, or rational, it returns 0 and false.
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
	}
	return 0, false
}
