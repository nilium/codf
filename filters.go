package codf

import (
	"errors"
)

// Filterer inspect a node and determine if it should be kept in a result set.
// A Filterer should return true if the node should be kept, false otherwise.
type Filterer interface {
	Filter(node Node, parent ParentNode) bool
}

// accumWalker appends any node matching its Filterer to its nodes slice.
type accumWalker struct {
	nodes  []Node
	parent ParentNode
	Filterer
}

func (f *accumWalker) Statement(stmt *Statement) error {
	if f.Filter(stmt, f.parent) {
		f.nodes = append(f.nodes, stmt)
	}
	return nil
}

func (f *accumWalker) EnterSection(sect *Section) (Walker, error) {
	if f.Filter(sect, f.parent) {
		f.nodes = append(f.nodes, sect)
	}
	return nil, nil
}

// lastWalker is similar to accumWalker, but only collects the last node it saw that matched its
// Filterer. If stopAfterFirst is true, it returns errWalkStopped for either Statement or
// EnterSection after the Filterer returns true for the first time.
type lastWalker struct {
	last           Node
	stopAfterFirst bool
	parent         ParentNode
	Filterer
}

var errWalkStopped = errors.New("walk stopped")

func (f *lastWalker) Statement(stmt *Statement) error {
	if f.Filter(stmt, f.parent) {
		f.last = stmt
		if f.stopAfterFirst {
			return errWalkStopped
		}
	}
	return nil
}

func (f *lastWalker) EnterSection(sect *Section) (Walker, error) {
	if f.Filter(sect, f.parent) {
		f.last = sect
		if f.stopAfterFirst {
			return nil, errWalkStopped
		}
	}
	return nil, nil
}

func singleFilter(fs []Filterer) Filterer {
	if len(fs) == 0 {
		return filterAlways
	}

	f := fs[0]
	if len(fs) > 1 {
		f = FilterAnd(fs...)
	}
	return f
}

// Filter returns all children for which f.Filter(child, parent) returns true.
// It does not recurse into sections.
func Filter(parent ParentNode, fs ...Filterer) []Node {
	f := singleFilter(fs)
	walker := accumWalker{
		nodes:    make([]Node, 0, len(parent.Nodes())),
		parent:   parent,
		Filterer: f,
	}
	_ = Walk(parent, &walker)
	return walker.nodes
}

// First returns the first child of the parent node that matches all Filterers.
//
// If no Filterer is passed, it returns the first child of the parent.
func First(parent ParentNode, fs ...Filterer) Node {
	f := singleFilter(fs)
	walker := lastWalker{
		parent:         parent,
		stopAfterFirst: true,
		Filterer:       f,
	}
	_ = Walk(parent, &walker)
	return walker.last
}

// Last returns the last child of the parent node that matches all Filterers.
//
// If no Filterer is passed, it returns the last child of the parent.
func Last(parent ParentNode, fs ...Filterer) Node {
	f := singleFilter(fs)
	walker := lastWalker{
		parent:   parent,
		Filterer: f,
	}
	_ = Walk(parent, &walker)
	return walker.last
}

// FilterFunc is a general-purpose Filterer function.
type FilterFunc func(node Node, parent ParentNode) bool

// Filter implements Filterer. It calls the underlying function of the receiver.
func (f FilterFunc) Filter(node Node, parent ParentNode) bool {
	return f(node, parent)
}

var filterAlways FilterFunc = func(node Node, _ ParentNode) bool {
	return true
}

type onlyStatements int

// FilterStatements is a Filterer that selects only nodes that are of type *Statement.
const FilterStatements = onlyStatements(0)

func (onlyStatements) Filter(node Node, _ ParentNode) bool {
	_, ok := node.(*Statement)
	return ok
}

type onlySections int

// FilterSections is a Filterer that selects only nodes that are of type *Section.
const FilterSections = onlySections(0)

func (onlySections) Filter(node Node, _ ParentNode) bool {
	_, ok := node.(*Section)
	return ok
}

// FilterNamed is a Filterer that selects only nodes that have a name equal to itself.
type FilterNamed string

// Filter implements Filterer.
func (n FilterNamed) Filter(node Node, _ ParentNode) bool {
	type namer interface {
		Node
		Name() string
	}
	nm, ok := node.(namer)
	return ok && nm.Name() == string(n)
}

type filterAnd []Filterer

// FilterAnd creates a Filterer that returns the conjunction of multiple Filterers.
func FilterAnd(and ...Filterer) Filterer {
	return filterAnd(and)
}

func (c filterAnd) Filter(node Node, parent ParentNode) bool {
	for _, f := range c {
		if !f.Filter(node, parent) {
			return false
		}
	}
	return true
}

type filterOr []Filterer

// FilterOr creates a Filterer that returns the disjunction of multiple Filtereres.
func FilterOr(or ...Filterer) Filterer {
	return filterOr(or)
}

func (c filterOr) Filter(node Node, parent ParentNode) bool {
	for _, f := range c {
		if f.Filter(node, parent) {
			return true
		}
	}
	return false
}
