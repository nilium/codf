package codf

import "fmt"

// Walker is used by Walk to consume statements and sections, recursively, in ParentNodes (sections
// and documents).
//
// Optionally, Walkers may also implement WalkExiter to see receive an ExitSection call when exiting
// a section.
type Walker interface {
	Statement(*Statement) error
	EnterSection(*Section) (Walker, error)
}

// WalkExiter is an optional interface implemented for a Walker to have Walk call ExitSection when
// it has finished consuming all children in a section.
type WalkExiter interface {
	Walker

	// ExitSection is called with the parent Walker, the exited node, and its parent node
	// (either the root document given to Walk or a section).
	ExitSection(Walker, *Section, ParentNode) error
}

// Walk walks a codf AST starting with but not including parent.
// It is assumed that by having the parent, it has already been walked.
//
// Walk will call walker.Statement for each statement encountered in parent and walker.EnterSection
// for each section (and walker.ExitSection if implemented).
//
// If walker.EnterSection returns a non-nil Walker, Walk will recursively call Walk with the section
// and the returned Walker.
//
// Walk will return a *WalkError if any error occurs during a walk. The WalkError will contain both
// the parent and child node that the error occurred for.
func Walk(parent ParentNode, walker Walker) (err error) {
	children := append([]Node(nil), parent.Nodes()...)
reset:
	for i := 0; i < len(children); i++ {
		child := children[i]
		switch child := child.(type) {
		case *Statement:
			// Statements are passed verbatim as directives
			err = walker.Statement(child)

		case *Section:
			// Sections are entered, walked, and exited -- the sub-Walker is given
			// a chance to interact with its parent when exiting the section, if it
			// implemented ConfigExiter.
			var sub Walker
			if sub, err = walker.EnterSection(child); err != nil || sub == nil {
				break
			}
			if err = Walk(child, sub); err != nil {
				break
			}
			err = exitWalker(sub, walker, child, parent)

		case *Document:
			// Merge a non-empty document's children with the current set of children,
			// discarding children already seen.
			docNodes := child.Nodes()
			if n := len(docNodes); n == 0 {
				continue
			} else if n < i+1 {
				children = append(append(children[:0], docNodes...), children[i+1:]...)
			} else {
				tail := children[i+1:]
				children = make([]Node, 0, len(tail)+n)
				children = append(append(children, docNodes...), tail...)
			}
			goto reset
		}

		if err != nil {
			return walkErr(parent, child, err)
		}
	}
	return nil
}

func exitWalker(walker Walker, next Walker, section *Section, parent ParentNode) error {
	ex, ok := walker.(WalkExiter)
	if !ok {
		return nil
	}
	return ex.ExitSection(next, section, parent)
}

// WalkError is an error returned by Walk if an error occurs during a Walk call.
type WalkError struct {
	// Context is the ParentNode that Node is a child of.
	Context ParentNode
	// Node is the node that was encountered when the error occurred.
	Node Node
	// Err is the error that a Walker returned.
	Err error
}

func walkErr(ctx ParentNode, node Node, err error) *WalkError {
	if we, ok := err.(*WalkError); ok {
		return we
	}
	return &WalkError{
		Context: ctx,
		Node:    node,
		Err:     err,
	}
}

func (e *WalkError) Error() string {
	return "[" + e.Node.Token().Start.String() + "] " +
		contextName(e.Node) + " in " + contextName(e.Context) + ": " + e.Err.Error()
}

func contextName(node Node) string {
	switch node := node.(type) {
	case *Document:
		return "main"
	case *Section:
		return node.Name()
	case *Statement:
		return node.Name()
	default:
		return fmt.Sprintf("<%v>", node)
	}
}
