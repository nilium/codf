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
	return walkInContext(parent, parent, walker)
}

func walkInContext(context, parent ParentNode, walker Walker) (err error) {
	for _, child := range parent.Nodes() {
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
			if err = walkInContext(child, child, sub); err != nil {
				break
			}
			if ex, ok := sub.(WalkExiter); ok {
				err = ex.ExitSection(walker, child, parent)
			}

		case *Document:
			err = walkInContext(context, child, walker)
		}
		if err != nil {
			return walkErr(parent, context, child, err)
		}
	}
	return nil
}

// WalkError is an error returned by Walk if an error occurs during a Walk call.
type WalkError struct {
	// Document is the document the context and node were found in, if the Walk root was
	// a document.
	Document *Document
	// Context is the ParentNode that Node is a child of.
	Context ParentNode
	// Node is the node that was encountered when the error occurred.
	Node Node
	// Err is the error that a Walker returned.
	Err error
}

func walkErr(owner ParentNode, ctx ParentNode, node Node, err error) *WalkError {
	if we, ok := err.(*WalkError); ok {
		if we.Document != nil {
		} else if doc, ok := owner.(*Document); ok {
			we.Document = doc
		}
		return we
	}
	doc, _ := owner.(*Document)
	return &WalkError{
		Document: doc,
		Context:  ctx,
		Node:     node,
		Err:      err,
	}
}

func (e *WalkError) Error() string {
	prefix := "[" + e.Node.Token().Start.String() + "] "
	suffix := contextName(e.Node) + " in " + contextName(e.Context) + ": " + e.Err.Error()
	if e.Document != nil && e.Document.Name != "" {
		return prefix + e.Document.Name + ": " + suffix
	}
	return prefix + suffix
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
