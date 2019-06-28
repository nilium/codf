package codx

import "go.spiff.io/codf"

type MapFunc func(codf.Node) (codf.Node, error)

type NodeMapper struct {
	w     codf.Walker
	mapfn MapFunc
}

func (m *NodeMapper) EnterSection(sect *codf.Section) (codf.Walker, error) {
	if m.w == nil {
		return m, nil
	}
	w, err := m.w.EnterSection(sect)
	if err == nil && w != nil {
		w = MapNodes(w, m.mapfn)
	}
	return w, err
}

func (m *NodeMapper) ExitSection(next codf.Walker, sect *codf.Section, parent codf.ParentNode) error {
	if w, ok := m.w.(codf.WalkExiter); ok {
		return w.ExitSection(next, sect, parent)
	}
	return nil
}

func (m *NodeMapper) Statement(stmt *codf.Statement) error {
	if m.w == nil {
		return nil
	}
	return m.w.Statement(stmt)
}

func (m *NodeMapper) Map(node codf.Node) (codf.Node, error) {
	return m.mapfn(node)
}

func MapNodes(walker codf.Walker, mapper MapFunc) *NodeMapper {
	return &NodeMapper{
		w:     walker,
		mapfn: mapper,
	}
}

func ChainMap(fns ...MapFunc) MapFunc {
	return func(node codf.Node) (codf.Node, error) {
		var err error
		for _, fn := range fns {
			if node, err = fn(node); err != nil {
				return nil, err
			}
		}
		return node, nil
	}
}
