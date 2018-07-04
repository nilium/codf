package codf

import (
	"reflect"
	"testing"
)

func TestFloat64Conversion(t *testing.T) {
	wants := mkexprs(
		mkdec("0.25"),
		mkdec("-4"),
		mkdec("4"),
		mkdec("4.1234"),
	)

	doc := mustParse(t, "input 1/4 -24/6 4 4.1234;")
	params := doc.Children[0].(*Statement).Params

	if got, want := len(params), len(wants); got != want {
		t.Fatalf("len(params) = %d; want %d", got, want)
	}

	for i, got := range params {
		want := wants[i]

		gotf, ok := Float64(got)
		if !ok {
			t.Fatalf("couldn't convert got=%q to float64: ", got)
		}

		wantf, ok := Float64(want)
		if !ok {
			t.Fatalf("couldn't convert want=%q to float64: ", want)
		}

		if gotf != wantf {
			t.Fatalf("got = %f (%#f); want %f (%#f)", gotf, gotf, wantf, wantf)
		}
	}
}

func TestInt64Conversion(t *testing.T) {
	wants := []int64{
		0,
		-4,
		4,
		4,
	}

	doc := mustParse(t, "input 1/4 -24/6 4 4.1234;")
	params := doc.Children[0].(*Statement).Params

	if got, want := len(params), len(wants); got != want {
		t.Fatalf("len(params) = %d; want %d", got, want)
	}

	for i, got := range params {
		want := wants[i]

		goti, ok := Int64(got)
		if !ok {
			t.Fatalf("couldn't convert got=%q to int64: ", got)
		}

		if goti != want {
			t.Fatalf("got = %d (%016x); want %d (%016x)", goti, goti, want, want)
		}
	}
}

func TestEach(t *testing.T) {
	doc := mustParse(t, `
	foo 1;
	foo 1 2;
	foo 1 2 3;
	`)

	Each(doc, func(i int, n Node) error {
		if doc.Children[i] != n {
			t.Fatalf("unexpected node for index %d = %#+v; want child[%d] = %#+v", i, n, i, doc.Children[i])
		}
		return nil
	})

	foo2 := doc.Children[1].(*Statement)
	Each(foo2, func(i int, n Node) error {
		if foo2.Params[i] != n {
			t.Fatalf("unexpected node for index %d = %#+v; want param[%d] = %#+v", i, n, i, foo2.Params[i])
		}
		return nil
	})
}

func TestSelect(t *testing.T) {
	doc := mustParse(t, `
	foo 9;
	bar 1;
	foo 8 7;
	bar 2 3;
	foo 6 5 4;
	bar 4 5 6;
	`)

	// Select half
	want := []Node{
		doc.Children[0],
		doc.Children[2],
		doc.Children[4],
	}
	got := Select(doc, "foo")

	if !reflect.DeepEqual(want, got) {
		t.Errorf("Select(foo) = %#+v; want %#+v", got, want)
	}

	// Select all
	want = doc.Children
	got = Select(doc, "foo", "bar")

	if !reflect.DeepEqual(want, got) {
		t.Errorf("Select(foo) = %#+v; want %#+v", got, want)
	}
}
