package codf

import "testing"

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
