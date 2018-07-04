package codf

import "testing"

func TestStringConversion(t *testing.T) {
	exprs := mkexprs(
		mkstr("quoted"),
		mkrawstr("raw quoted"),
		mkword("word-string"),
		mkdec("4.1234"),
		mkexpr(1234),
		mkrat(1, 2),
	)

	check := func(t *testing.T, convert func(Node) (string, bool), expected ...bool) {
		for i, wantOK := range expected {
			ex := exprs[i]
			got, ok := convert(ex)
			if ok != wantOK {
				t.Errorf("%d: converting %v returned %t; want %t", i, ex, ok, wantOK)
				continue
			} else if !ok {
				continue
			}

			want := ex.(*Literal).Value().(string)
			if got != want {
				t.Errorf("%d: converting %v returned %q; want %q", i, ex, got, want)
			}
		}
	}

	t.Run("String", func(t *testing.T) {
		check(t, String,
			true, true, true,
			false, false, false,
		)
	})

	t.Run("Quote", func(t *testing.T) {
		check(t, Quote,
			true, true, false,
			false, false, false,
		)
	})

	t.Run("Word", func(t *testing.T) {
		check(t, Word,
			false, false, true,
			false, false, false,
		)
	})
}

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
