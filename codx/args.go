package codx

import (
	"encoding"
	"math"
	"math/big"
	"net/url"
	"reflect"
	"sort"
	"strconv"
	"time"

	"go.spiff.io/codf"
	"golang.org/x/xerrors"
)

func CheckNumArgs(argc, min, max int) error {
	if argc >= min && argc <= max {
		return nil
	}
	if min != max {
		return xerrors.Errorf("expected %d..%d arguments; got %d", min, max, argc)
	}
	return xerrors.Errorf("expected %d arguments; got %d", min, argc)
}

func CheckFixedArgs(argc int, lengths ...int) error {
	i := sort.SearchInts(lengths, argc)
	if n := len(lengths); i < n && lengths[i] == argc {
		return nil
	} else if i >= n {
		i--
	}
	return xerrors.Errorf("expected %d arguments; got %d", lengths[i], argc)
}

func ParseArgsTo(args []codf.ExprNode, dest ...interface{}) error {
	if len(args) > len(dest) {
		args = args[:len(dest)]
	}
	return ParseArgs(args, dest...)
}

func ParseArgs(args []codf.ExprNode, dest ...interface{}) error {
	if err := CheckNumArgs(len(args), len(dest), len(dest)); err != nil {
		return err
	}
	for i, p := range dest {
		if err := ParseArg(args[i], p); err != nil {
			return xerrors.Errorf("error parsing parameter %d: %v", i+1, err)
		}
	}
	return nil
}

func ParseArg(arg codf.ExprNode, dest interface{}) error {
	const maxUint = ^uint(0)
	const minUint = 0
	const maxInt = int(maxUint >> 1)
	const minInt = -maxInt - 1

	var expected string

	switch v := dest.(type) {
	case ExprValue:
		return v.Set(arg)

	case Value:
		return v.Set(arg)

	case *codf.Token:
		*v = arg.Token()

	case *codf.Location:
		*v = arg.Token().Start

	case *codf.Node:
		*v = arg

	case *codf.ExprNode:
		*v = arg

	case **big.Int:
		if bi := codf.BigInt(arg); bi != nil {
			*v = new(big.Int).Set(bi)
			return nil
		}
		expected = "bigint"

	case *big.Int:
		if bi := codf.BigInt(arg); bi == nil {
			v.Set(bi)
			return nil
		}
		expected = "bigint"

	case *bool:
		if b, ok := codf.Bool(arg); ok {
			*v = b
			return nil
		}
		expected = "boolean"

	case **bool:
		if b, ok := codf.Bool(arg); ok {
			*v = &b
			return nil
		}
		expected = "boolean"

	case *float64:
		if f, ok := codf.Float64(arg); ok {
			*v = f
			return nil
		}
		expected = "float"

	case **float64:
		if f, ok := codf.Float64(arg); ok {
			*v = &f
			return nil
		}
		expected = "float"

	case *float32:
		if f, ok := codf.Float64(arg); ok {
			*v = float32(f)
			return nil
		}
		expected = "float"

	case **float32:
		if f, ok := codf.Float64(arg); ok {
			q := float32(f)
			*v = &q
			return nil
		}
		expected = "float"

	case *int:
		if i, ok := codf.Int64(arg); ok {
			if maxInt != math.MaxInt64 && i > int64(maxInt) || i < int64(minInt) {
				return xerrors.Errorf("integer out of range: must be within %d..%d",
					minInt, maxInt)
			}
			*v = int(i)
			return nil
		}
		expected = "integer"

	case **int:
		if i, ok := codf.Int64(arg); ok {
			if maxInt != math.MaxInt64 && i > int64(maxInt) || i < int64(minInt) {
				return xerrors.Errorf("integer out of range: must be within %d..%d",
					minInt, maxInt)
			}
			q := int(i)
			*v = &q
			return nil
		}
		expected = "integer"

	case *int64:
		if i, ok := codf.Int64(arg); ok {
			*v = i
			return nil
		} else if bi := codf.BigInt(arg); bi != nil && !bi.IsInt64() {
			return xerrors.Errorf("integer out of range: must be within %d..%d",
				math.MinInt64, math.MaxInt64)
		}
		expected = "integer"

	case **int64:
		if i, ok := codf.Int64(arg); ok {
			*v = &i
			return nil
		} else if bi := codf.BigInt(arg); bi != nil && !bi.IsInt64() {
			return xerrors.Errorf("integer out of range: must be within %d..%d",
				math.MinInt64, math.MaxInt64)
		}
		expected = "integer"

	case *int32:
		if i, ok := codf.Int64(arg); ok {
			if i > math.MaxInt32 || i < math.MinInt32 {
				return xerrors.Errorf("integer out of range: must be within %d..%d",
					math.MinInt32, math.MaxInt32)
			}
			*v = int32(i)
			return nil
		}
		expected = "integer"

	case **int32:
		if i, ok := codf.Int64(arg); ok {
			if i > math.MaxInt32 || i < math.MinInt32 {
				return xerrors.Errorf("integer out of range: must be within %d..%d",
					math.MinInt32, math.MaxInt32)
			}
			q := int32(i)
			*v = &q
			return nil
		}
		expected = "integer"

	case *int16:
		if i, ok := codf.Int64(arg); ok {
			if i > math.MaxInt16 || i < math.MinInt16 {
				return xerrors.Errorf("integer out of range: must be within %d..%d",
					math.MinInt16, math.MaxInt16)
			}
			*v = int16(i)
			return nil
		}
		expected = "integer"

	case **int16:
		if i, ok := codf.Int64(arg); ok {
			if i > math.MaxInt16 || i < math.MinInt16 {
				return xerrors.Errorf("integer out of range: must be within %d..%d",
					math.MinInt16, math.MaxInt16)
			}
			q := int16(i)
			*v = &q
			return nil
		}
		expected = "integer"

	case *[]string:
		expected = "[string]"

		ary, ok := arg.(*codf.Array)
		if !ok {
			break
		}

		parsed := make([]string, len(ary.Elems))
		for i, cell := range ary.Elems {
			parsed[i], ok = codf.String(cell)
			if !ok {
				goto cannotParse
			}
		}

		*v = parsed
		return nil

	case *string:
		if s, ok := codf.String(arg); ok {
			*v = s
			return nil
		}
		expected = "string"

	case **string:
		if s, ok := codf.String(arg); ok {
			*v = &s
			return nil
		}
		expected = "string"

	case *url.URL:
		if s, ok := codf.String(arg); ok {
			u, err := url.Parse(s)
			if err != nil {
				return err
			}
			*v = *u
			return nil
		}
		expected = "URL"

	case **url.URL:
		if s, ok := codf.String(arg); ok {
			u, err := url.Parse(s)
			if err != nil {
				return err
			}
			*v = u
			return nil
		}
		expected = "URL"

	case *time.Duration:
		if d, ok := codf.Duration(arg); ok {
			*v = d
			return nil
		} else if d, ok := codf.Int64(arg); ok && d == 0 {
			*v = 0
			return nil
		} else if d, ok := codf.Float64(arg); ok && !(math.IsInf(d, 0) || math.IsNaN(d)) {
			*v = time.Duration(float64(time.Second) * d)
			return nil
		}
		expected = "duration"

	case **time.Duration:
		if d, ok := codf.Duration(arg); ok {
			*v = &d
			return nil
		} else if d, ok := codf.Int64(arg); ok && d == 0 {
			*v = new(time.Duration)
			return nil
		} else if d, ok := codf.Float64(arg); ok && !(math.IsInf(d, 0) || math.IsNaN(d)) {
			q := time.Duration(float64(time.Second) * d)
			*v = &q
			return nil
		}
		expected = "duration"

	case encoding.TextUnmarshaler:
		if s, ok := codf.String(arg); ok {
			return v.UnmarshalText([]byte(s))
		}
		expected = reflect.TypeOf(v).Name()

	case encoding.BinaryUnmarshaler:
		if s, ok := codf.String(arg); ok {
			return v.UnmarshalBinary([]byte(s))
		}
		expected = reflect.TypeOf(v).Name()

	default:
		return xerrors.Errorf("cannot parse argument of type %T", dest)
	}

cannotParse:
	return expectedErr(arg, expected)
}

func expectedErr(node codf.Node, kind string) error {
	return xerrors.Errorf("expected %s; got %s", kind, node.Token().Kind)
}

// Auxiliary types for performing more specific matches

// Keyword is an argument type used when the argument must match a specific keyword (a word token of
// a specific value).
type Keyword string

// Set implements ExprValue.
func (k Keyword) Set(v codf.ExprNode) error {
	s, _ := codf.Word(v)
	if s != string(k) {
		return expectedErr(v, "keyword "+strconv.Quote(string(k)))
	}
	return nil
}

// Word is an argument type used when the argument must be a word.
type Word string

// AsWord returns a string pointer as a Word pointer.
func AsWord(dest *string) *Word {
	return (*Word)(dest)
}

// Set implements ExprValue.
func (w *Word) Set(v codf.ExprNode) error {
	s, ok := codf.Word(v)
	if !ok {
		return expectedErr(v, "word")
	}
	*w = Word(s)
	return nil
}

// Quote is an argument type used when the argument must be a quoted string.
type Quote string

// AsQuote returns a string pointer as a Quote pointer.
func AsQuote(dest *string) *Quote {
	return (*Quote)(dest)
}

// Set implements ExprValue.
func (q *Quote) Set(v codf.ExprNode) error {
	s, ok := codf.Quote(v)
	if !ok {
		return expectedErr(v, "quoted string")
	}
	*q = Quote(s)
	return nil
}

// OnOff is an argument type used when the argument must be the word "on" or "off".
type OnOff bool

// AsOnOff returns a boolean pointer as an OnOff pointer.
func AsOnOff(dest *bool) *OnOff {
	return (*OnOff)(dest)
}

// Set implements ExprValue.
func (o *OnOff) Set(v codf.ExprNode) error {
	switch s, _ := codf.Word(v); s {
	case "on", "off":
		*o = s == "on"
		return nil
	default:
		return expectedErr(v, "`on` or `off`")
	}
}

// Value is any value that can be set by ParseArg.
type Value interface {
	Set(codf.Node) error
}

// ExprValue is any value that can be set by ParseArg, but accepts an ExprNode instead of a Node.
type ExprValue interface {
	Set(codf.ExprNode) error
}

// OneOf attempts to parse an argument as at least one of the items in its slice.
// If OneOf fails on all elements of its slice, it returns the first error it encountered.
type OneOf []interface{}

// Set implements ExprValue.
func (o OneOf) Set(arg codf.ExprNode) (err error) {
	for _, dest := range o {
		if perr := ParseArg(arg, dest); perr == nil {
			return nil
		} else if err == nil {
			err = perr
		}
	}
	return err
}

// AllOf attempts to parse an argument as all of the items in its slice.
// This can be useful for things like extracting both the token and the value of the token. For
// example:
//
//     var where codf.Location
//     var duration time.Duration
//     _ = codx.ParseArgs(stmt.Parameters(), codx.AllOf{&where, &duration})
//     if duration < 0 {
//         return fmt.Errorf("%v: invalid duration %v: must be >= 0s", where, duration)
//     }
//
// If any error occurs passing an AllOf's element to ParseArg, it returns that error immediately.
type AllOf []interface{}

// Set implements ExprValue.
func (a AllOf) Set(arg codf.ExprNode) error {
	for _, dest := range a {
		if err := ParseArg(arg, dest); err != nil {
			return err
		}
	}
	return nil
}
