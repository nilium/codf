package codf

import "testing"

// logf is a pointer to the current test's Logf function.
// Only used for debugging.
var logf = func(string, ...interface{}) {}

func setlogf(t *testing.T) func() {
	temp := logf
	logf = t.Logf
	return func() { logf = temp }
}
