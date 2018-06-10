// Package codf is used to lex and parse codf configuration files.
//
// Codf is similar in syntax to nginx configurations, albeit with differences
// (to keep parsing simple). Like its peers, codf structures configuration as
// a combination of sections and statements, both of which may accept values.
// For example, an HTTP server's configuration might look like the following (in
// keeping with the nginx references):
//
//     server go.spiff.io {
//         listen 0.0.0.0:80;
//         control unix:///var/run/httpd.sock;
//         proxy unix:///var/run/go-redirect.sock {
//             strip-x-headers yes;
//             log-access no;
//         }
//         ' keep caches in 64mb of memory
//         cache memory 64mb {
//              expire 10m 404;
//              expire 1h  301 302;
//              expire 5m  200;
//         }
//     }
//
// codf is intended to accept generic input that is then filtered
// by the program using it, so syntax is kept restrictive enough to avoid
// too-complex parsing rules.
//
package codf // import "go.spiff.io/codf"
