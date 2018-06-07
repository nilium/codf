package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"path/filepath"

	"github.com/kr/pretty"
	"go.spiff.io/codf"
)

func main() {
	log.SetFlags(log.Lshortfile)
	if len(os.Args) == 1 {
		load("stdin", os.Stdin)
	}

	for _, p := range os.Args[1:] {
		loadFile(p)
	}
}

func loadFile(path string) {
	fi, err := os.Open(path)
	if err != nil {
		log.Fatalf("error opening file: %v", err)
	}
	defer fi.Close()
	load(filepath.Base(path), fi)
}

func load(name string, file *os.File) {
	buf := bufio.NewReader(file)
	lexer := codf.NewLexer(buf)

	p := codf.NewParser()
	if err := p.Parse(lexer); err != nil {
		log.Print(err)
	}
	fmt.Fprintf(os.Stderr, "%# v\n------------------------------------------------------------------------\n",
		pretty.Formatter(p.Document()))
	os.Stderr.Sync()
	fmt.Printf("%s\n",
		p.Document())
	os.Stdout.Sync()
}
