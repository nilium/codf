package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"

	"go.spiff.io/codf"
)

func main() {
	if len(os.Args) == 1 {
		load("stdin", os.Stdin)
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
	buffer, err := ioutil.ReadAll(file)
	if err != nil {
		log.Fatal(err)
	}

	p := &codf.Parser{Buffer: string(buffer), Pretty: true}
	p.Init()

	if err := p.Parse(); err != nil {
		log.Print(err)
		return
	}

	p.PrintSyntaxTree()
	if err := p.Run(); err != nil {
		log.Fatal("parse error: ", err)
	}

	fmt.Println(p.Root())
}
