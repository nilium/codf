package codf

//go:generate peg codf.peg

import (
	"fmt"
	"math"
	"math/big"
	"sort"
	"strconv"
)

const endSymbol rune = 1114112

/* The rule types inferred from the grammar are below. */
type pegRule uint8

const (
	ruleUnknown pegRule = iota
	ruleRoot
	ruleBody
	ruleDecl
	ruleEmpty
	ruleStatement
	ruleSection
	rulesectionOpen
	rulesectionClose
	ruleName
	ruleExpr
	ruleLiteral
	ruleArray
	rulearrayBody
	rulearrayOpen
	rulearrayClose
	ruleMap
	rulemapOpen
	rulemapClose
	rulemapBody
	rulemapExpr
	rulemapKey
	rulemapValue
	ruleSymbol
	rulebareSymbol
	rulebareSymbolTail
	ruleRegexp
	ruleregexpBody
	ruleregexpTail
	ruleregexpEscape
	ruleString
	rulequotedString
	rulestringBody
	ruledoubleQuoteEscape
	rulestringTail
	rulestringEscape
	rulestringOct
	rulestringHex
	rulestringUni16
	rulestringUni32
	ruleNumber
	ruleoctDigit
	rulehexNibble
	rulehexOctet
	ruleRational
	ruleDecimal
	rulenormDec
	ruleexpDec
	ruleexponent
	ruleInteger
	ruledecInt
	ruleDecInt
	ruleposDecNum
	ruleBaseInt
	rulebaseDec
	rulebaseLit
	ruleHexLit
	ruleOctLit
	ruleBinLit
	rulesign
	ruleminus
	ruleplus
	ruleBoolean
	ruleTrue
	ruleFalse
	ruleNil
	ruleterm
	rulesep
	rulews
	rulecomment
	ruleAction0
	rulePegText
	ruleAction1
	ruleAction2
	ruleAction3
	ruleAction4
	ruleAction5
	ruleAction6
	ruleAction7
	ruleAction8
	ruleAction9
	ruleAction10
	ruleAction11
	ruleAction12
	ruleAction13
	ruleAction14
	ruleAction15
	ruleAction16
	ruleAction17
	ruleAction18
	ruleAction19
	ruleAction20
	ruleAction21
	ruleAction22
	ruleAction23
	ruleAction24
	ruleAction25
	ruleAction26
	ruleAction27
	ruleAction28
	ruleAction29
	ruleAction30
)

var rul3s = [...]string{
	"Unknown",
	"Root",
	"Body",
	"Decl",
	"Empty",
	"Statement",
	"Section",
	"sectionOpen",
	"sectionClose",
	"Name",
	"Expr",
	"Literal",
	"Array",
	"arrayBody",
	"arrayOpen",
	"arrayClose",
	"Map",
	"mapOpen",
	"mapClose",
	"mapBody",
	"mapExpr",
	"mapKey",
	"mapValue",
	"Symbol",
	"bareSymbol",
	"bareSymbolTail",
	"Regexp",
	"regexpBody",
	"regexpTail",
	"regexpEscape",
	"String",
	"quotedString",
	"stringBody",
	"doubleQuoteEscape",
	"stringTail",
	"stringEscape",
	"stringOct",
	"stringHex",
	"stringUni16",
	"stringUni32",
	"Number",
	"octDigit",
	"hexNibble",
	"hexOctet",
	"Rational",
	"Decimal",
	"normDec",
	"expDec",
	"exponent",
	"Integer",
	"decInt",
	"DecInt",
	"posDecNum",
	"BaseInt",
	"baseDec",
	"baseLit",
	"HexLit",
	"OctLit",
	"BinLit",
	"sign",
	"minus",
	"plus",
	"Boolean",
	"True",
	"False",
	"Nil",
	"term",
	"sep",
	"ws",
	"comment",
	"Action0",
	"PegText",
	"Action1",
	"Action2",
	"Action3",
	"Action4",
	"Action5",
	"Action6",
	"Action7",
	"Action8",
	"Action9",
	"Action10",
	"Action11",
	"Action12",
	"Action13",
	"Action14",
	"Action15",
	"Action16",
	"Action17",
	"Action18",
	"Action19",
	"Action20",
	"Action21",
	"Action22",
	"Action23",
	"Action24",
	"Action25",
	"Action26",
	"Action27",
	"Action28",
	"Action29",
	"Action30",
}

type token32 struct {
	pegRule
	begin, end uint32
}

func (t *token32) String() string {
	return fmt.Sprintf("\x1B[34m%v\x1B[m %v %v", rul3s[t.pegRule], t.begin, t.end)
}

type node32 struct {
	token32
	up, next *node32
}

func (node *node32) print(pretty bool, buffer string) {
	var print func(node *node32, depth int)
	print = func(node *node32, depth int) {
		for node != nil {
			for c := 0; c < depth; c++ {
				fmt.Printf(" ")
			}
			rule := rul3s[node.pegRule]
			quote := strconv.Quote(string(([]rune(buffer)[node.begin:node.end])))
			if !pretty {
				fmt.Printf("%v %v\n", rule, quote)
			} else {
				fmt.Printf("\x1B[34m%v\x1B[m %v\n", rule, quote)
			}
			if node.up != nil {
				print(node.up, depth+1)
			}
			node = node.next
		}
	}
	print(node, 0)
}

func (node *node32) Print(buffer string) {
	node.print(false, buffer)
}

func (node *node32) PrettyPrint(buffer string) {
	node.print(true, buffer)
}

type tokens32 struct {
	tree []token32
}

func (t *tokens32) Trim(length uint32) {
	t.tree = t.tree[:length]
}

func (t *tokens32) Print() {
	for _, token := range t.tree {
		fmt.Println(token.String())
	}
}

func (t *tokens32) AST() *node32 {
	type element struct {
		node *node32
		down *element
	}
	tokens := t.Tokens()
	var stack *element
	for _, token := range tokens {
		if token.begin == token.end {
			continue
		}
		node := &node32{token32: token}
		for stack != nil && stack.node.begin >= token.begin && stack.node.end <= token.end {
			stack.node.next = node.up
			node.up = stack.node
			stack = stack.down
		}
		stack = &element{node: node, down: stack}
	}
	if stack != nil {
		return stack.node
	}
	return nil
}

func (t *tokens32) PrintSyntaxTree(buffer string) {
	t.AST().Print(buffer)
}

func (t *tokens32) PrettyPrintSyntaxTree(buffer string) {
	t.AST().PrettyPrint(buffer)
}

func (t *tokens32) Add(rule pegRule, begin, end, index uint32) {
	if tree := t.tree; int(index) >= len(tree) {
		expanded := make([]token32, 2*len(tree))
		copy(expanded, tree)
		t.tree = expanded
	}
	t.tree[index] = token32{
		pegRule: rule,
		begin:   begin,
		end:     end,
	}
}

func (t *tokens32) Tokens() []token32 {
	return t.tree
}

type Parser struct {
	err error

	// stages []stage
	root *Root

	sign        int
	consumers   []consumer
	baseIntBase int

	Buffer string
	buffer []rune
	rules  [102]func() bool
	parse  func(rule ...int) error
	reset  func()
	Pretty bool
	tokens32
}

func (p *Parser) Parse(rule ...int) error {
	return p.parse(rule...)
}

func (p *Parser) Reset() {
	p.reset()
}

type textPosition struct {
	line, symbol int
}

type textPositionMap map[int]textPosition

func translatePositions(buffer []rune, positions []int) textPositionMap {
	length, translations, j, line, symbol := len(positions), make(textPositionMap, len(positions)), 0, 1, 0
	sort.Ints(positions)

search:
	for i, c := range buffer {
		if c == '\n' {
			line, symbol = line+1, 0
		} else {
			symbol++
		}
		if i == positions[j] {
			translations[positions[j]] = textPosition{line, symbol}
			for j++; j < length; j++ {
				if i != positions[j] {
					continue search
				}
			}
			break search
		}
	}

	return translations
}

type parseError struct {
	p   *Parser
	max token32
}

func (e *parseError) Error() string {
	tokens, error := []token32{e.max}, "\n"
	positions, p := make([]int, 2*len(tokens)), 0
	for _, token := range tokens {
		positions[p], p = int(token.begin), p+1
		positions[p], p = int(token.end), p+1
	}
	translations := translatePositions(e.p.buffer, positions)
	format := "parse error near %v (line %v symbol %v - line %v symbol %v):\n%v\n"
	if e.p.Pretty {
		format = "parse error near \x1B[34m%v\x1B[m (line %v symbol %v - line %v symbol %v):\n%v\n"
	}
	for _, token := range tokens {
		begin, end := int(token.begin), int(token.end)
		error += fmt.Sprintf(format,
			rul3s[token.pegRule],
			translations[begin].line, translations[begin].symbol,
			translations[end].line, translations[end].symbol,
			strconv.Quote(string(e.p.buffer[begin:end])))
	}

	return error
}

func (p *Parser) PrintSyntaxTree() {
	if p.Pretty {
		p.tokens32.PrettyPrintSyntaxTree(p.Buffer)
	} else {
		p.tokens32.PrintSyntaxTree(p.Buffer)
	}
}

func (p *Parser) Execute() {
	buffer, _buffer, text, begin, end := p.Buffer, p.buffer, "", 0, 0
	for _, token := range p.Tokens() {
		switch token.pegRule {

		case rulePegText:
			begin, end = int(token.begin), int(token.end)
			text = string(_buffer[begin:end])

		case ruleAction0:

			if p.root == nil {
				p.root = &Root{}
			}
			p.consumers = []consumer{p.root}

		case ruleAction1:
			p.beginStatement(text)
		case ruleAction2:
			p.closeStatement()
		case ruleAction3:
			p.beginSection(text)
		case ruleAction4:
			p.closeSection()
		case ruleAction5:
			p.beginArray()
		case ruleAction6:
			p.closeArray()
		case ruleAction7:
			p.beginMap()
		case ruleAction8:
			p.closeMap()
		case ruleAction9:
			p.consume(Symbol(text))
		case ruleAction10:
			p.consume(Symbol(unquote(typeSymbol, text)))
		case ruleAction11:
			p.beginRegexp()
		case ruleAction12:
			p.closeRegexp()
		case ruleAction13:
			p.tip().(*regexpBuilder).add(text)
		case ruleAction14:
			p.tip().(*regexpBuilder).add("/")
		case ruleAction15:
			p.consume(String(unquote(typeString, text)))
		case ruleAction16:
			p.sign = 1
		case ruleAction17:

			var x big.Rat
			if _, ok := x.SetString(signedText(text, p.sign)); !ok {
				p.err = fmt.Errorf("malformed rational: %q", text)
				return
			}
			p.consume(Rational{&x})

		case ruleAction18:

			var x big.Float
			if _, ok := x.SetString(signedText(text, p.sign)); !ok {
				p.err = fmt.Errorf("malformed decimal: %q", text)
				return
			}
			p.consume(Decimal{&x})

		case ruleAction19:

			var x big.Float
			if _, ok := x.SetString(signedText(text, p.sign)); !ok {
				p.err = fmt.Errorf("malformed decimal: %q", text)
				return
			}
			p.consume(Decimal{&x})

		case ruleAction20:

			var x big.Int
			if _, ok := x.SetString(signedText(text, p.sign), 10); !ok {
				p.err = fmt.Errorf("malformed integer: %q", text)
				return
			}
			p.consume(Integer{&x})

		case ruleAction21:

			if p.baseIntBase, p.err = strconv.Atoi(text); p.err != nil {
				p.err = fmt.Errorf("invalid integer base %q: %v", text, p.err)
				return
			}

		case ruleAction22:

			var x big.Int
			if _, ok := x.SetString(signedText(text, p.sign), p.baseIntBase); !ok {
				p.err = fmt.Errorf("malformed base-%d integer %q", p.baseIntBase, text)
				return
			}

			p.consume(Integer{&x})

		case ruleAction23:

			var x big.Int
			if _, ok := x.SetString(signedText(text, p.sign), 16); !ok {
				p.err = fmt.Errorf("malformed integer: %q", text)
				return
			}
			p.consume(Integer{&x})

		case ruleAction24:

			var x big.Int
			if _, ok := x.SetString(signedText(text, p.sign), 8); !ok {
				p.err = fmt.Errorf("malformed integer: %q", text)
				return
			}
			p.consume(Integer{&x})

		case ruleAction25:

			var x big.Int
			if _, ok := x.SetString(signedText(text, p.sign), 2); !ok {
				p.err = fmt.Errorf("malformed integer: %q", text)
				return
			}
			p.consume(Integer{&x})

		case ruleAction26:
			p.sign *= -1
		case ruleAction27:
			p.sign = 1
		case ruleAction28:
			p.consume(Bool(true))
		case ruleAction29:
			p.consume(Bool(false))
		case ruleAction30:
			p.consume(Nil)

		}
	}
	_, _, _, _, _ = buffer, _buffer, text, begin, end
}

func (p *Parser) Init() {
	var (
		max                  token32
		position, tokenIndex uint32
		buffer               []rune
	)
	p.reset = func() {
		max = token32{}
		position, tokenIndex = 0, 0

		p.buffer = []rune(p.Buffer)
		if len(p.buffer) == 0 || p.buffer[len(p.buffer)-1] != endSymbol {
			p.buffer = append(p.buffer, endSymbol)
		}
		buffer = p.buffer
	}
	p.reset()

	_rules := p.rules
	tree := tokens32{tree: make([]token32, math.MaxInt16)}
	p.parse = func(rule ...int) error {
		r := 1
		if len(rule) > 0 {
			r = rule[0]
		}
		matches := p.rules[r]()
		p.tokens32 = tree
		if matches {
			p.Trim(tokenIndex)
			return nil
		}
		return &parseError{p, max}
	}

	add := func(rule pegRule, begin uint32) {
		tree.Add(rule, begin, position, tokenIndex)
		tokenIndex++
		if begin != position && position > max.end {
			max = token32{rule, begin, position}
		}
	}

	matchDot := func() bool {
		if buffer[position] != endSymbol {
			position++
			return true
		}
		return false
	}

	/*matchChar := func(c byte) bool {
		if buffer[position] == c {
			position++
			return true
		}
		return false
	}*/

	/*matchRange := func(lower byte, upper byte) bool {
		if c := buffer[position]; c >= lower && c <= upper {
			position++
			return true
		}
		return false
	}*/

	_rules = [...]func() bool{
		nil,
		/* 0 Root <- <(Action0 ws Body ws !.)> */
		func() bool {
			position0, tokenIndex0 := position, tokenIndex
			{
				position1 := position
				if !_rules[ruleAction0]() {
					goto l0
				}
				if !_rules[rulews]() {
					goto l0
				}
				if !_rules[ruleBody]() {
					goto l0
				}
				if !_rules[rulews]() {
					goto l0
				}
				{
					position2, tokenIndex2 := position, tokenIndex
					if !matchDot() {
						goto l2
					}
					goto l0
				l2:
					position, tokenIndex = position2, tokenIndex2
				}
				add(ruleRoot, position1)
			}
			return true
		l0:
			position, tokenIndex = position0, tokenIndex0
			return false
		},
		/* 1 Body <- <(ws Decl)*> */
		func() bool {
			{
				position4 := position
			l5:
				{
					position6, tokenIndex6 := position, tokenIndex
					if !_rules[rulews]() {
						goto l6
					}
					if !_rules[ruleDecl]() {
						goto l6
					}
					goto l5
				l6:
					position, tokenIndex = position6, tokenIndex6
				}
				add(ruleBody, position4)
			}
			return true
		},
		/* 2 Decl <- <(Section / Statement / Empty)> */
		func() bool {
			position7, tokenIndex7 := position, tokenIndex
			{
				position8 := position
				{
					position9, tokenIndex9 := position, tokenIndex
					if !_rules[ruleSection]() {
						goto l10
					}
					goto l9
				l10:
					position, tokenIndex = position9, tokenIndex9
					if !_rules[ruleStatement]() {
						goto l11
					}
					goto l9
				l11:
					position, tokenIndex = position9, tokenIndex9
					if !_rules[ruleEmpty]() {
						goto l7
					}
				}
			l9:
				add(ruleDecl, position8)
			}
			return true
		l7:
			position, tokenIndex = position7, tokenIndex7
			return false
		},
		/* 3 Empty <- <term> */
		func() bool {
			position12, tokenIndex12 := position, tokenIndex
			{
				position13 := position
				if !_rules[ruleterm]() {
					goto l12
				}
				add(ruleEmpty, position13)
			}
			return true
		l12:
			position, tokenIndex = position12, tokenIndex12
			return false
		},
		/* 4 Statement <- <(<Name> Action1 (sep Expr)* ws term Action2)> */
		func() bool {
			position14, tokenIndex14 := position, tokenIndex
			{
				position15 := position
				{
					position16 := position
					if !_rules[ruleName]() {
						goto l14
					}
					add(rulePegText, position16)
				}
				if !_rules[ruleAction1]() {
					goto l14
				}
			l17:
				{
					position18, tokenIndex18 := position, tokenIndex
					if !_rules[rulesep]() {
						goto l18
					}
					if !_rules[ruleExpr]() {
						goto l18
					}
					goto l17
				l18:
					position, tokenIndex = position18, tokenIndex18
				}
				if !_rules[rulews]() {
					goto l14
				}
				if !_rules[ruleterm]() {
					goto l14
				}
				if !_rules[ruleAction2]() {
					goto l14
				}
				add(ruleStatement, position15)
			}
			return true
		l14:
			position, tokenIndex = position14, tokenIndex14
			return false
		},
		/* 5 Section <- <(<Name> Action3 (sep Expr)* ws sectionOpen (ws Body)? ws sectionClose Action4)> */
		func() bool {
			position19, tokenIndex19 := position, tokenIndex
			{
				position20 := position
				{
					position21 := position
					if !_rules[ruleName]() {
						goto l19
					}
					add(rulePegText, position21)
				}
				if !_rules[ruleAction3]() {
					goto l19
				}
			l22:
				{
					position23, tokenIndex23 := position, tokenIndex
					if !_rules[rulesep]() {
						goto l23
					}
					if !_rules[ruleExpr]() {
						goto l23
					}
					goto l22
				l23:
					position, tokenIndex = position23, tokenIndex23
				}
				if !_rules[rulews]() {
					goto l19
				}
				if !_rules[rulesectionOpen]() {
					goto l19
				}
				{
					position24, tokenIndex24 := position, tokenIndex
					if !_rules[rulews]() {
						goto l24
					}
					if !_rules[ruleBody]() {
						goto l24
					}
					goto l25
				l24:
					position, tokenIndex = position24, tokenIndex24
				}
			l25:
				if !_rules[rulews]() {
					goto l19
				}
				if !_rules[rulesectionClose]() {
					goto l19
				}
				if !_rules[ruleAction4]() {
					goto l19
				}
				add(ruleSection, position20)
			}
			return true
		l19:
			position, tokenIndex = position19, tokenIndex19
			return false
		},
		/* 6 sectionOpen <- <'{'> */
		func() bool {
			position26, tokenIndex26 := position, tokenIndex
			{
				position27 := position
				if buffer[position] != rune('{') {
					goto l26
				}
				position++
				add(rulesectionOpen, position27)
			}
			return true
		l26:
			position, tokenIndex = position26, tokenIndex26
			return false
		},
		/* 7 sectionClose <- <'}'> */
		func() bool {
			position28, tokenIndex28 := position, tokenIndex
			{
				position29 := position
				if buffer[position] != rune('}') {
					goto l28
				}
				position++
				add(rulesectionClose, position29)
			}
			return true
		l28:
			position, tokenIndex = position28, tokenIndex28
			return false
		},
		/* 8 Name <- <bareSymbol> */
		func() bool {
			position30, tokenIndex30 := position, tokenIndex
			{
				position31 := position
				if !_rules[rulebareSymbol]() {
					goto l30
				}
				add(ruleName, position31)
			}
			return true
		l30:
			position, tokenIndex = position30, tokenIndex30
			return false
		},
		/* 9 Expr <- <Literal> */
		func() bool {
			position32, tokenIndex32 := position, tokenIndex
			{
				position33 := position
				if !_rules[ruleLiteral]() {
					goto l32
				}
				add(ruleExpr, position33)
			}
			return true
		l32:
			position, tokenIndex = position32, tokenIndex32
			return false
		},
		/* 10 Literal <- <(Number / Boolean / Nil / Regexp / String / Symbol / Map / Array)> */
		func() bool {
			position34, tokenIndex34 := position, tokenIndex
			{
				position35 := position
				{
					position36, tokenIndex36 := position, tokenIndex
					if !_rules[ruleNumber]() {
						goto l37
					}
					goto l36
				l37:
					position, tokenIndex = position36, tokenIndex36
					if !_rules[ruleBoolean]() {
						goto l38
					}
					goto l36
				l38:
					position, tokenIndex = position36, tokenIndex36
					if !_rules[ruleNil]() {
						goto l39
					}
					goto l36
				l39:
					position, tokenIndex = position36, tokenIndex36
					if !_rules[ruleRegexp]() {
						goto l40
					}
					goto l36
				l40:
					position, tokenIndex = position36, tokenIndex36
					if !_rules[ruleString]() {
						goto l41
					}
					goto l36
				l41:
					position, tokenIndex = position36, tokenIndex36
					if !_rules[ruleSymbol]() {
						goto l42
					}
					goto l36
				l42:
					position, tokenIndex = position36, tokenIndex36
					if !_rules[ruleMap]() {
						goto l43
					}
					goto l36
				l43:
					position, tokenIndex = position36, tokenIndex36
					if !_rules[ruleArray]() {
						goto l34
					}
				}
			l36:
				add(ruleLiteral, position35)
			}
			return true
		l34:
			position, tokenIndex = position34, tokenIndex34
			return false
		},
		/* 11 Array <- <(arrayOpen Action5 (ws arrayBody)? ws arrayClose Action6)> */
		func() bool {
			position44, tokenIndex44 := position, tokenIndex
			{
				position45 := position
				if !_rules[rulearrayOpen]() {
					goto l44
				}
				if !_rules[ruleAction5]() {
					goto l44
				}
				{
					position46, tokenIndex46 := position, tokenIndex
					if !_rules[rulews]() {
						goto l46
					}
					if !_rules[rulearrayBody]() {
						goto l46
					}
					goto l47
				l46:
					position, tokenIndex = position46, tokenIndex46
				}
			l47:
				if !_rules[rulews]() {
					goto l44
				}
				if !_rules[rulearrayClose]() {
					goto l44
				}
				if !_rules[ruleAction6]() {
					goto l44
				}
				add(ruleArray, position45)
			}
			return true
		l44:
			position, tokenIndex = position44, tokenIndex44
			return false
		},
		/* 12 arrayBody <- <(Expr (sep Expr)*)> */
		func() bool {
			position48, tokenIndex48 := position, tokenIndex
			{
				position49 := position
				if !_rules[ruleExpr]() {
					goto l48
				}
			l50:
				{
					position51, tokenIndex51 := position, tokenIndex
					if !_rules[rulesep]() {
						goto l51
					}
					if !_rules[ruleExpr]() {
						goto l51
					}
					goto l50
				l51:
					position, tokenIndex = position51, tokenIndex51
				}
				add(rulearrayBody, position49)
			}
			return true
		l48:
			position, tokenIndex = position48, tokenIndex48
			return false
		},
		/* 13 arrayOpen <- <'['> */
		func() bool {
			position52, tokenIndex52 := position, tokenIndex
			{
				position53 := position
				if buffer[position] != rune('[') {
					goto l52
				}
				position++
				add(rulearrayOpen, position53)
			}
			return true
		l52:
			position, tokenIndex = position52, tokenIndex52
			return false
		},
		/* 14 arrayClose <- <']'> */
		func() bool {
			position54, tokenIndex54 := position, tokenIndex
			{
				position55 := position
				if buffer[position] != rune(']') {
					goto l54
				}
				position++
				add(rulearrayClose, position55)
			}
			return true
		l54:
			position, tokenIndex = position54, tokenIndex54
			return false
		},
		/* 15 Map <- <(mapOpen Action7 (ws mapBody)? ws mapClose Action8)> */
		func() bool {
			position56, tokenIndex56 := position, tokenIndex
			{
				position57 := position
				if !_rules[rulemapOpen]() {
					goto l56
				}
				if !_rules[ruleAction7]() {
					goto l56
				}
				{
					position58, tokenIndex58 := position, tokenIndex
					if !_rules[rulews]() {
						goto l58
					}
					if !_rules[rulemapBody]() {
						goto l58
					}
					goto l59
				l58:
					position, tokenIndex = position58, tokenIndex58
				}
			l59:
				if !_rules[rulews]() {
					goto l56
				}
				if !_rules[rulemapClose]() {
					goto l56
				}
				if !_rules[ruleAction8]() {
					goto l56
				}
				add(ruleMap, position57)
			}
			return true
		l56:
			position, tokenIndex = position56, tokenIndex56
			return false
		},
		/* 16 mapOpen <- <(':' '{')> */
		func() bool {
			position60, tokenIndex60 := position, tokenIndex
			{
				position61 := position
				if buffer[position] != rune(':') {
					goto l60
				}
				position++
				if buffer[position] != rune('{') {
					goto l60
				}
				position++
				add(rulemapOpen, position61)
			}
			return true
		l60:
			position, tokenIndex = position60, tokenIndex60
			return false
		},
		/* 17 mapClose <- <'}'> */
		func() bool {
			position62, tokenIndex62 := position, tokenIndex
			{
				position63 := position
				if buffer[position] != rune('}') {
					goto l62
				}
				position++
				add(rulemapClose, position63)
			}
			return true
		l62:
			position, tokenIndex = position62, tokenIndex62
			return false
		},
		/* 18 mapBody <- <(mapExpr (sep mapExpr)*)> */
		func() bool {
			position64, tokenIndex64 := position, tokenIndex
			{
				position65 := position
				if !_rules[rulemapExpr]() {
					goto l64
				}
			l66:
				{
					position67, tokenIndex67 := position, tokenIndex
					if !_rules[rulesep]() {
						goto l67
					}
					if !_rules[rulemapExpr]() {
						goto l67
					}
					goto l66
				l67:
					position, tokenIndex = position67, tokenIndex67
				}
				add(rulemapBody, position65)
			}
			return true
		l64:
			position, tokenIndex = position64, tokenIndex64
			return false
		},
		/* 19 mapExpr <- <(mapKey sep mapValue)> */
		func() bool {
			position68, tokenIndex68 := position, tokenIndex
			{
				position69 := position
				if !_rules[rulemapKey]() {
					goto l68
				}
				if !_rules[rulesep]() {
					goto l68
				}
				if !_rules[rulemapValue]() {
					goto l68
				}
				add(rulemapExpr, position69)
			}
			return true
		l68:
			position, tokenIndex = position68, tokenIndex68
			return false
		},
		/* 20 mapKey <- <(String / Symbol)> */
		func() bool {
			position70, tokenIndex70 := position, tokenIndex
			{
				position71 := position
				{
					position72, tokenIndex72 := position, tokenIndex
					if !_rules[ruleString]() {
						goto l73
					}
					goto l72
				l73:
					position, tokenIndex = position72, tokenIndex72
					if !_rules[ruleSymbol]() {
						goto l70
					}
				}
			l72:
				add(rulemapKey, position71)
			}
			return true
		l70:
			position, tokenIndex = position70, tokenIndex70
			return false
		},
		/* 21 mapValue <- <Expr> */
		func() bool {
			position74, tokenIndex74 := position, tokenIndex
			{
				position75 := position
				if !_rules[ruleExpr]() {
					goto l74
				}
				add(rulemapValue, position75)
			}
			return true
		l74:
			position, tokenIndex = position74, tokenIndex74
			return false
		},
		/* 22 Symbol <- <((<bareSymbol> Action9) / (':' <String> Action10))> */
		func() bool {
			position76, tokenIndex76 := position, tokenIndex
			{
				position77 := position
				{
					position78, tokenIndex78 := position, tokenIndex
					{
						position80 := position
						if !_rules[rulebareSymbol]() {
							goto l79
						}
						add(rulePegText, position80)
					}
					if !_rules[ruleAction9]() {
						goto l79
					}
					goto l78
				l79:
					position, tokenIndex = position78, tokenIndex78
					if buffer[position] != rune(':') {
						goto l76
					}
					position++
					{
						position81 := position
						if !_rules[ruleString]() {
							goto l76
						}
						add(rulePegText, position81)
					}
					if !_rules[ruleAction10]() {
						goto l76
					}
				}
			l78:
				add(ruleSymbol, position77)
			}
			return true
		l76:
			position, tokenIndex = position76, tokenIndex76
			return false
		},
		/* 23 bareSymbol <- <(('.' / '?' / '/' / '!' / '@' / '$' / '%' / '^' / '&' / '*' / '|' / '_' / [a-z] / [A-Z]) bareSymbolTail?)> */
		func() bool {
			position82, tokenIndex82 := position, tokenIndex
			{
				position83 := position
				{
					position84, tokenIndex84 := position, tokenIndex
					if buffer[position] != rune('.') {
						goto l85
					}
					position++
					goto l84
				l85:
					position, tokenIndex = position84, tokenIndex84
					if buffer[position] != rune('?') {
						goto l86
					}
					position++
					goto l84
				l86:
					position, tokenIndex = position84, tokenIndex84
					if buffer[position] != rune('/') {
						goto l87
					}
					position++
					goto l84
				l87:
					position, tokenIndex = position84, tokenIndex84
					if buffer[position] != rune('!') {
						goto l88
					}
					position++
					goto l84
				l88:
					position, tokenIndex = position84, tokenIndex84
					if buffer[position] != rune('@') {
						goto l89
					}
					position++
					goto l84
				l89:
					position, tokenIndex = position84, tokenIndex84
					if buffer[position] != rune('$') {
						goto l90
					}
					position++
					goto l84
				l90:
					position, tokenIndex = position84, tokenIndex84
					if buffer[position] != rune('%') {
						goto l91
					}
					position++
					goto l84
				l91:
					position, tokenIndex = position84, tokenIndex84
					if buffer[position] != rune('^') {
						goto l92
					}
					position++
					goto l84
				l92:
					position, tokenIndex = position84, tokenIndex84
					if buffer[position] != rune('&') {
						goto l93
					}
					position++
					goto l84
				l93:
					position, tokenIndex = position84, tokenIndex84
					if buffer[position] != rune('*') {
						goto l94
					}
					position++
					goto l84
				l94:
					position, tokenIndex = position84, tokenIndex84
					if buffer[position] != rune('|') {
						goto l95
					}
					position++
					goto l84
				l95:
					position, tokenIndex = position84, tokenIndex84
					if buffer[position] != rune('_') {
						goto l96
					}
					position++
					goto l84
				l96:
					position, tokenIndex = position84, tokenIndex84
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l97
					}
					position++
					goto l84
				l97:
					position, tokenIndex = position84, tokenIndex84
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l82
					}
					position++
				}
			l84:
				{
					position98, tokenIndex98 := position, tokenIndex
					if !_rules[rulebareSymbolTail]() {
						goto l98
					}
					goto l99
				l98:
					position, tokenIndex = position98, tokenIndex98
				}
			l99:
				add(rulebareSymbol, position83)
			}
			return true
		l82:
			position, tokenIndex = position82, tokenIndex82
			return false
		},
		/* 24 bareSymbolTail <- <('-' / '+' / '=' / '<' / '>' / '.' / '?' / '/' / '!' / '@' / '$' / '%' / '^' / '&' / '*' / '#' / '|' / ':' / '_' / [a-z] / [A-Z] / [0-9])+> */
		func() bool {
			position100, tokenIndex100 := position, tokenIndex
			{
				position101 := position
				{
					position104, tokenIndex104 := position, tokenIndex
					if buffer[position] != rune('-') {
						goto l105
					}
					position++
					goto l104
				l105:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('+') {
						goto l106
					}
					position++
					goto l104
				l106:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('=') {
						goto l107
					}
					position++
					goto l104
				l107:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('<') {
						goto l108
					}
					position++
					goto l104
				l108:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('>') {
						goto l109
					}
					position++
					goto l104
				l109:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('.') {
						goto l110
					}
					position++
					goto l104
				l110:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('?') {
						goto l111
					}
					position++
					goto l104
				l111:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('/') {
						goto l112
					}
					position++
					goto l104
				l112:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('!') {
						goto l113
					}
					position++
					goto l104
				l113:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('@') {
						goto l114
					}
					position++
					goto l104
				l114:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('$') {
						goto l115
					}
					position++
					goto l104
				l115:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('%') {
						goto l116
					}
					position++
					goto l104
				l116:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('^') {
						goto l117
					}
					position++
					goto l104
				l117:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('&') {
						goto l118
					}
					position++
					goto l104
				l118:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('*') {
						goto l119
					}
					position++
					goto l104
				l119:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('#') {
						goto l120
					}
					position++
					goto l104
				l120:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('|') {
						goto l121
					}
					position++
					goto l104
				l121:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune(':') {
						goto l122
					}
					position++
					goto l104
				l122:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('_') {
						goto l123
					}
					position++
					goto l104
				l123:
					position, tokenIndex = position104, tokenIndex104
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l124
					}
					position++
					goto l104
				l124:
					position, tokenIndex = position104, tokenIndex104
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l125
					}
					position++
					goto l104
				l125:
					position, tokenIndex = position104, tokenIndex104
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l100
					}
					position++
				}
			l104:
			l102:
				{
					position103, tokenIndex103 := position, tokenIndex
					{
						position126, tokenIndex126 := position, tokenIndex
						if buffer[position] != rune('-') {
							goto l127
						}
						position++
						goto l126
					l127:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('+') {
							goto l128
						}
						position++
						goto l126
					l128:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('=') {
							goto l129
						}
						position++
						goto l126
					l129:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('<') {
							goto l130
						}
						position++
						goto l126
					l130:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('>') {
							goto l131
						}
						position++
						goto l126
					l131:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('.') {
							goto l132
						}
						position++
						goto l126
					l132:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('?') {
							goto l133
						}
						position++
						goto l126
					l133:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('/') {
							goto l134
						}
						position++
						goto l126
					l134:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('!') {
							goto l135
						}
						position++
						goto l126
					l135:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('@') {
							goto l136
						}
						position++
						goto l126
					l136:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('$') {
							goto l137
						}
						position++
						goto l126
					l137:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('%') {
							goto l138
						}
						position++
						goto l126
					l138:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('^') {
							goto l139
						}
						position++
						goto l126
					l139:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('&') {
							goto l140
						}
						position++
						goto l126
					l140:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('*') {
							goto l141
						}
						position++
						goto l126
					l141:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('#') {
							goto l142
						}
						position++
						goto l126
					l142:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('|') {
							goto l143
						}
						position++
						goto l126
					l143:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune(':') {
							goto l144
						}
						position++
						goto l126
					l144:
						position, tokenIndex = position126, tokenIndex126
						if buffer[position] != rune('_') {
							goto l145
						}
						position++
						goto l126
					l145:
						position, tokenIndex = position126, tokenIndex126
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l146
						}
						position++
						goto l126
					l146:
						position, tokenIndex = position126, tokenIndex126
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l147
						}
						position++
						goto l126
					l147:
						position, tokenIndex = position126, tokenIndex126
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l103
						}
						position++
					}
				l126:
					goto l102
				l103:
					position, tokenIndex = position103, tokenIndex103
				}
				add(rulebareSymbolTail, position101)
			}
			return true
		l100:
			position, tokenIndex = position100, tokenIndex100
			return false
		},
		/* 25 Regexp <- <(':' '/' Action11 regexpBody '/' Action12)> */
		func() bool {
			position148, tokenIndex148 := position, tokenIndex
			{
				position149 := position
				if buffer[position] != rune(':') {
					goto l148
				}
				position++
				if buffer[position] != rune('/') {
					goto l148
				}
				position++
				if !_rules[ruleAction11]() {
					goto l148
				}
				if !_rules[ruleregexpBody]() {
					goto l148
				}
				if buffer[position] != rune('/') {
					goto l148
				}
				position++
				if !_rules[ruleAction12]() {
					goto l148
				}
				add(ruleRegexp, position149)
			}
			return true
		l148:
			position, tokenIndex = position148, tokenIndex148
			return false
		},
		/* 26 regexpBody <- <(regexpEscape / regexpTail)*> */
		func() bool {
			{
				position151 := position
			l152:
				{
					position153, tokenIndex153 := position, tokenIndex
					{
						position154, tokenIndex154 := position, tokenIndex
						if !_rules[ruleregexpEscape]() {
							goto l155
						}
						goto l154
					l155:
						position, tokenIndex = position154, tokenIndex154
						if !_rules[ruleregexpTail]() {
							goto l153
						}
					}
				l154:
					goto l152
				l153:
					position, tokenIndex = position153, tokenIndex153
				}
				add(ruleregexpBody, position151)
			}
			return true
		},
		/* 27 regexpTail <- <(<(!('\\' / '/') .)+> Action13)> */
		func() bool {
			position156, tokenIndex156 := position, tokenIndex
			{
				position157 := position
				{
					position158 := position
					{
						position161, tokenIndex161 := position, tokenIndex
						{
							position162, tokenIndex162 := position, tokenIndex
							if buffer[position] != rune('\\') {
								goto l163
							}
							position++
							goto l162
						l163:
							position, tokenIndex = position162, tokenIndex162
							if buffer[position] != rune('/') {
								goto l161
							}
							position++
						}
					l162:
						goto l156
					l161:
						position, tokenIndex = position161, tokenIndex161
					}
					if !matchDot() {
						goto l156
					}
				l159:
					{
						position160, tokenIndex160 := position, tokenIndex
						{
							position164, tokenIndex164 := position, tokenIndex
							{
								position165, tokenIndex165 := position, tokenIndex
								if buffer[position] != rune('\\') {
									goto l166
								}
								position++
								goto l165
							l166:
								position, tokenIndex = position165, tokenIndex165
								if buffer[position] != rune('/') {
									goto l164
								}
								position++
							}
						l165:
							goto l160
						l164:
							position, tokenIndex = position164, tokenIndex164
						}
						if !matchDot() {
							goto l160
						}
						goto l159
					l160:
						position, tokenIndex = position160, tokenIndex160
					}
					add(rulePegText, position158)
				}
				if !_rules[ruleAction13]() {
					goto l156
				}
				add(ruleregexpTail, position157)
			}
			return true
		l156:
			position, tokenIndex = position156, tokenIndex156
			return false
		},
		/* 28 regexpEscape <- <('\\' ('/' Action14))> */
		func() bool {
			position167, tokenIndex167 := position, tokenIndex
			{
				position168 := position
				if buffer[position] != rune('\\') {
					goto l167
				}
				position++
				if buffer[position] != rune('/') {
					goto l167
				}
				position++
				if !_rules[ruleAction14]() {
					goto l167
				}
				add(ruleregexpEscape, position168)
			}
			return true
		l167:
			position, tokenIndex = position167, tokenIndex167
			return false
		},
		/* 29 String <- <(<quotedString> Action15)> */
		func() bool {
			position169, tokenIndex169 := position, tokenIndex
			{
				position170 := position
				{
					position171 := position
					if !_rules[rulequotedString]() {
						goto l169
					}
					add(rulePegText, position171)
				}
				if !_rules[ruleAction15]() {
					goto l169
				}
				add(ruleString, position170)
			}
			return true
		l169:
			position, tokenIndex = position169, tokenIndex169
			return false
		},
		/* 30 quotedString <- <('"' stringBody '"')> */
		func() bool {
			position172, tokenIndex172 := position, tokenIndex
			{
				position173 := position
				if buffer[position] != rune('"') {
					goto l172
				}
				position++
				if !_rules[rulestringBody]() {
					goto l172
				}
				if buffer[position] != rune('"') {
					goto l172
				}
				position++
				add(rulequotedString, position173)
			}
			return true
		l172:
			position, tokenIndex = position172, tokenIndex172
			return false
		},
		/* 31 stringBody <- <(stringEscape / doubleQuoteEscape / stringTail)*> */
		func() bool {
			{
				position175 := position
			l176:
				{
					position177, tokenIndex177 := position, tokenIndex
					{
						position178, tokenIndex178 := position, tokenIndex
						if !_rules[rulestringEscape]() {
							goto l179
						}
						goto l178
					l179:
						position, tokenIndex = position178, tokenIndex178
						if !_rules[ruledoubleQuoteEscape]() {
							goto l180
						}
						goto l178
					l180:
						position, tokenIndex = position178, tokenIndex178
						if !_rules[rulestringTail]() {
							goto l177
						}
					}
				l178:
					goto l176
				l177:
					position, tokenIndex = position177, tokenIndex177
				}
				add(rulestringBody, position175)
			}
			return true
		},
		/* 32 doubleQuoteEscape <- <('"' '"')> */
		func() bool {
			position181, tokenIndex181 := position, tokenIndex
			{
				position182 := position
				if buffer[position] != rune('"') {
					goto l181
				}
				position++
				if buffer[position] != rune('"') {
					goto l181
				}
				position++
				add(ruledoubleQuoteEscape, position182)
			}
			return true
		l181:
			position, tokenIndex = position181, tokenIndex181
			return false
		},
		/* 33 stringTail <- <(!('\\' / '"') .)+> */
		func() bool {
			position183, tokenIndex183 := position, tokenIndex
			{
				position184 := position
				{
					position187, tokenIndex187 := position, tokenIndex
					{
						position188, tokenIndex188 := position, tokenIndex
						if buffer[position] != rune('\\') {
							goto l189
						}
						position++
						goto l188
					l189:
						position, tokenIndex = position188, tokenIndex188
						if buffer[position] != rune('"') {
							goto l187
						}
						position++
					}
				l188:
					goto l183
				l187:
					position, tokenIndex = position187, tokenIndex187
				}
				if !matchDot() {
					goto l183
				}
			l185:
				{
					position186, tokenIndex186 := position, tokenIndex
					{
						position190, tokenIndex190 := position, tokenIndex
						{
							position191, tokenIndex191 := position, tokenIndex
							if buffer[position] != rune('\\') {
								goto l192
							}
							position++
							goto l191
						l192:
							position, tokenIndex = position191, tokenIndex191
							if buffer[position] != rune('"') {
								goto l190
							}
							position++
						}
					l191:
						goto l186
					l190:
						position, tokenIndex = position190, tokenIndex190
					}
					if !matchDot() {
						goto l186
					}
					goto l185
				l186:
					position, tokenIndex = position186, tokenIndex186
				}
				add(rulestringTail, position184)
			}
			return true
		l183:
			position, tokenIndex = position183, tokenIndex183
			return false
		},
		/* 34 stringEscape <- <('\\' (('\r'? '\n') / ('\\' / 'a' / 'b' / 'f' / 'n' / 'r' / 't' / 'v' / 'e') / stringOct / ('x' stringHex) / ('u' stringUni16) / ('U' stringUni32)))> */
		func() bool {
			position193, tokenIndex193 := position, tokenIndex
			{
				position194 := position
				if buffer[position] != rune('\\') {
					goto l193
				}
				position++
				{
					position195, tokenIndex195 := position, tokenIndex
					{
						position197, tokenIndex197 := position, tokenIndex
						if buffer[position] != rune('\r') {
							goto l197
						}
						position++
						goto l198
					l197:
						position, tokenIndex = position197, tokenIndex197
					}
				l198:
					if buffer[position] != rune('\n') {
						goto l196
					}
					position++
					goto l195
				l196:
					position, tokenIndex = position195, tokenIndex195
					{
						position200, tokenIndex200 := position, tokenIndex
						if buffer[position] != rune('\\') {
							goto l201
						}
						position++
						goto l200
					l201:
						position, tokenIndex = position200, tokenIndex200
						if buffer[position] != rune('a') {
							goto l202
						}
						position++
						goto l200
					l202:
						position, tokenIndex = position200, tokenIndex200
						if buffer[position] != rune('b') {
							goto l203
						}
						position++
						goto l200
					l203:
						position, tokenIndex = position200, tokenIndex200
						if buffer[position] != rune('f') {
							goto l204
						}
						position++
						goto l200
					l204:
						position, tokenIndex = position200, tokenIndex200
						if buffer[position] != rune('n') {
							goto l205
						}
						position++
						goto l200
					l205:
						position, tokenIndex = position200, tokenIndex200
						if buffer[position] != rune('r') {
							goto l206
						}
						position++
						goto l200
					l206:
						position, tokenIndex = position200, tokenIndex200
						if buffer[position] != rune('t') {
							goto l207
						}
						position++
						goto l200
					l207:
						position, tokenIndex = position200, tokenIndex200
						if buffer[position] != rune('v') {
							goto l208
						}
						position++
						goto l200
					l208:
						position, tokenIndex = position200, tokenIndex200
						if buffer[position] != rune('e') {
							goto l199
						}
						position++
					}
				l200:
					goto l195
				l199:
					position, tokenIndex = position195, tokenIndex195
					if !_rules[rulestringOct]() {
						goto l209
					}
					goto l195
				l209:
					position, tokenIndex = position195, tokenIndex195
					if buffer[position] != rune('x') {
						goto l210
					}
					position++
					if !_rules[rulestringHex]() {
						goto l210
					}
					goto l195
				l210:
					position, tokenIndex = position195, tokenIndex195
					if buffer[position] != rune('u') {
						goto l211
					}
					position++
					if !_rules[rulestringUni16]() {
						goto l211
					}
					goto l195
				l211:
					position, tokenIndex = position195, tokenIndex195
					if buffer[position] != rune('U') {
						goto l193
					}
					position++
					if !_rules[rulestringUni32]() {
						goto l193
					}
				}
			l195:
				add(rulestringEscape, position194)
			}
			return true
		l193:
			position, tokenIndex = position193, tokenIndex193
			return false
		},
		/* 35 stringOct <- <(octDigit (octDigit octDigit?)?)> */
		func() bool {
			position212, tokenIndex212 := position, tokenIndex
			{
				position213 := position
				if !_rules[ruleoctDigit]() {
					goto l212
				}
				{
					position214, tokenIndex214 := position, tokenIndex
					if !_rules[ruleoctDigit]() {
						goto l214
					}
					{
						position216, tokenIndex216 := position, tokenIndex
						if !_rules[ruleoctDigit]() {
							goto l216
						}
						goto l217
					l216:
						position, tokenIndex = position216, tokenIndex216
					}
				l217:
					goto l215
				l214:
					position, tokenIndex = position214, tokenIndex214
				}
			l215:
				add(rulestringOct, position213)
			}
			return true
		l212:
			position, tokenIndex = position212, tokenIndex212
			return false
		},
		/* 36 stringHex <- <hexOctet> */
		func() bool {
			position218, tokenIndex218 := position, tokenIndex
			{
				position219 := position
				if !_rules[rulehexOctet]() {
					goto l218
				}
				add(rulestringHex, position219)
			}
			return true
		l218:
			position, tokenIndex = position218, tokenIndex218
			return false
		},
		/* 37 stringUni16 <- <(hexOctet hexOctet)> */
		func() bool {
			position220, tokenIndex220 := position, tokenIndex
			{
				position221 := position
				if !_rules[rulehexOctet]() {
					goto l220
				}
				if !_rules[rulehexOctet]() {
					goto l220
				}
				add(rulestringUni16, position221)
			}
			return true
		l220:
			position, tokenIndex = position220, tokenIndex220
			return false
		},
		/* 38 stringUni32 <- <(hexOctet hexOctet hexOctet hexOctet)> */
		func() bool {
			position222, tokenIndex222 := position, tokenIndex
			{
				position223 := position
				if !_rules[rulehexOctet]() {
					goto l222
				}
				if !_rules[rulehexOctet]() {
					goto l222
				}
				if !_rules[rulehexOctet]() {
					goto l222
				}
				if !_rules[rulehexOctet]() {
					goto l222
				}
				add(rulestringUni32, position223)
			}
			return true
		l222:
			position, tokenIndex = position222, tokenIndex222
			return false
		},
		/* 39 Number <- <(Action16 sign? (Decimal / Rational / Integer))> */
		func() bool {
			position224, tokenIndex224 := position, tokenIndex
			{
				position225 := position
				if !_rules[ruleAction16]() {
					goto l224
				}
				{
					position226, tokenIndex226 := position, tokenIndex
					if !_rules[rulesign]() {
						goto l226
					}
					goto l227
				l226:
					position, tokenIndex = position226, tokenIndex226
				}
			l227:
				{
					position228, tokenIndex228 := position, tokenIndex
					if !_rules[ruleDecimal]() {
						goto l229
					}
					goto l228
				l229:
					position, tokenIndex = position228, tokenIndex228
					if !_rules[ruleRational]() {
						goto l230
					}
					goto l228
				l230:
					position, tokenIndex = position228, tokenIndex228
					if !_rules[ruleInteger]() {
						goto l224
					}
				}
			l228:
				add(ruleNumber, position225)
			}
			return true
		l224:
			position, tokenIndex = position224, tokenIndex224
			return false
		},
		/* 40 octDigit <- <[0-8]> */
		func() bool {
			position231, tokenIndex231 := position, tokenIndex
			{
				position232 := position
				if c := buffer[position]; c < rune('0') || c > rune('8') {
					goto l231
				}
				position++
				add(ruleoctDigit, position232)
			}
			return true
		l231:
			position, tokenIndex = position231, tokenIndex231
			return false
		},
		/* 41 hexNibble <- <([0-9] / [a-f] / [A-F])> */
		func() bool {
			position233, tokenIndex233 := position, tokenIndex
			{
				position234 := position
				{
					position235, tokenIndex235 := position, tokenIndex
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l236
					}
					position++
					goto l235
				l236:
					position, tokenIndex = position235, tokenIndex235
					if c := buffer[position]; c < rune('a') || c > rune('f') {
						goto l237
					}
					position++
					goto l235
				l237:
					position, tokenIndex = position235, tokenIndex235
					if c := buffer[position]; c < rune('A') || c > rune('F') {
						goto l233
					}
					position++
				}
			l235:
				add(rulehexNibble, position234)
			}
			return true
		l233:
			position, tokenIndex = position233, tokenIndex233
			return false
		},
		/* 42 hexOctet <- <(hexNibble hexNibble)> */
		func() bool {
			position238, tokenIndex238 := position, tokenIndex
			{
				position239 := position
				if !_rules[rulehexNibble]() {
					goto l238
				}
				if !_rules[rulehexNibble]() {
					goto l238
				}
				add(rulehexOctet, position239)
			}
			return true
		l238:
			position, tokenIndex = position238, tokenIndex238
			return false
		},
		/* 43 Rational <- <(<(decInt '/' decInt)> Action17)> */
		func() bool {
			position240, tokenIndex240 := position, tokenIndex
			{
				position241 := position
				{
					position242 := position
					if !_rules[ruledecInt]() {
						goto l240
					}
					if buffer[position] != rune('/') {
						goto l240
					}
					position++
					if !_rules[ruledecInt]() {
						goto l240
					}
					add(rulePegText, position242)
				}
				if !_rules[ruleAction17]() {
					goto l240
				}
				add(ruleRational, position241)
			}
			return true
		l240:
			position, tokenIndex = position240, tokenIndex240
			return false
		},
		/* 44 Decimal <- <((<normDec> Action18) / (<expDec> Action19))> */
		func() bool {
			position243, tokenIndex243 := position, tokenIndex
			{
				position244 := position
				{
					position245, tokenIndex245 := position, tokenIndex
					{
						position247 := position
						if !_rules[rulenormDec]() {
							goto l246
						}
						add(rulePegText, position247)
					}
					if !_rules[ruleAction18]() {
						goto l246
					}
					goto l245
				l246:
					position, tokenIndex = position245, tokenIndex245
					{
						position248 := position
						if !_rules[ruleexpDec]() {
							goto l243
						}
						add(rulePegText, position248)
					}
					if !_rules[ruleAction19]() {
						goto l243
					}
				}
			l245:
				add(ruleDecimal, position244)
			}
			return true
		l243:
			position, tokenIndex = position243, tokenIndex243
			return false
		},
		/* 45 normDec <- <(decInt '.' decInt exponent?)> */
		func() bool {
			position249, tokenIndex249 := position, tokenIndex
			{
				position250 := position
				if !_rules[ruledecInt]() {
					goto l249
				}
				if buffer[position] != rune('.') {
					goto l249
				}
				position++
				if !_rules[ruledecInt]() {
					goto l249
				}
				{
					position251, tokenIndex251 := position, tokenIndex
					if !_rules[ruleexponent]() {
						goto l251
					}
					goto l252
				l251:
					position, tokenIndex = position251, tokenIndex251
				}
			l252:
				add(rulenormDec, position250)
			}
			return true
		l249:
			position, tokenIndex = position249, tokenIndex249
			return false
		},
		/* 46 expDec <- <(decInt exponent)> */
		func() bool {
			position253, tokenIndex253 := position, tokenIndex
			{
				position254 := position
				if !_rules[ruledecInt]() {
					goto l253
				}
				if !_rules[ruleexponent]() {
					goto l253
				}
				add(ruleexpDec, position254)
			}
			return true
		l253:
			position, tokenIndex = position253, tokenIndex253
			return false
		},
		/* 47 exponent <- <(('E' / 'e') sign? decInt)> */
		func() bool {
			position255, tokenIndex255 := position, tokenIndex
			{
				position256 := position
				{
					position257, tokenIndex257 := position, tokenIndex
					if buffer[position] != rune('E') {
						goto l258
					}
					position++
					goto l257
				l258:
					position, tokenIndex = position257, tokenIndex257
					if buffer[position] != rune('e') {
						goto l255
					}
					position++
				}
			l257:
				{
					position259, tokenIndex259 := position, tokenIndex
					if !_rules[rulesign]() {
						goto l259
					}
					goto l260
				l259:
					position, tokenIndex = position259, tokenIndex259
				}
			l260:
				if !_rules[ruledecInt]() {
					goto l255
				}
				add(ruleexponent, position256)
			}
			return true
		l255:
			position, tokenIndex = position255, tokenIndex255
			return false
		},
		/* 48 Integer <- <(BaseInt / HexLit / BinLit / OctLit / DecInt)> */
		func() bool {
			position261, tokenIndex261 := position, tokenIndex
			{
				position262 := position
				{
					position263, tokenIndex263 := position, tokenIndex
					if !_rules[ruleBaseInt]() {
						goto l264
					}
					goto l263
				l264:
					position, tokenIndex = position263, tokenIndex263
					if !_rules[ruleHexLit]() {
						goto l265
					}
					goto l263
				l265:
					position, tokenIndex = position263, tokenIndex263
					if !_rules[ruleBinLit]() {
						goto l266
					}
					goto l263
				l266:
					position, tokenIndex = position263, tokenIndex263
					if !_rules[ruleOctLit]() {
						goto l267
					}
					goto l263
				l267:
					position, tokenIndex = position263, tokenIndex263
					if !_rules[ruleDecInt]() {
						goto l261
					}
				}
			l263:
				add(ruleInteger, position262)
			}
			return true
		l261:
			position, tokenIndex = position261, tokenIndex261
			return false
		},
		/* 49 decInt <- <('0' / posDecNum)> */
		func() bool {
			position268, tokenIndex268 := position, tokenIndex
			{
				position269 := position
				{
					position270, tokenIndex270 := position, tokenIndex
					if buffer[position] != rune('0') {
						goto l271
					}
					position++
					goto l270
				l271:
					position, tokenIndex = position270, tokenIndex270
					if !_rules[ruleposDecNum]() {
						goto l268
					}
				}
			l270:
				add(ruledecInt, position269)
			}
			return true
		l268:
			position, tokenIndex = position268, tokenIndex268
			return false
		},
		/* 50 DecInt <- <(<decInt> Action20)> */
		func() bool {
			position272, tokenIndex272 := position, tokenIndex
			{
				position273 := position
				{
					position274 := position
					if !_rules[ruledecInt]() {
						goto l272
					}
					add(rulePegText, position274)
				}
				if !_rules[ruleAction20]() {
					goto l272
				}
				add(ruleDecInt, position273)
			}
			return true
		l272:
			position, tokenIndex = position272, tokenIndex272
			return false
		},
		/* 51 posDecNum <- <([1-9] [0-9]*)> */
		func() bool {
			position275, tokenIndex275 := position, tokenIndex
			{
				position276 := position
				if c := buffer[position]; c < rune('1') || c > rune('9') {
					goto l275
				}
				position++
			l277:
				{
					position278, tokenIndex278 := position, tokenIndex
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l278
					}
					position++
					goto l277
				l278:
					position, tokenIndex = position278, tokenIndex278
				}
				add(ruleposDecNum, position276)
			}
			return true
		l275:
			position, tokenIndex = position275, tokenIndex275
			return false
		},
		/* 52 BaseInt <- <(<baseDec> Action21 '#' <baseLit> Action22)> */
		func() bool {
			position279, tokenIndex279 := position, tokenIndex
			{
				position280 := position
				{
					position281 := position
					if !_rules[rulebaseDec]() {
						goto l279
					}
					add(rulePegText, position281)
				}
				if !_rules[ruleAction21]() {
					goto l279
				}
				if buffer[position] != rune('#') {
					goto l279
				}
				position++
				{
					position282 := position
					if !_rules[rulebaseLit]() {
						goto l279
					}
					add(rulePegText, position282)
				}
				if !_rules[ruleAction22]() {
					goto l279
				}
				add(ruleBaseInt, position280)
			}
			return true
		l279:
			position, tokenIndex = position279, tokenIndex279
			return false
		},
		/* 53 baseDec <- <((('1' / '2') [0-9]) / ('3' [0-6]) / ([2-9] ![0-9]))> */
		func() bool {
			position283, tokenIndex283 := position, tokenIndex
			{
				position284 := position
				{
					position285, tokenIndex285 := position, tokenIndex
					{
						position287, tokenIndex287 := position, tokenIndex
						if buffer[position] != rune('1') {
							goto l288
						}
						position++
						goto l287
					l288:
						position, tokenIndex = position287, tokenIndex287
						if buffer[position] != rune('2') {
							goto l286
						}
						position++
					}
				l287:
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l286
					}
					position++
					goto l285
				l286:
					position, tokenIndex = position285, tokenIndex285
					if buffer[position] != rune('3') {
						goto l289
					}
					position++
					if c := buffer[position]; c < rune('0') || c > rune('6') {
						goto l289
					}
					position++
					goto l285
				l289:
					position, tokenIndex = position285, tokenIndex285
					if c := buffer[position]; c < rune('2') || c > rune('9') {
						goto l283
					}
					position++
					{
						position290, tokenIndex290 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l290
						}
						position++
						goto l283
					l290:
						position, tokenIndex = position290, tokenIndex290
					}
				}
			l285:
				add(rulebaseDec, position284)
			}
			return true
		l283:
			position, tokenIndex = position283, tokenIndex283
			return false
		},
		/* 54 baseLit <- <([a-z] / [A-Z] / [0-9])+> */
		func() bool {
			position291, tokenIndex291 := position, tokenIndex
			{
				position292 := position
				{
					position295, tokenIndex295 := position, tokenIndex
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l296
					}
					position++
					goto l295
				l296:
					position, tokenIndex = position295, tokenIndex295
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l297
					}
					position++
					goto l295
				l297:
					position, tokenIndex = position295, tokenIndex295
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l291
					}
					position++
				}
			l295:
			l293:
				{
					position294, tokenIndex294 := position, tokenIndex
					{
						position298, tokenIndex298 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l299
						}
						position++
						goto l298
					l299:
						position, tokenIndex = position298, tokenIndex298
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l300
						}
						position++
						goto l298
					l300:
						position, tokenIndex = position298, tokenIndex298
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l294
						}
						position++
					}
				l298:
					goto l293
				l294:
					position, tokenIndex = position294, tokenIndex294
				}
				add(rulebaseLit, position292)
			}
			return true
		l291:
			position, tokenIndex = position291, tokenIndex291
			return false
		},
		/* 55 HexLit <- <('0' ('X' / 'x') <hexNibble+> Action23)> */
		func() bool {
			position301, tokenIndex301 := position, tokenIndex
			{
				position302 := position
				if buffer[position] != rune('0') {
					goto l301
				}
				position++
				{
					position303, tokenIndex303 := position, tokenIndex
					if buffer[position] != rune('X') {
						goto l304
					}
					position++
					goto l303
				l304:
					position, tokenIndex = position303, tokenIndex303
					if buffer[position] != rune('x') {
						goto l301
					}
					position++
				}
			l303:
				{
					position305 := position
					if !_rules[rulehexNibble]() {
						goto l301
					}
				l306:
					{
						position307, tokenIndex307 := position, tokenIndex
						if !_rules[rulehexNibble]() {
							goto l307
						}
						goto l306
					l307:
						position, tokenIndex = position307, tokenIndex307
					}
					add(rulePegText, position305)
				}
				if !_rules[ruleAction23]() {
					goto l301
				}
				add(ruleHexLit, position302)
			}
			return true
		l301:
			position, tokenIndex = position301, tokenIndex301
			return false
		},
		/* 56 OctLit <- <('0' <[0-7]+> ![8-9] Action24)> */
		func() bool {
			position308, tokenIndex308 := position, tokenIndex
			{
				position309 := position
				if buffer[position] != rune('0') {
					goto l308
				}
				position++
				{
					position310 := position
					if c := buffer[position]; c < rune('0') || c > rune('7') {
						goto l308
					}
					position++
				l311:
					{
						position312, tokenIndex312 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('7') {
							goto l312
						}
						position++
						goto l311
					l312:
						position, tokenIndex = position312, tokenIndex312
					}
					add(rulePegText, position310)
				}
				{
					position313, tokenIndex313 := position, tokenIndex
					if c := buffer[position]; c < rune('8') || c > rune('9') {
						goto l313
					}
					position++
					goto l308
				l313:
					position, tokenIndex = position313, tokenIndex313
				}
				if !_rules[ruleAction24]() {
					goto l308
				}
				add(ruleOctLit, position309)
			}
			return true
		l308:
			position, tokenIndex = position308, tokenIndex308
			return false
		},
		/* 57 BinLit <- <('0' ('B' / 'b') <('0' / '1')+> ![2-9] Action25)> */
		func() bool {
			position314, tokenIndex314 := position, tokenIndex
			{
				position315 := position
				if buffer[position] != rune('0') {
					goto l314
				}
				position++
				{
					position316, tokenIndex316 := position, tokenIndex
					if buffer[position] != rune('B') {
						goto l317
					}
					position++
					goto l316
				l317:
					position, tokenIndex = position316, tokenIndex316
					if buffer[position] != rune('b') {
						goto l314
					}
					position++
				}
			l316:
				{
					position318 := position
					{
						position321, tokenIndex321 := position, tokenIndex
						if buffer[position] != rune('0') {
							goto l322
						}
						position++
						goto l321
					l322:
						position, tokenIndex = position321, tokenIndex321
						if buffer[position] != rune('1') {
							goto l314
						}
						position++
					}
				l321:
				l319:
					{
						position320, tokenIndex320 := position, tokenIndex
						{
							position323, tokenIndex323 := position, tokenIndex
							if buffer[position] != rune('0') {
								goto l324
							}
							position++
							goto l323
						l324:
							position, tokenIndex = position323, tokenIndex323
							if buffer[position] != rune('1') {
								goto l320
							}
							position++
						}
					l323:
						goto l319
					l320:
						position, tokenIndex = position320, tokenIndex320
					}
					add(rulePegText, position318)
				}
				{
					position325, tokenIndex325 := position, tokenIndex
					if c := buffer[position]; c < rune('2') || c > rune('9') {
						goto l325
					}
					position++
					goto l314
				l325:
					position, tokenIndex = position325, tokenIndex325
				}
				if !_rules[ruleAction25]() {
					goto l314
				}
				add(ruleBinLit, position315)
			}
			return true
		l314:
			position, tokenIndex = position314, tokenIndex314
			return false
		},
		/* 58 sign <- <((minus Action26) / (plus Action27))> */
		func() bool {
			position326, tokenIndex326 := position, tokenIndex
			{
				position327 := position
				{
					position328, tokenIndex328 := position, tokenIndex
					if !_rules[ruleminus]() {
						goto l329
					}
					if !_rules[ruleAction26]() {
						goto l329
					}
					goto l328
				l329:
					position, tokenIndex = position328, tokenIndex328
					if !_rules[ruleplus]() {
						goto l326
					}
					if !_rules[ruleAction27]() {
						goto l326
					}
				}
			l328:
				add(rulesign, position327)
			}
			return true
		l326:
			position, tokenIndex = position326, tokenIndex326
			return false
		},
		/* 59 minus <- <'-'> */
		func() bool {
			position330, tokenIndex330 := position, tokenIndex
			{
				position331 := position
				if buffer[position] != rune('-') {
					goto l330
				}
				position++
				add(ruleminus, position331)
			}
			return true
		l330:
			position, tokenIndex = position330, tokenIndex330
			return false
		},
		/* 60 plus <- <'+'> */
		func() bool {
			position332, tokenIndex332 := position, tokenIndex
			{
				position333 := position
				if buffer[position] != rune('+') {
					goto l332
				}
				position++
				add(ruleplus, position333)
			}
			return true
		l332:
			position, tokenIndex = position332, tokenIndex332
			return false
		},
		/* 61 Boolean <- <(((True Action28) / (False Action29)) !bareSymbolTail)> */
		func() bool {
			position334, tokenIndex334 := position, tokenIndex
			{
				position335 := position
				{
					position336, tokenIndex336 := position, tokenIndex
					if !_rules[ruleTrue]() {
						goto l337
					}
					if !_rules[ruleAction28]() {
						goto l337
					}
					goto l336
				l337:
					position, tokenIndex = position336, tokenIndex336
					if !_rules[ruleFalse]() {
						goto l334
					}
					if !_rules[ruleAction29]() {
						goto l334
					}
				}
			l336:
				{
					position338, tokenIndex338 := position, tokenIndex
					if !_rules[rulebareSymbolTail]() {
						goto l338
					}
					goto l334
				l338:
					position, tokenIndex = position338, tokenIndex338
				}
				add(ruleBoolean, position335)
			}
			return true
		l334:
			position, tokenIndex = position334, tokenIndex334
			return false
		},
		/* 62 True <- <(('t' 'r' 'u' 'e') / ('o' 'n') / ('y' 'e' 's'))> */
		func() bool {
			position339, tokenIndex339 := position, tokenIndex
			{
				position340 := position
				{
					position341, tokenIndex341 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l342
					}
					position++
					if buffer[position] != rune('r') {
						goto l342
					}
					position++
					if buffer[position] != rune('u') {
						goto l342
					}
					position++
					if buffer[position] != rune('e') {
						goto l342
					}
					position++
					goto l341
				l342:
					position, tokenIndex = position341, tokenIndex341
					if buffer[position] != rune('o') {
						goto l343
					}
					position++
					if buffer[position] != rune('n') {
						goto l343
					}
					position++
					goto l341
				l343:
					position, tokenIndex = position341, tokenIndex341
					if buffer[position] != rune('y') {
						goto l339
					}
					position++
					if buffer[position] != rune('e') {
						goto l339
					}
					position++
					if buffer[position] != rune('s') {
						goto l339
					}
					position++
				}
			l341:
				add(ruleTrue, position340)
			}
			return true
		l339:
			position, tokenIndex = position339, tokenIndex339
			return false
		},
		/* 63 False <- <(('f' 'a' 'l' 's' 'e') / ('o' 'f' 'f') / ('n' 'o'))> */
		func() bool {
			position344, tokenIndex344 := position, tokenIndex
			{
				position345 := position
				{
					position346, tokenIndex346 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l347
					}
					position++
					if buffer[position] != rune('a') {
						goto l347
					}
					position++
					if buffer[position] != rune('l') {
						goto l347
					}
					position++
					if buffer[position] != rune('s') {
						goto l347
					}
					position++
					if buffer[position] != rune('e') {
						goto l347
					}
					position++
					goto l346
				l347:
					position, tokenIndex = position346, tokenIndex346
					if buffer[position] != rune('o') {
						goto l348
					}
					position++
					if buffer[position] != rune('f') {
						goto l348
					}
					position++
					if buffer[position] != rune('f') {
						goto l348
					}
					position++
					goto l346
				l348:
					position, tokenIndex = position346, tokenIndex346
					if buffer[position] != rune('n') {
						goto l344
					}
					position++
					if buffer[position] != rune('o') {
						goto l344
					}
					position++
				}
			l346:
				add(ruleFalse, position345)
			}
			return true
		l344:
			position, tokenIndex = position344, tokenIndex344
			return false
		},
		/* 64 Nil <- <('n' 'i' 'l' !bareSymbolTail Action30)> */
		func() bool {
			position349, tokenIndex349 := position, tokenIndex
			{
				position350 := position
				if buffer[position] != rune('n') {
					goto l349
				}
				position++
				if buffer[position] != rune('i') {
					goto l349
				}
				position++
				if buffer[position] != rune('l') {
					goto l349
				}
				position++
				{
					position351, tokenIndex351 := position, tokenIndex
					if !_rules[rulebareSymbolTail]() {
						goto l351
					}
					goto l349
				l351:
					position, tokenIndex = position351, tokenIndex351
				}
				if !_rules[ruleAction30]() {
					goto l349
				}
				add(ruleNil, position350)
			}
			return true
		l349:
			position, tokenIndex = position349, tokenIndex349
			return false
		},
		/* 65 term <- <';'> */
		func() bool {
			position352, tokenIndex352 := position, tokenIndex
			{
				position353 := position
				if buffer[position] != rune(';') {
					goto l352
				}
				position++
				add(ruleterm, position353)
			}
			return true
		l352:
			position, tokenIndex = position352, tokenIndex352
			return false
		},
		/* 66 sep <- <(('\r'? '\n') / (' ' / '\t'))+> */
		func() bool {
			position354, tokenIndex354 := position, tokenIndex
			{
				position355 := position
				{
					position358, tokenIndex358 := position, tokenIndex
					{
						position360, tokenIndex360 := position, tokenIndex
						if buffer[position] != rune('\r') {
							goto l360
						}
						position++
						goto l361
					l360:
						position, tokenIndex = position360, tokenIndex360
					}
				l361:
					if buffer[position] != rune('\n') {
						goto l359
					}
					position++
					goto l358
				l359:
					position, tokenIndex = position358, tokenIndex358
					{
						position362, tokenIndex362 := position, tokenIndex
						if buffer[position] != rune(' ') {
							goto l363
						}
						position++
						goto l362
					l363:
						position, tokenIndex = position362, tokenIndex362
						if buffer[position] != rune('\t') {
							goto l354
						}
						position++
					}
				l362:
				}
			l358:
			l356:
				{
					position357, tokenIndex357 := position, tokenIndex
					{
						position364, tokenIndex364 := position, tokenIndex
						{
							position366, tokenIndex366 := position, tokenIndex
							if buffer[position] != rune('\r') {
								goto l366
							}
							position++
							goto l367
						l366:
							position, tokenIndex = position366, tokenIndex366
						}
					l367:
						if buffer[position] != rune('\n') {
							goto l365
						}
						position++
						goto l364
					l365:
						position, tokenIndex = position364, tokenIndex364
						{
							position368, tokenIndex368 := position, tokenIndex
							if buffer[position] != rune(' ') {
								goto l369
							}
							position++
							goto l368
						l369:
							position, tokenIndex = position368, tokenIndex368
							if buffer[position] != rune('\t') {
								goto l357
							}
							position++
						}
					l368:
					}
				l364:
					goto l356
				l357:
					position, tokenIndex = position357, tokenIndex357
				}
				add(rulesep, position355)
			}
			return true
		l354:
			position, tokenIndex = position354, tokenIndex354
			return false
		},
		/* 67 ws <- <((' ' / '\t' / '\n' / '\r')+ / comment)*> */
		func() bool {
			{
				position371 := position
			l372:
				{
					position373, tokenIndex373 := position, tokenIndex
					{
						position374, tokenIndex374 := position, tokenIndex
						{
							position378, tokenIndex378 := position, tokenIndex
							if buffer[position] != rune(' ') {
								goto l379
							}
							position++
							goto l378
						l379:
							position, tokenIndex = position378, tokenIndex378
							if buffer[position] != rune('\t') {
								goto l380
							}
							position++
							goto l378
						l380:
							position, tokenIndex = position378, tokenIndex378
							if buffer[position] != rune('\n') {
								goto l381
							}
							position++
							goto l378
						l381:
							position, tokenIndex = position378, tokenIndex378
							if buffer[position] != rune('\r') {
								goto l375
							}
							position++
						}
					l378:
					l376:
						{
							position377, tokenIndex377 := position, tokenIndex
							{
								position382, tokenIndex382 := position, tokenIndex
								if buffer[position] != rune(' ') {
									goto l383
								}
								position++
								goto l382
							l383:
								position, tokenIndex = position382, tokenIndex382
								if buffer[position] != rune('\t') {
									goto l384
								}
								position++
								goto l382
							l384:
								position, tokenIndex = position382, tokenIndex382
								if buffer[position] != rune('\n') {
									goto l385
								}
								position++
								goto l382
							l385:
								position, tokenIndex = position382, tokenIndex382
								if buffer[position] != rune('\r') {
									goto l377
								}
								position++
							}
						l382:
							goto l376
						l377:
							position, tokenIndex = position377, tokenIndex377
						}
						goto l374
					l375:
						position, tokenIndex = position374, tokenIndex374
						if !_rules[rulecomment]() {
							goto l373
						}
					}
				l374:
					goto l372
				l373:
					position, tokenIndex = position373, tokenIndex373
				}
				add(rulews, position371)
			}
			return true
		},
		/* 68 comment <- <('#' (!'\n' .)* '\n')> */
		func() bool {
			position386, tokenIndex386 := position, tokenIndex
			{
				position387 := position
				if buffer[position] != rune('#') {
					goto l386
				}
				position++
			l388:
				{
					position389, tokenIndex389 := position, tokenIndex
					{
						position390, tokenIndex390 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l390
						}
						position++
						goto l389
					l390:
						position, tokenIndex = position390, tokenIndex390
					}
					if !matchDot() {
						goto l389
					}
					goto l388
				l389:
					position, tokenIndex = position389, tokenIndex389
				}
				if buffer[position] != rune('\n') {
					goto l386
				}
				position++
				add(rulecomment, position387)
			}
			return true
		l386:
			position, tokenIndex = position386, tokenIndex386
			return false
		},
		/* 70 Action0 <- <{
			if p.root == nil {
				p.root = &Root{}
			}
			p.consumers = []consumer{p.root}
		}> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		nil,
		/* 72 Action1 <- <{ p.beginStatement(text) }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		/* 73 Action2 <- <{ p.closeStatement() }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
		/* 74 Action3 <- <{ p.beginSection(text) }> */
		func() bool {
			{
				add(ruleAction3, position)
			}
			return true
		},
		/* 75 Action4 <- <{ p.closeSection() }> */
		func() bool {
			{
				add(ruleAction4, position)
			}
			return true
		},
		/* 76 Action5 <- <{ p.beginArray() }> */
		func() bool {
			{
				add(ruleAction5, position)
			}
			return true
		},
		/* 77 Action6 <- <{ p.closeArray() }> */
		func() bool {
			{
				add(ruleAction6, position)
			}
			return true
		},
		/* 78 Action7 <- <{ p.beginMap() }> */
		func() bool {
			{
				add(ruleAction7, position)
			}
			return true
		},
		/* 79 Action8 <- <{ p.closeMap() }> */
		func() bool {
			{
				add(ruleAction8, position)
			}
			return true
		},
		/* 80 Action9 <- <{ p.consume(Symbol(text))}> */
		func() bool {
			{
				add(ruleAction9, position)
			}
			return true
		},
		/* 81 Action10 <- <{ p.consume(Symbol(unquote(typeSymbol, text))) }> */
		func() bool {
			{
				add(ruleAction10, position)
			}
			return true
		},
		/* 82 Action11 <- <{ p.beginRegexp() }> */
		func() bool {
			{
				add(ruleAction11, position)
			}
			return true
		},
		/* 83 Action12 <- <{ p.closeRegexp() }> */
		func() bool {
			{
				add(ruleAction12, position)
			}
			return true
		},
		/* 84 Action13 <- <{ p.tip().(*regexpBuilder).add(text) }> */
		func() bool {
			{
				add(ruleAction13, position)
			}
			return true
		},
		/* 85 Action14 <- <{ p.tip().(*regexpBuilder).add("/") }> */
		func() bool {
			{
				add(ruleAction14, position)
			}
			return true
		},
		/* 86 Action15 <- <{ p.consume(String(unquote(typeString, text))) }> */
		func() bool {
			{
				add(ruleAction15, position)
			}
			return true
		},
		/* 87 Action16 <- <{ p.sign = 1 }> */
		func() bool {
			{
				add(ruleAction16, position)
			}
			return true
		},
		/* 88 Action17 <- <{
			var x big.Rat
			if _, ok := x.SetString(signedText(text, p.sign)); !ok {
				p.err = fmt.Errorf("malformed rational: %q", text)
				return
			}
			p.consume(Rational{&x})
		}> */
		func() bool {
			{
				add(ruleAction17, position)
			}
			return true
		},
		/* 89 Action18 <- <{
			var x big.Float
			if _, ok := x.SetString(signedText(text, p.sign)); !ok {
				p.err = fmt.Errorf("malformed decimal: %q", text)
				return
			}
			p.consume(Decimal{&x})
		}> */
		func() bool {
			{
				add(ruleAction18, position)
			}
			return true
		},
		/* 90 Action19 <- <{
			var x big.Float
			if _, ok := x.SetString(signedText(text, p.sign)); !ok {
				p.err = fmt.Errorf("malformed decimal: %q", text)
				return
			}
			p.consume(Decimal{&x})
		}> */
		func() bool {
			{
				add(ruleAction19, position)
			}
			return true
		},
		/* 91 Action20 <- <{
			var x big.Int
			if _, ok := x.SetString(signedText(text, p.sign), 10); !ok {
				p.err = fmt.Errorf("malformed integer: %q", text)
				return
			}
			p.consume(Integer{&x})
		}> */
		func() bool {
			{
				add(ruleAction20, position)
			}
			return true
		},
		/* 92 Action21 <- <{
			if p.baseIntBase, p.err = strconv.Atoi(text); p.err != nil {
				p.err = fmt.Errorf("invalid integer base %q: %v", text, p.err)
				return
			}
		}> */
		func() bool {
			{
				add(ruleAction21, position)
			}
			return true
		},
		/* 93 Action22 <- <{
			var x big.Int
			if _, ok := x.SetString(signedText(text, p.sign), p.baseIntBase); !ok {
				p.err = fmt.Errorf("malformed base-%d integer %q", p.baseIntBase, text)
				return
			}

			p.consume(Integer{&x})
		}> */
		func() bool {
			{
				add(ruleAction22, position)
			}
			return true
		},
		/* 94 Action23 <- <{
			var x big.Int
			if _, ok := x.SetString(signedText(text, p.sign), 16); !ok {
				p.err = fmt.Errorf("malformed integer: %q", text)
				return
			}
			p.consume(Integer{&x})
		}> */
		func() bool {
			{
				add(ruleAction23, position)
			}
			return true
		},
		/* 95 Action24 <- <{
			var x big.Int
			if _, ok := x.SetString(signedText(text, p.sign), 8); !ok {
				p.err = fmt.Errorf("malformed integer: %q", text)
				return
			}
			p.consume(Integer{&x})
		}> */
		func() bool {
			{
				add(ruleAction24, position)
			}
			return true
		},
		/* 96 Action25 <- <{
			var x big.Int
			if _, ok := x.SetString(signedText(text, p.sign), 2); !ok {
				p.err = fmt.Errorf("malformed integer: %q", text)
				return
			}
			p.consume(Integer{&x})
		}> */
		func() bool {
			{
				add(ruleAction25, position)
			}
			return true
		},
		/* 97 Action26 <- <{ p.sign *= -1 }> */
		func() bool {
			{
				add(ruleAction26, position)
			}
			return true
		},
		/* 98 Action27 <- <{ p.sign = 1 }> */
		func() bool {
			{
				add(ruleAction27, position)
			}
			return true
		},
		/* 99 Action28 <- <{ p.consume(Bool(true)) }> */
		func() bool {
			{
				add(ruleAction28, position)
			}
			return true
		},
		/* 100 Action29 <- <{ p.consume(Bool(false)) }> */
		func() bool {
			{
				add(ruleAction29, position)
			}
			return true
		},
		/* 101 Action30 <- <{ p.consume(Nil) }> */
		func() bool {
			{
				add(ruleAction30, position)
			}
			return true
		},
	}
	p.rules = _rules
}
