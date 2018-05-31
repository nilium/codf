package codf

//go:generate peg codf.peg

import (
	"fmt"
	"math"
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
	ruleStatement
	ruleSection
	ruleName
	ruleExpr
	ruleExprList
	ruleLiteral
	ruleArray
	ruleMap
	ruleMapPairs
	ruleMapPair
	ruleMapKey
	ruleMapValue
	ruleSymbol
	ruleBareword
	ruleBarewordInitial
	ruleBarewordTail
	ruleRegexp
	ruleregexpBody
	ruleregexpTail
	ruleregexpEscape
	ruleString
	ruleQuotedString
	ruleStringBody
	ruleEscape
	ruleOctEscape
	ruleHexEscape
	ruleUnicodeShortEscape
	ruleUnicodeWideEscape
	ruleNumber
	ruleOctDigit
	ruleHexDigit
	ruleHexByte
	ruleRational
	ruleDecimal
	rulePointDecimal
	ruleExpDecimal
	ruleExponent
	ruleInteger
	ruleInt
	rulePosInt
	ruleDecInt
	ruleBaseInt
	ruleNumBase
	ruleNumLit
	ruleHexLit
	ruleOctLit
	ruleBinLit
	ruleSign
	ruleBoolean
	ruleTrueKw
	ruleFalseKw
	ruleTrue
	ruleFalse
	ruleSentinel
	ruleSpace
	ruleReqSpace
	ruleOptSpace
	ruleComment
	ruleEOL
	ruleEOF
	ruleAction0
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
	rulePegText
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
)

var rul3s = [...]string{
	"Unknown",
	"Root",
	"Body",
	"Decl",
	"Statement",
	"Section",
	"Name",
	"Expr",
	"ExprList",
	"Literal",
	"Array",
	"Map",
	"MapPairs",
	"MapPair",
	"MapKey",
	"MapValue",
	"Symbol",
	"Bareword",
	"BarewordInitial",
	"BarewordTail",
	"Regexp",
	"regexpBody",
	"regexpTail",
	"regexpEscape",
	"String",
	"QuotedString",
	"StringBody",
	"Escape",
	"OctEscape",
	"HexEscape",
	"UnicodeShortEscape",
	"UnicodeWideEscape",
	"Number",
	"OctDigit",
	"HexDigit",
	"HexByte",
	"Rational",
	"Decimal",
	"PointDecimal",
	"ExpDecimal",
	"Exponent",
	"Integer",
	"Int",
	"PosInt",
	"DecInt",
	"BaseInt",
	"NumBase",
	"NumLit",
	"HexLit",
	"OctLit",
	"BinLit",
	"Sign",
	"Boolean",
	"TrueKw",
	"FalseKw",
	"True",
	"False",
	"Sentinel",
	"Space",
	"ReqSpace",
	"OptSpace",
	"Comment",
	"EOL",
	"EOF",
	"Action0",
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
	"PegText",
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

	// Float precision
	Precision uint

	// stages []stage
	root *Root

	sign        int
	consumers   []consumer
	baseIntBase int

	Buffer string
	buffer []rune
	rules  [95]func() bool
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
			p.init()
		case ruleAction1:
			p.beginStatement(text)
		case ruleAction2:
			p.closeStatement()
		case ruleAction3:
			p.beginSection(text)
		case ruleAction4:
			p.closeSection()
		case ruleAction5:
			p.sign = 1
		case ruleAction6:
			p.beginArray()
		case ruleAction7:
			p.closeArray()
		case ruleAction8:
			p.beginMap()
		case ruleAction9:
			p.closeMap()
		case ruleAction10:
			p.consume(Symbol(text))
		case ruleAction11:
			p.consume(Symbol(unquote(typeSymbol, text)))
		case ruleAction12:
			p.beginRegexp()
		case ruleAction13:
			p.closeRegexp()
		case ruleAction14:
			p.tip().(*regexpBuilder).add(text)
		case ruleAction15:
			p.tip().(*regexpBuilder).add("/")
		case ruleAction16:
			p.consume(String(unquote(typeString, text)))
		case ruleAction17:
			p.consumeRational(text)
		case ruleAction18:
			p.consumeFloat(text)
		case ruleAction19:
			p.consumeFloat(text)
		case ruleAction20:
			p.consumeInteger(text, 10)
		case ruleAction21:
			p.parseBase(text)
		case ruleAction22:
			p.consumeInteger(text, p.baseIntBase)
		case ruleAction23:
			p.consumeInteger(text, 16)
		case ruleAction24:
			p.consumeInteger(text, 8)
		case ruleAction25:
			p.consumeInteger(text, 2)
		case ruleAction26:
			p.sign = -1
		case ruleAction27:
			p.sign = 1
		case ruleAction28:
			p.consume(Bool(true))
		case ruleAction29:
			p.consume(Bool(false))

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
		/* 0 Root <- <(Action0 Body* OptSpace EOF)> */
		func() bool {
			position0, tokenIndex0 := position, tokenIndex
			{
				position1 := position
				if !_rules[ruleAction0]() {
					goto l0
				}
			l2:
				{
					position3, tokenIndex3 := position, tokenIndex
					if !_rules[ruleBody]() {
						goto l3
					}
					goto l2
				l3:
					position, tokenIndex = position3, tokenIndex3
				}
				if !_rules[ruleOptSpace]() {
					goto l0
				}
				if !_rules[ruleEOF]() {
					goto l0
				}
				add(ruleRoot, position1)
			}
			return true
		l0:
			position, tokenIndex = position0, tokenIndex0
			return false
		},
		/* 1 Body <- <(OptSpace Decl)> */
		func() bool {
			position4, tokenIndex4 := position, tokenIndex
			{
				position5 := position
				if !_rules[ruleOptSpace]() {
					goto l4
				}
				if !_rules[ruleDecl]() {
					goto l4
				}
				add(ruleBody, position5)
			}
			return true
		l4:
			position, tokenIndex = position4, tokenIndex4
			return false
		},
		/* 2 Decl <- <(Section / Statement / Sentinel)> */
		func() bool {
			position6, tokenIndex6 := position, tokenIndex
			{
				position7 := position
				{
					position8, tokenIndex8 := position, tokenIndex
					if !_rules[ruleSection]() {
						goto l9
					}
					goto l8
				l9:
					position, tokenIndex = position8, tokenIndex8
					if !_rules[ruleStatement]() {
						goto l10
					}
					goto l8
				l10:
					position, tokenIndex = position8, tokenIndex8
					if !_rules[ruleSentinel]() {
						goto l6
					}
				}
			l8:
				add(ruleDecl, position7)
			}
			return true
		l6:
			position, tokenIndex = position6, tokenIndex6
			return false
		},
		/* 3 Statement <- <(Name Action1 (ReqSpace Expr)* OptSpace Sentinel Action2)> */
		func() bool {
			position11, tokenIndex11 := position, tokenIndex
			{
				position12 := position
				if !_rules[ruleName]() {
					goto l11
				}
				if !_rules[ruleAction1]() {
					goto l11
				}
			l13:
				{
					position14, tokenIndex14 := position, tokenIndex
					if !_rules[ruleReqSpace]() {
						goto l14
					}
					if !_rules[ruleExpr]() {
						goto l14
					}
					goto l13
				l14:
					position, tokenIndex = position14, tokenIndex14
				}
				if !_rules[ruleOptSpace]() {
					goto l11
				}
				if !_rules[ruleSentinel]() {
					goto l11
				}
				if !_rules[ruleAction2]() {
					goto l11
				}
				add(ruleStatement, position12)
			}
			return true
		l11:
			position, tokenIndex = position11, tokenIndex11
			return false
		},
		/* 4 Section <- <(Name Action3 (ReqSpace Expr)* OptSpace '{' Body* OptSpace '}' Action4)> */
		func() bool {
			position15, tokenIndex15 := position, tokenIndex
			{
				position16 := position
				if !_rules[ruleName]() {
					goto l15
				}
				if !_rules[ruleAction3]() {
					goto l15
				}
			l17:
				{
					position18, tokenIndex18 := position, tokenIndex
					if !_rules[ruleReqSpace]() {
						goto l18
					}
					if !_rules[ruleExpr]() {
						goto l18
					}
					goto l17
				l18:
					position, tokenIndex = position18, tokenIndex18
				}
				if !_rules[ruleOptSpace]() {
					goto l15
				}
				if buffer[position] != rune('{') {
					goto l15
				}
				position++
			l19:
				{
					position20, tokenIndex20 := position, tokenIndex
					if !_rules[ruleBody]() {
						goto l20
					}
					goto l19
				l20:
					position, tokenIndex = position20, tokenIndex20
				}
				if !_rules[ruleOptSpace]() {
					goto l15
				}
				if buffer[position] != rune('}') {
					goto l15
				}
				position++
				if !_rules[ruleAction4]() {
					goto l15
				}
				add(ruleSection, position16)
			}
			return true
		l15:
			position, tokenIndex = position15, tokenIndex15
			return false
		},
		/* 5 Name <- <Bareword> */
		func() bool {
			position21, tokenIndex21 := position, tokenIndex
			{
				position22 := position
				if !_rules[ruleBareword]() {
					goto l21
				}
				add(ruleName, position22)
			}
			return true
		l21:
			position, tokenIndex = position21, tokenIndex21
			return false
		},
		/* 6 Expr <- <Literal> */
		func() bool {
			position23, tokenIndex23 := position, tokenIndex
			{
				position24 := position
				if !_rules[ruleLiteral]() {
					goto l23
				}
				add(ruleExpr, position24)
			}
			return true
		l23:
			position, tokenIndex = position23, tokenIndex23
			return false
		},
		/* 7 ExprList <- <(Expr (ReqSpace Expr)*)> */
		func() bool {
			position25, tokenIndex25 := position, tokenIndex
			{
				position26 := position
				if !_rules[ruleExpr]() {
					goto l25
				}
			l27:
				{
					position28, tokenIndex28 := position, tokenIndex
					if !_rules[ruleReqSpace]() {
						goto l28
					}
					if !_rules[ruleExpr]() {
						goto l28
					}
					goto l27
				l28:
					position, tokenIndex = position28, tokenIndex28
				}
				add(ruleExprList, position26)
			}
			return true
		l25:
			position, tokenIndex = position25, tokenIndex25
			return false
		},
		/* 8 Literal <- <((Action5 Number) / Boolean / Regexp / String / Symbol / Map / Array)> */
		func() bool {
			position29, tokenIndex29 := position, tokenIndex
			{
				position30 := position
				{
					position31, tokenIndex31 := position, tokenIndex
					if !_rules[ruleAction5]() {
						goto l32
					}
					if !_rules[ruleNumber]() {
						goto l32
					}
					goto l31
				l32:
					position, tokenIndex = position31, tokenIndex31
					if !_rules[ruleBoolean]() {
						goto l33
					}
					goto l31
				l33:
					position, tokenIndex = position31, tokenIndex31
					if !_rules[ruleRegexp]() {
						goto l34
					}
					goto l31
				l34:
					position, tokenIndex = position31, tokenIndex31
					if !_rules[ruleString]() {
						goto l35
					}
					goto l31
				l35:
					position, tokenIndex = position31, tokenIndex31
					if !_rules[ruleSymbol]() {
						goto l36
					}
					goto l31
				l36:
					position, tokenIndex = position31, tokenIndex31
					if !_rules[ruleMap]() {
						goto l37
					}
					goto l31
				l37:
					position, tokenIndex = position31, tokenIndex31
					if !_rules[ruleArray]() {
						goto l29
					}
				}
			l31:
				add(ruleLiteral, position30)
			}
			return true
		l29:
			position, tokenIndex = position29, tokenIndex29
			return false
		},
		/* 9 Array <- <('[' OptSpace Action6 ExprList? OptSpace ']' Action7)> */
		func() bool {
			position38, tokenIndex38 := position, tokenIndex
			{
				position39 := position
				if buffer[position] != rune('[') {
					goto l38
				}
				position++
				if !_rules[ruleOptSpace]() {
					goto l38
				}
				if !_rules[ruleAction6]() {
					goto l38
				}
				{
					position40, tokenIndex40 := position, tokenIndex
					if !_rules[ruleExprList]() {
						goto l40
					}
					goto l41
				l40:
					position, tokenIndex = position40, tokenIndex40
				}
			l41:
				if !_rules[ruleOptSpace]() {
					goto l38
				}
				if buffer[position] != rune(']') {
					goto l38
				}
				position++
				if !_rules[ruleAction7]() {
					goto l38
				}
				add(ruleArray, position39)
			}
			return true
		l38:
			position, tokenIndex = position38, tokenIndex38
			return false
		},
		/* 10 Map <- <('#' '{' OptSpace Action8 MapPairs? OptSpace '}' Action9)> */
		func() bool {
			position42, tokenIndex42 := position, tokenIndex
			{
				position43 := position
				if buffer[position] != rune('#') {
					goto l42
				}
				position++
				if buffer[position] != rune('{') {
					goto l42
				}
				position++
				if !_rules[ruleOptSpace]() {
					goto l42
				}
				if !_rules[ruleAction8]() {
					goto l42
				}
				{
					position44, tokenIndex44 := position, tokenIndex
					if !_rules[ruleMapPairs]() {
						goto l44
					}
					goto l45
				l44:
					position, tokenIndex = position44, tokenIndex44
				}
			l45:
				if !_rules[ruleOptSpace]() {
					goto l42
				}
				if buffer[position] != rune('}') {
					goto l42
				}
				position++
				if !_rules[ruleAction9]() {
					goto l42
				}
				add(ruleMap, position43)
			}
			return true
		l42:
			position, tokenIndex = position42, tokenIndex42
			return false
		},
		/* 11 MapPairs <- <(MapPair (ReqSpace MapPair)*)> */
		func() bool {
			position46, tokenIndex46 := position, tokenIndex
			{
				position47 := position
				if !_rules[ruleMapPair]() {
					goto l46
				}
			l48:
				{
					position49, tokenIndex49 := position, tokenIndex
					if !_rules[ruleReqSpace]() {
						goto l49
					}
					if !_rules[ruleMapPair]() {
						goto l49
					}
					goto l48
				l49:
					position, tokenIndex = position49, tokenIndex49
				}
				add(ruleMapPairs, position47)
			}
			return true
		l46:
			position, tokenIndex = position46, tokenIndex46
			return false
		},
		/* 12 MapPair <- <(MapKey ReqSpace MapValue)> */
		func() bool {
			position50, tokenIndex50 := position, tokenIndex
			{
				position51 := position
				if !_rules[ruleMapKey]() {
					goto l50
				}
				if !_rules[ruleReqSpace]() {
					goto l50
				}
				if !_rules[ruleMapValue]() {
					goto l50
				}
				add(ruleMapPair, position51)
			}
			return true
		l50:
			position, tokenIndex = position50, tokenIndex50
			return false
		},
		/* 13 MapKey <- <(String / Symbol)> */
		func() bool {
			position52, tokenIndex52 := position, tokenIndex
			{
				position53 := position
				{
					position54, tokenIndex54 := position, tokenIndex
					if !_rules[ruleString]() {
						goto l55
					}
					goto l54
				l55:
					position, tokenIndex = position54, tokenIndex54
					if !_rules[ruleSymbol]() {
						goto l52
					}
				}
			l54:
				add(ruleMapKey, position53)
			}
			return true
		l52:
			position, tokenIndex = position52, tokenIndex52
			return false
		},
		/* 14 MapValue <- <Expr> */
		func() bool {
			position56, tokenIndex56 := position, tokenIndex
			{
				position57 := position
				if !_rules[ruleExpr]() {
					goto l56
				}
				add(ruleMapValue, position57)
			}
			return true
		l56:
			position, tokenIndex = position56, tokenIndex56
			return false
		},
		/* 15 Symbol <- <((Bareword Action10) / ('#' QuotedString Action11))> */
		func() bool {
			position58, tokenIndex58 := position, tokenIndex
			{
				position59 := position
				{
					position60, tokenIndex60 := position, tokenIndex
					if !_rules[ruleBareword]() {
						goto l61
					}
					if !_rules[ruleAction10]() {
						goto l61
					}
					goto l60
				l61:
					position, tokenIndex = position60, tokenIndex60
					if buffer[position] != rune('#') {
						goto l58
					}
					position++
					if !_rules[ruleQuotedString]() {
						goto l58
					}
					if !_rules[ruleAction11]() {
						goto l58
					}
				}
			l60:
				add(ruleSymbol, position59)
			}
			return true
		l58:
			position, tokenIndex = position58, tokenIndex58
			return false
		},
		/* 16 Bareword <- <<(BarewordInitial BarewordTail*)>> */
		func() bool {
			position62, tokenIndex62 := position, tokenIndex
			{
				position63 := position
				{
					position64 := position
					if !_rules[ruleBarewordInitial]() {
						goto l62
					}
				l65:
					{
						position66, tokenIndex66 := position, tokenIndex
						if !_rules[ruleBarewordTail]() {
							goto l66
						}
						goto l65
					l66:
						position, tokenIndex = position66, tokenIndex66
					}
					add(rulePegText, position64)
				}
				add(ruleBareword, position63)
			}
			return true
		l62:
			position, tokenIndex = position62, tokenIndex62
			return false
		},
		/* 17 BarewordInitial <- <('.' / '?' / '/' / '!' / '@' / '$' / '%' / '^' / '&' / '*' / '|' / '_' / ([a-z] / [A-Z]))> */
		func() bool {
			position67, tokenIndex67 := position, tokenIndex
			{
				position68 := position
				{
					position69, tokenIndex69 := position, tokenIndex
					if buffer[position] != rune('.') {
						goto l70
					}
					position++
					goto l69
				l70:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('?') {
						goto l71
					}
					position++
					goto l69
				l71:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('/') {
						goto l72
					}
					position++
					goto l69
				l72:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('!') {
						goto l73
					}
					position++
					goto l69
				l73:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('@') {
						goto l74
					}
					position++
					goto l69
				l74:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('$') {
						goto l75
					}
					position++
					goto l69
				l75:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('%') {
						goto l76
					}
					position++
					goto l69
				l76:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('^') {
						goto l77
					}
					position++
					goto l69
				l77:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('&') {
						goto l78
					}
					position++
					goto l69
				l78:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('*') {
						goto l79
					}
					position++
					goto l69
				l79:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('|') {
						goto l80
					}
					position++
					goto l69
				l80:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('_') {
						goto l81
					}
					position++
					goto l69
				l81:
					position, tokenIndex = position69, tokenIndex69
					{
						position82, tokenIndex82 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l83
						}
						position++
						goto l82
					l83:
						position, tokenIndex = position82, tokenIndex82
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l67
						}
						position++
					}
				l82:
				}
			l69:
				add(ruleBarewordInitial, position68)
			}
			return true
		l67:
			position, tokenIndex = position67, tokenIndex67
			return false
		},
		/* 18 BarewordTail <- <('-' / '+' / '=' / '<' / '>' / '.' / '?' / '/' / '!' / '@' / '$' / '%' / '^' / '&' / '*' / '#' / '|' / ':' / '_' / ([a-z] / [A-Z]) / ([0-9] / [0-9]))> */
		func() bool {
			position84, tokenIndex84 := position, tokenIndex
			{
				position85 := position
				{
					position86, tokenIndex86 := position, tokenIndex
					if buffer[position] != rune('-') {
						goto l87
					}
					position++
					goto l86
				l87:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('+') {
						goto l88
					}
					position++
					goto l86
				l88:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('=') {
						goto l89
					}
					position++
					goto l86
				l89:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('<') {
						goto l90
					}
					position++
					goto l86
				l90:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('>') {
						goto l91
					}
					position++
					goto l86
				l91:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('.') {
						goto l92
					}
					position++
					goto l86
				l92:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('?') {
						goto l93
					}
					position++
					goto l86
				l93:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('/') {
						goto l94
					}
					position++
					goto l86
				l94:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('!') {
						goto l95
					}
					position++
					goto l86
				l95:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('@') {
						goto l96
					}
					position++
					goto l86
				l96:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('$') {
						goto l97
					}
					position++
					goto l86
				l97:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('%') {
						goto l98
					}
					position++
					goto l86
				l98:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('^') {
						goto l99
					}
					position++
					goto l86
				l99:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('&') {
						goto l100
					}
					position++
					goto l86
				l100:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('*') {
						goto l101
					}
					position++
					goto l86
				l101:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('#') {
						goto l102
					}
					position++
					goto l86
				l102:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('|') {
						goto l103
					}
					position++
					goto l86
				l103:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune(':') {
						goto l104
					}
					position++
					goto l86
				l104:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('_') {
						goto l105
					}
					position++
					goto l86
				l105:
					position, tokenIndex = position86, tokenIndex86
					{
						position107, tokenIndex107 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l108
						}
						position++
						goto l107
					l108:
						position, tokenIndex = position107, tokenIndex107
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l106
						}
						position++
					}
				l107:
					goto l86
				l106:
					position, tokenIndex = position86, tokenIndex86
					{
						position109, tokenIndex109 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l110
						}
						position++
						goto l109
					l110:
						position, tokenIndex = position109, tokenIndex109
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l84
						}
						position++
					}
				l109:
				}
			l86:
				add(ruleBarewordTail, position85)
			}
			return true
		l84:
			position, tokenIndex = position84, tokenIndex84
			return false
		},
		/* 19 Regexp <- <('#' '/' Action12 regexpBody* '/' Action13)> */
		func() bool {
			position111, tokenIndex111 := position, tokenIndex
			{
				position112 := position
				if buffer[position] != rune('#') {
					goto l111
				}
				position++
				if buffer[position] != rune('/') {
					goto l111
				}
				position++
				if !_rules[ruleAction12]() {
					goto l111
				}
			l113:
				{
					position114, tokenIndex114 := position, tokenIndex
					if !_rules[ruleregexpBody]() {
						goto l114
					}
					goto l113
				l114:
					position, tokenIndex = position114, tokenIndex114
				}
				if buffer[position] != rune('/') {
					goto l111
				}
				position++
				if !_rules[ruleAction13]() {
					goto l111
				}
				add(ruleRegexp, position112)
			}
			return true
		l111:
			position, tokenIndex = position111, tokenIndex111
			return false
		},
		/* 20 regexpBody <- <(regexpEscape / regexpTail)> */
		func() bool {
			position115, tokenIndex115 := position, tokenIndex
			{
				position116 := position
				{
					position117, tokenIndex117 := position, tokenIndex
					if !_rules[ruleregexpEscape]() {
						goto l118
					}
					goto l117
				l118:
					position, tokenIndex = position117, tokenIndex117
					if !_rules[ruleregexpTail]() {
						goto l115
					}
				}
			l117:
				add(ruleregexpBody, position116)
			}
			return true
		l115:
			position, tokenIndex = position115, tokenIndex115
			return false
		},
		/* 21 regexpTail <- <(<(!('\\' / '/') .)+> Action14)> */
		func() bool {
			position119, tokenIndex119 := position, tokenIndex
			{
				position120 := position
				{
					position121 := position
					{
						position124, tokenIndex124 := position, tokenIndex
						{
							position125, tokenIndex125 := position, tokenIndex
							if buffer[position] != rune('\\') {
								goto l126
							}
							position++
							goto l125
						l126:
							position, tokenIndex = position125, tokenIndex125
							if buffer[position] != rune('/') {
								goto l124
							}
							position++
						}
					l125:
						goto l119
					l124:
						position, tokenIndex = position124, tokenIndex124
					}
					if !matchDot() {
						goto l119
					}
				l122:
					{
						position123, tokenIndex123 := position, tokenIndex
						{
							position127, tokenIndex127 := position, tokenIndex
							{
								position128, tokenIndex128 := position, tokenIndex
								if buffer[position] != rune('\\') {
									goto l129
								}
								position++
								goto l128
							l129:
								position, tokenIndex = position128, tokenIndex128
								if buffer[position] != rune('/') {
									goto l127
								}
								position++
							}
						l128:
							goto l123
						l127:
							position, tokenIndex = position127, tokenIndex127
						}
						if !matchDot() {
							goto l123
						}
						goto l122
					l123:
						position, tokenIndex = position123, tokenIndex123
					}
					add(rulePegText, position121)
				}
				if !_rules[ruleAction14]() {
					goto l119
				}
				add(ruleregexpTail, position120)
			}
			return true
		l119:
			position, tokenIndex = position119, tokenIndex119
			return false
		},
		/* 22 regexpEscape <- <('\\' '/' Action15)> */
		func() bool {
			position130, tokenIndex130 := position, tokenIndex
			{
				position131 := position
				if buffer[position] != rune('\\') {
					goto l130
				}
				position++
				if buffer[position] != rune('/') {
					goto l130
				}
				position++
				if !_rules[ruleAction15]() {
					goto l130
				}
				add(ruleregexpEscape, position131)
			}
			return true
		l130:
			position, tokenIndex = position130, tokenIndex130
			return false
		},
		/* 23 String <- <(QuotedString Action16)> */
		func() bool {
			position132, tokenIndex132 := position, tokenIndex
			{
				position133 := position
				if !_rules[ruleQuotedString]() {
					goto l132
				}
				if !_rules[ruleAction16]() {
					goto l132
				}
				add(ruleString, position133)
			}
			return true
		l132:
			position, tokenIndex = position132, tokenIndex132
			return false
		},
		/* 24 QuotedString <- <<('"' StringBody* '"')>> */
		func() bool {
			position134, tokenIndex134 := position, tokenIndex
			{
				position135 := position
				{
					position136 := position
					if buffer[position] != rune('"') {
						goto l134
					}
					position++
				l137:
					{
						position138, tokenIndex138 := position, tokenIndex
						if !_rules[ruleStringBody]() {
							goto l138
						}
						goto l137
					l138:
						position, tokenIndex = position138, tokenIndex138
					}
					if buffer[position] != rune('"') {
						goto l134
					}
					position++
					add(rulePegText, position136)
				}
				add(ruleQuotedString, position135)
			}
			return true
		l134:
			position, tokenIndex = position134, tokenIndex134
			return false
		},
		/* 25 StringBody <- <(Escape / ('\\' '"') / (!'"' .))> */
		func() bool {
			position139, tokenIndex139 := position, tokenIndex
			{
				position140 := position
				{
					position141, tokenIndex141 := position, tokenIndex
					if !_rules[ruleEscape]() {
						goto l142
					}
					goto l141
				l142:
					position, tokenIndex = position141, tokenIndex141
					if buffer[position] != rune('\\') {
						goto l143
					}
					position++
					if buffer[position] != rune('"') {
						goto l143
					}
					position++
					goto l141
				l143:
					position, tokenIndex = position141, tokenIndex141
					{
						position144, tokenIndex144 := position, tokenIndex
						if buffer[position] != rune('"') {
							goto l144
						}
						position++
						goto l139
					l144:
						position, tokenIndex = position144, tokenIndex144
					}
					if !matchDot() {
						goto l139
					}
				}
			l141:
				add(ruleStringBody, position140)
			}
			return true
		l139:
			position, tokenIndex = position139, tokenIndex139
			return false
		},
		/* 26 Escape <- <(('\\' 'a') / ('\\' 'b') / ('\\' 'f') / ('\\' 'n') / ('\\' 'r') / ('\\' 't') / ('\\' 'v') / OctEscape / HexEscape / UnicodeShortEscape / UnicodeWideEscape)> */
		func() bool {
			position145, tokenIndex145 := position, tokenIndex
			{
				position146 := position
				{
					position147, tokenIndex147 := position, tokenIndex
					if buffer[position] != rune('\\') {
						goto l148
					}
					position++
					if buffer[position] != rune('a') {
						goto l148
					}
					position++
					goto l147
				l148:
					position, tokenIndex = position147, tokenIndex147
					if buffer[position] != rune('\\') {
						goto l149
					}
					position++
					if buffer[position] != rune('b') {
						goto l149
					}
					position++
					goto l147
				l149:
					position, tokenIndex = position147, tokenIndex147
					if buffer[position] != rune('\\') {
						goto l150
					}
					position++
					if buffer[position] != rune('f') {
						goto l150
					}
					position++
					goto l147
				l150:
					position, tokenIndex = position147, tokenIndex147
					if buffer[position] != rune('\\') {
						goto l151
					}
					position++
					if buffer[position] != rune('n') {
						goto l151
					}
					position++
					goto l147
				l151:
					position, tokenIndex = position147, tokenIndex147
					if buffer[position] != rune('\\') {
						goto l152
					}
					position++
					if buffer[position] != rune('r') {
						goto l152
					}
					position++
					goto l147
				l152:
					position, tokenIndex = position147, tokenIndex147
					if buffer[position] != rune('\\') {
						goto l153
					}
					position++
					if buffer[position] != rune('t') {
						goto l153
					}
					position++
					goto l147
				l153:
					position, tokenIndex = position147, tokenIndex147
					if buffer[position] != rune('\\') {
						goto l154
					}
					position++
					if buffer[position] != rune('v') {
						goto l154
					}
					position++
					goto l147
				l154:
					position, tokenIndex = position147, tokenIndex147
					if !_rules[ruleOctEscape]() {
						goto l155
					}
					goto l147
				l155:
					position, tokenIndex = position147, tokenIndex147
					if !_rules[ruleHexEscape]() {
						goto l156
					}
					goto l147
				l156:
					position, tokenIndex = position147, tokenIndex147
					if !_rules[ruleUnicodeShortEscape]() {
						goto l157
					}
					goto l147
				l157:
					position, tokenIndex = position147, tokenIndex147
					if !_rules[ruleUnicodeWideEscape]() {
						goto l145
					}
				}
			l147:
				add(ruleEscape, position146)
			}
			return true
		l145:
			position, tokenIndex = position145, tokenIndex145
			return false
		},
		/* 27 OctEscape <- <('\\' OctDigit OctDigit OctDigit)> */
		func() bool {
			position158, tokenIndex158 := position, tokenIndex
			{
				position159 := position
				if buffer[position] != rune('\\') {
					goto l158
				}
				position++
				if !_rules[ruleOctDigit]() {
					goto l158
				}
				if !_rules[ruleOctDigit]() {
					goto l158
				}
				if !_rules[ruleOctDigit]() {
					goto l158
				}
				add(ruleOctEscape, position159)
			}
			return true
		l158:
			position, tokenIndex = position158, tokenIndex158
			return false
		},
		/* 28 HexEscape <- <('\\' 'x' HexByte)> */
		func() bool {
			position160, tokenIndex160 := position, tokenIndex
			{
				position161 := position
				if buffer[position] != rune('\\') {
					goto l160
				}
				position++
				if buffer[position] != rune('x') {
					goto l160
				}
				position++
				if !_rules[ruleHexByte]() {
					goto l160
				}
				add(ruleHexEscape, position161)
			}
			return true
		l160:
			position, tokenIndex = position160, tokenIndex160
			return false
		},
		/* 29 UnicodeShortEscape <- <('\\' 'u' HexByte HexByte)> */
		func() bool {
			position162, tokenIndex162 := position, tokenIndex
			{
				position163 := position
				if buffer[position] != rune('\\') {
					goto l162
				}
				position++
				if buffer[position] != rune('u') {
					goto l162
				}
				position++
				if !_rules[ruleHexByte]() {
					goto l162
				}
				if !_rules[ruleHexByte]() {
					goto l162
				}
				add(ruleUnicodeShortEscape, position163)
			}
			return true
		l162:
			position, tokenIndex = position162, tokenIndex162
			return false
		},
		/* 30 UnicodeWideEscape <- <('\\' 'U' HexByte HexByte HexByte HexByte)> */
		func() bool {
			position164, tokenIndex164 := position, tokenIndex
			{
				position165 := position
				if buffer[position] != rune('\\') {
					goto l164
				}
				position++
				if buffer[position] != rune('U') {
					goto l164
				}
				position++
				if !_rules[ruleHexByte]() {
					goto l164
				}
				if !_rules[ruleHexByte]() {
					goto l164
				}
				if !_rules[ruleHexByte]() {
					goto l164
				}
				if !_rules[ruleHexByte]() {
					goto l164
				}
				add(ruleUnicodeWideEscape, position165)
			}
			return true
		l164:
			position, tokenIndex = position164, tokenIndex164
			return false
		},
		/* 31 Number <- <(Sign? (Decimal / Rational / Integer))> */
		func() bool {
			position166, tokenIndex166 := position, tokenIndex
			{
				position167 := position
				{
					position168, tokenIndex168 := position, tokenIndex
					if !_rules[ruleSign]() {
						goto l168
					}
					goto l169
				l168:
					position, tokenIndex = position168, tokenIndex168
				}
			l169:
				{
					position170, tokenIndex170 := position, tokenIndex
					if !_rules[ruleDecimal]() {
						goto l171
					}
					goto l170
				l171:
					position, tokenIndex = position170, tokenIndex170
					if !_rules[ruleRational]() {
						goto l172
					}
					goto l170
				l172:
					position, tokenIndex = position170, tokenIndex170
					if !_rules[ruleInteger]() {
						goto l166
					}
				}
			l170:
				add(ruleNumber, position167)
			}
			return true
		l166:
			position, tokenIndex = position166, tokenIndex166
			return false
		},
		/* 32 OctDigit <- <[0-8]> */
		func() bool {
			position173, tokenIndex173 := position, tokenIndex
			{
				position174 := position
				if c := buffer[position]; c < rune('0') || c > rune('8') {
					goto l173
				}
				position++
				add(ruleOctDigit, position174)
			}
			return true
		l173:
			position, tokenIndex = position173, tokenIndex173
			return false
		},
		/* 33 HexDigit <- <([0-9] / [0-9] / ([a-f] / [A-F]))> */
		func() bool {
			position175, tokenIndex175 := position, tokenIndex
			{
				position176 := position
				{
					position177, tokenIndex177 := position, tokenIndex
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l178
					}
					position++
					goto l177
				l178:
					position, tokenIndex = position177, tokenIndex177
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l179
					}
					position++
					goto l177
				l179:
					position, tokenIndex = position177, tokenIndex177
					{
						position180, tokenIndex180 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('f') {
							goto l181
						}
						position++
						goto l180
					l181:
						position, tokenIndex = position180, tokenIndex180
						if c := buffer[position]; c < rune('A') || c > rune('F') {
							goto l175
						}
						position++
					}
				l180:
				}
			l177:
				add(ruleHexDigit, position176)
			}
			return true
		l175:
			position, tokenIndex = position175, tokenIndex175
			return false
		},
		/* 34 HexByte <- <(HexDigit HexDigit)> */
		func() bool {
			position182, tokenIndex182 := position, tokenIndex
			{
				position183 := position
				if !_rules[ruleHexDigit]() {
					goto l182
				}
				if !_rules[ruleHexDigit]() {
					goto l182
				}
				add(ruleHexByte, position183)
			}
			return true
		l182:
			position, tokenIndex = position182, tokenIndex182
			return false
		},
		/* 35 Rational <- <(<(Int '/' PosInt)> Action17)> */
		func() bool {
			position184, tokenIndex184 := position, tokenIndex
			{
				position185 := position
				{
					position186 := position
					if !_rules[ruleInt]() {
						goto l184
					}
					if buffer[position] != rune('/') {
						goto l184
					}
					position++
					if !_rules[rulePosInt]() {
						goto l184
					}
					add(rulePegText, position186)
				}
				if !_rules[ruleAction17]() {
					goto l184
				}
				add(ruleRational, position185)
			}
			return true
		l184:
			position, tokenIndex = position184, tokenIndex184
			return false
		},
		/* 36 Decimal <- <((PointDecimal Action18) / (ExpDecimal Action19))> */
		func() bool {
			position187, tokenIndex187 := position, tokenIndex
			{
				position188 := position
				{
					position189, tokenIndex189 := position, tokenIndex
					if !_rules[rulePointDecimal]() {
						goto l190
					}
					if !_rules[ruleAction18]() {
						goto l190
					}
					goto l189
				l190:
					position, tokenIndex = position189, tokenIndex189
					if !_rules[ruleExpDecimal]() {
						goto l187
					}
					if !_rules[ruleAction19]() {
						goto l187
					}
				}
			l189:
				add(ruleDecimal, position188)
			}
			return true
		l187:
			position, tokenIndex = position187, tokenIndex187
			return false
		},
		/* 37 PointDecimal <- <<(Int '.' Int Exponent?)>> */
		func() bool {
			position191, tokenIndex191 := position, tokenIndex
			{
				position192 := position
				{
					position193 := position
					if !_rules[ruleInt]() {
						goto l191
					}
					if buffer[position] != rune('.') {
						goto l191
					}
					position++
					if !_rules[ruleInt]() {
						goto l191
					}
					{
						position194, tokenIndex194 := position, tokenIndex
						if !_rules[ruleExponent]() {
							goto l194
						}
						goto l195
					l194:
						position, tokenIndex = position194, tokenIndex194
					}
				l195:
					add(rulePegText, position193)
				}
				add(rulePointDecimal, position192)
			}
			return true
		l191:
			position, tokenIndex = position191, tokenIndex191
			return false
		},
		/* 38 ExpDecimal <- <<(Int Exponent)>> */
		func() bool {
			position196, tokenIndex196 := position, tokenIndex
			{
				position197 := position
				{
					position198 := position
					if !_rules[ruleInt]() {
						goto l196
					}
					if !_rules[ruleExponent]() {
						goto l196
					}
					add(rulePegText, position198)
				}
				add(ruleExpDecimal, position197)
			}
			return true
		l196:
			position, tokenIndex = position196, tokenIndex196
			return false
		},
		/* 39 Exponent <- <(('E' / 'e') ('-' / '+')? Int)> */
		func() bool {
			position199, tokenIndex199 := position, tokenIndex
			{
				position200 := position
				{
					position201, tokenIndex201 := position, tokenIndex
					if buffer[position] != rune('E') {
						goto l202
					}
					position++
					goto l201
				l202:
					position, tokenIndex = position201, tokenIndex201
					if buffer[position] != rune('e') {
						goto l199
					}
					position++
				}
			l201:
				{
					position203, tokenIndex203 := position, tokenIndex
					{
						position205, tokenIndex205 := position, tokenIndex
						if buffer[position] != rune('-') {
							goto l206
						}
						position++
						goto l205
					l206:
						position, tokenIndex = position205, tokenIndex205
						if buffer[position] != rune('+') {
							goto l203
						}
						position++
					}
				l205:
					goto l204
				l203:
					position, tokenIndex = position203, tokenIndex203
				}
			l204:
				if !_rules[ruleInt]() {
					goto l199
				}
				add(ruleExponent, position200)
			}
			return true
		l199:
			position, tokenIndex = position199, tokenIndex199
			return false
		},
		/* 40 Integer <- <(BaseInt / HexLit / BinLit / OctLit / DecInt)> */
		func() bool {
			position207, tokenIndex207 := position, tokenIndex
			{
				position208 := position
				{
					position209, tokenIndex209 := position, tokenIndex
					if !_rules[ruleBaseInt]() {
						goto l210
					}
					goto l209
				l210:
					position, tokenIndex = position209, tokenIndex209
					if !_rules[ruleHexLit]() {
						goto l211
					}
					goto l209
				l211:
					position, tokenIndex = position209, tokenIndex209
					if !_rules[ruleBinLit]() {
						goto l212
					}
					goto l209
				l212:
					position, tokenIndex = position209, tokenIndex209
					if !_rules[ruleOctLit]() {
						goto l213
					}
					goto l209
				l213:
					position, tokenIndex = position209, tokenIndex209
					if !_rules[ruleDecInt]() {
						goto l207
					}
				}
			l209:
				add(ruleInteger, position208)
			}
			return true
		l207:
			position, tokenIndex = position207, tokenIndex207
			return false
		},
		/* 41 Int <- <('0' / PosInt)> */
		func() bool {
			position214, tokenIndex214 := position, tokenIndex
			{
				position215 := position
				{
					position216, tokenIndex216 := position, tokenIndex
					if buffer[position] != rune('0') {
						goto l217
					}
					position++
					goto l216
				l217:
					position, tokenIndex = position216, tokenIndex216
					if !_rules[rulePosInt]() {
						goto l214
					}
				}
			l216:
				add(ruleInt, position215)
			}
			return true
		l214:
			position, tokenIndex = position214, tokenIndex214
			return false
		},
		/* 42 PosInt <- <([1-9] [0-9]*)> */
		func() bool {
			position218, tokenIndex218 := position, tokenIndex
			{
				position219 := position
				if c := buffer[position]; c < rune('1') || c > rune('9') {
					goto l218
				}
				position++
			l220:
				{
					position221, tokenIndex221 := position, tokenIndex
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l221
					}
					position++
					goto l220
				l221:
					position, tokenIndex = position221, tokenIndex221
				}
				add(rulePosInt, position219)
			}
			return true
		l218:
			position, tokenIndex = position218, tokenIndex218
			return false
		},
		/* 43 DecInt <- <(<Int> Action20)> */
		func() bool {
			position222, tokenIndex222 := position, tokenIndex
			{
				position223 := position
				{
					position224 := position
					if !_rules[ruleInt]() {
						goto l222
					}
					add(rulePegText, position224)
				}
				if !_rules[ruleAction20]() {
					goto l222
				}
				add(ruleDecInt, position223)
			}
			return true
		l222:
			position, tokenIndex = position222, tokenIndex222
			return false
		},
		/* 44 BaseInt <- <(NumBase Action21 '#' NumLit Action22)> */
		func() bool {
			position225, tokenIndex225 := position, tokenIndex
			{
				position226 := position
				if !_rules[ruleNumBase]() {
					goto l225
				}
				if !_rules[ruleAction21]() {
					goto l225
				}
				if buffer[position] != rune('#') {
					goto l225
				}
				position++
				if !_rules[ruleNumLit]() {
					goto l225
				}
				if !_rules[ruleAction22]() {
					goto l225
				}
				add(ruleBaseInt, position226)
			}
			return true
		l225:
			position, tokenIndex = position225, tokenIndex225
			return false
		},
		/* 45 NumBase <- <<((('1' / '2') [0-9]) / ('3' [0-6]) / ([2-9] ![0-9]))>> */
		func() bool {
			position227, tokenIndex227 := position, tokenIndex
			{
				position228 := position
				{
					position229 := position
					{
						position230, tokenIndex230 := position, tokenIndex
						{
							position232, tokenIndex232 := position, tokenIndex
							if buffer[position] != rune('1') {
								goto l233
							}
							position++
							goto l232
						l233:
							position, tokenIndex = position232, tokenIndex232
							if buffer[position] != rune('2') {
								goto l231
							}
							position++
						}
					l232:
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l231
						}
						position++
						goto l230
					l231:
						position, tokenIndex = position230, tokenIndex230
						if buffer[position] != rune('3') {
							goto l234
						}
						position++
						if c := buffer[position]; c < rune('0') || c > rune('6') {
							goto l234
						}
						position++
						goto l230
					l234:
						position, tokenIndex = position230, tokenIndex230
						if c := buffer[position]; c < rune('2') || c > rune('9') {
							goto l227
						}
						position++
						{
							position235, tokenIndex235 := position, tokenIndex
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l235
							}
							position++
							goto l227
						l235:
							position, tokenIndex = position235, tokenIndex235
						}
					}
				l230:
					add(rulePegText, position229)
				}
				add(ruleNumBase, position228)
			}
			return true
		l227:
			position, tokenIndex = position227, tokenIndex227
			return false
		},
		/* 46 NumLit <- <<([a-z] / [A-Z] / ([0-9] / [0-9]))+>> */
		func() bool {
			position236, tokenIndex236 := position, tokenIndex
			{
				position237 := position
				{
					position238 := position
					{
						position241, tokenIndex241 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l242
						}
						position++
						goto l241
					l242:
						position, tokenIndex = position241, tokenIndex241
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l243
						}
						position++
						goto l241
					l243:
						position, tokenIndex = position241, tokenIndex241
						{
							position244, tokenIndex244 := position, tokenIndex
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l245
							}
							position++
							goto l244
						l245:
							position, tokenIndex = position244, tokenIndex244
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l236
							}
							position++
						}
					l244:
					}
				l241:
				l239:
					{
						position240, tokenIndex240 := position, tokenIndex
						{
							position246, tokenIndex246 := position, tokenIndex
							if c := buffer[position]; c < rune('a') || c > rune('z') {
								goto l247
							}
							position++
							goto l246
						l247:
							position, tokenIndex = position246, tokenIndex246
							if c := buffer[position]; c < rune('A') || c > rune('Z') {
								goto l248
							}
							position++
							goto l246
						l248:
							position, tokenIndex = position246, tokenIndex246
							{
								position249, tokenIndex249 := position, tokenIndex
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l250
								}
								position++
								goto l249
							l250:
								position, tokenIndex = position249, tokenIndex249
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l240
								}
								position++
							}
						l249:
						}
					l246:
						goto l239
					l240:
						position, tokenIndex = position240, tokenIndex240
					}
					add(rulePegText, position238)
				}
				add(ruleNumLit, position237)
			}
			return true
		l236:
			position, tokenIndex = position236, tokenIndex236
			return false
		},
		/* 47 HexLit <- <('0' ('x' / 'X') <HexDigit+> Action23)> */
		func() bool {
			position251, tokenIndex251 := position, tokenIndex
			{
				position252 := position
				if buffer[position] != rune('0') {
					goto l251
				}
				position++
				{
					position253, tokenIndex253 := position, tokenIndex
					if buffer[position] != rune('x') {
						goto l254
					}
					position++
					goto l253
				l254:
					position, tokenIndex = position253, tokenIndex253
					if buffer[position] != rune('X') {
						goto l251
					}
					position++
				}
			l253:
				{
					position255 := position
					if !_rules[ruleHexDigit]() {
						goto l251
					}
				l256:
					{
						position257, tokenIndex257 := position, tokenIndex
						if !_rules[ruleHexDigit]() {
							goto l257
						}
						goto l256
					l257:
						position, tokenIndex = position257, tokenIndex257
					}
					add(rulePegText, position255)
				}
				if !_rules[ruleAction23]() {
					goto l251
				}
				add(ruleHexLit, position252)
			}
			return true
		l251:
			position, tokenIndex = position251, tokenIndex251
			return false
		},
		/* 48 OctLit <- <('0' <[0-7]+> ![8-9] Action24)> */
		func() bool {
			position258, tokenIndex258 := position, tokenIndex
			{
				position259 := position
				if buffer[position] != rune('0') {
					goto l258
				}
				position++
				{
					position260 := position
					if c := buffer[position]; c < rune('0') || c > rune('7') {
						goto l258
					}
					position++
				l261:
					{
						position262, tokenIndex262 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('7') {
							goto l262
						}
						position++
						goto l261
					l262:
						position, tokenIndex = position262, tokenIndex262
					}
					add(rulePegText, position260)
				}
				{
					position263, tokenIndex263 := position, tokenIndex
					if c := buffer[position]; c < rune('8') || c > rune('9') {
						goto l263
					}
					position++
					goto l258
				l263:
					position, tokenIndex = position263, tokenIndex263
				}
				if !_rules[ruleAction24]() {
					goto l258
				}
				add(ruleOctLit, position259)
			}
			return true
		l258:
			position, tokenIndex = position258, tokenIndex258
			return false
		},
		/* 49 BinLit <- <('0' ('b' / 'B') <('0' / '1')+> ![2-9] Action25)> */
		func() bool {
			position264, tokenIndex264 := position, tokenIndex
			{
				position265 := position
				if buffer[position] != rune('0') {
					goto l264
				}
				position++
				{
					position266, tokenIndex266 := position, tokenIndex
					if buffer[position] != rune('b') {
						goto l267
					}
					position++
					goto l266
				l267:
					position, tokenIndex = position266, tokenIndex266
					if buffer[position] != rune('B') {
						goto l264
					}
					position++
				}
			l266:
				{
					position268 := position
					{
						position271, tokenIndex271 := position, tokenIndex
						if buffer[position] != rune('0') {
							goto l272
						}
						position++
						goto l271
					l272:
						position, tokenIndex = position271, tokenIndex271
						if buffer[position] != rune('1') {
							goto l264
						}
						position++
					}
				l271:
				l269:
					{
						position270, tokenIndex270 := position, tokenIndex
						{
							position273, tokenIndex273 := position, tokenIndex
							if buffer[position] != rune('0') {
								goto l274
							}
							position++
							goto l273
						l274:
							position, tokenIndex = position273, tokenIndex273
							if buffer[position] != rune('1') {
								goto l270
							}
							position++
						}
					l273:
						goto l269
					l270:
						position, tokenIndex = position270, tokenIndex270
					}
					add(rulePegText, position268)
				}
				{
					position275, tokenIndex275 := position, tokenIndex
					if c := buffer[position]; c < rune('2') || c > rune('9') {
						goto l275
					}
					position++
					goto l264
				l275:
					position, tokenIndex = position275, tokenIndex275
				}
				if !_rules[ruleAction25]() {
					goto l264
				}
				add(ruleBinLit, position265)
			}
			return true
		l264:
			position, tokenIndex = position264, tokenIndex264
			return false
		},
		/* 50 Sign <- <(('-' Action26) / ('+' Action27))> */
		func() bool {
			position276, tokenIndex276 := position, tokenIndex
			{
				position277 := position
				{
					position278, tokenIndex278 := position, tokenIndex
					if buffer[position] != rune('-') {
						goto l279
					}
					position++
					if !_rules[ruleAction26]() {
						goto l279
					}
					goto l278
				l279:
					position, tokenIndex = position278, tokenIndex278
					if buffer[position] != rune('+') {
						goto l276
					}
					position++
					if !_rules[ruleAction27]() {
						goto l276
					}
				}
			l278:
				add(ruleSign, position277)
			}
			return true
		l276:
			position, tokenIndex = position276, tokenIndex276
			return false
		},
		/* 51 Boolean <- <((True Action28) / (False Action29))> */
		func() bool {
			position280, tokenIndex280 := position, tokenIndex
			{
				position281 := position
				{
					position282, tokenIndex282 := position, tokenIndex
					if !_rules[ruleTrue]() {
						goto l283
					}
					if !_rules[ruleAction28]() {
						goto l283
					}
					goto l282
				l283:
					position, tokenIndex = position282, tokenIndex282
					if !_rules[ruleFalse]() {
						goto l280
					}
					if !_rules[ruleAction29]() {
						goto l280
					}
				}
			l282:
				add(ruleBoolean, position281)
			}
			return true
		l280:
			position, tokenIndex = position280, tokenIndex280
			return false
		},
		/* 52 TrueKw <- <(('t' 'r' 'u' 'e') / ('T' 'r' 'u' 'e') / ('T' 'R' 'U' 'E') / ('y' 'e' 's') / ('Y' 'e' 's') / ('Y' 'E' 'S'))> */
		func() bool {
			position284, tokenIndex284 := position, tokenIndex
			{
				position285 := position
				{
					position286, tokenIndex286 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l287
					}
					position++
					if buffer[position] != rune('r') {
						goto l287
					}
					position++
					if buffer[position] != rune('u') {
						goto l287
					}
					position++
					if buffer[position] != rune('e') {
						goto l287
					}
					position++
					goto l286
				l287:
					position, tokenIndex = position286, tokenIndex286
					if buffer[position] != rune('T') {
						goto l288
					}
					position++
					if buffer[position] != rune('r') {
						goto l288
					}
					position++
					if buffer[position] != rune('u') {
						goto l288
					}
					position++
					if buffer[position] != rune('e') {
						goto l288
					}
					position++
					goto l286
				l288:
					position, tokenIndex = position286, tokenIndex286
					if buffer[position] != rune('T') {
						goto l289
					}
					position++
					if buffer[position] != rune('R') {
						goto l289
					}
					position++
					if buffer[position] != rune('U') {
						goto l289
					}
					position++
					if buffer[position] != rune('E') {
						goto l289
					}
					position++
					goto l286
				l289:
					position, tokenIndex = position286, tokenIndex286
					if buffer[position] != rune('y') {
						goto l290
					}
					position++
					if buffer[position] != rune('e') {
						goto l290
					}
					position++
					if buffer[position] != rune('s') {
						goto l290
					}
					position++
					goto l286
				l290:
					position, tokenIndex = position286, tokenIndex286
					if buffer[position] != rune('Y') {
						goto l291
					}
					position++
					if buffer[position] != rune('e') {
						goto l291
					}
					position++
					if buffer[position] != rune('s') {
						goto l291
					}
					position++
					goto l286
				l291:
					position, tokenIndex = position286, tokenIndex286
					if buffer[position] != rune('Y') {
						goto l284
					}
					position++
					if buffer[position] != rune('E') {
						goto l284
					}
					position++
					if buffer[position] != rune('S') {
						goto l284
					}
					position++
				}
			l286:
				add(ruleTrueKw, position285)
			}
			return true
		l284:
			position, tokenIndex = position284, tokenIndex284
			return false
		},
		/* 53 FalseKw <- <(('f' 'a' 'l' 's' 'e') / ('F' 'a' 'l' 's' 'e') / ('F' 'A' 'L' 'S' 'E') / ('n' 'o') / ('N' 'o') / ('N' 'O'))> */
		func() bool {
			position292, tokenIndex292 := position, tokenIndex
			{
				position293 := position
				{
					position294, tokenIndex294 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l295
					}
					position++
					if buffer[position] != rune('a') {
						goto l295
					}
					position++
					if buffer[position] != rune('l') {
						goto l295
					}
					position++
					if buffer[position] != rune('s') {
						goto l295
					}
					position++
					if buffer[position] != rune('e') {
						goto l295
					}
					position++
					goto l294
				l295:
					position, tokenIndex = position294, tokenIndex294
					if buffer[position] != rune('F') {
						goto l296
					}
					position++
					if buffer[position] != rune('a') {
						goto l296
					}
					position++
					if buffer[position] != rune('l') {
						goto l296
					}
					position++
					if buffer[position] != rune('s') {
						goto l296
					}
					position++
					if buffer[position] != rune('e') {
						goto l296
					}
					position++
					goto l294
				l296:
					position, tokenIndex = position294, tokenIndex294
					if buffer[position] != rune('F') {
						goto l297
					}
					position++
					if buffer[position] != rune('A') {
						goto l297
					}
					position++
					if buffer[position] != rune('L') {
						goto l297
					}
					position++
					if buffer[position] != rune('S') {
						goto l297
					}
					position++
					if buffer[position] != rune('E') {
						goto l297
					}
					position++
					goto l294
				l297:
					position, tokenIndex = position294, tokenIndex294
					if buffer[position] != rune('n') {
						goto l298
					}
					position++
					if buffer[position] != rune('o') {
						goto l298
					}
					position++
					goto l294
				l298:
					position, tokenIndex = position294, tokenIndex294
					if buffer[position] != rune('N') {
						goto l299
					}
					position++
					if buffer[position] != rune('o') {
						goto l299
					}
					position++
					goto l294
				l299:
					position, tokenIndex = position294, tokenIndex294
					if buffer[position] != rune('N') {
						goto l292
					}
					position++
					if buffer[position] != rune('O') {
						goto l292
					}
					position++
				}
			l294:
				add(ruleFalseKw, position293)
			}
			return true
		l292:
			position, tokenIndex = position292, tokenIndex292
			return false
		},
		/* 54 True <- <(TrueKw !BarewordTail)> */
		func() bool {
			position300, tokenIndex300 := position, tokenIndex
			{
				position301 := position
				if !_rules[ruleTrueKw]() {
					goto l300
				}
				{
					position302, tokenIndex302 := position, tokenIndex
					if !_rules[ruleBarewordTail]() {
						goto l302
					}
					goto l300
				l302:
					position, tokenIndex = position302, tokenIndex302
				}
				add(ruleTrue, position301)
			}
			return true
		l300:
			position, tokenIndex = position300, tokenIndex300
			return false
		},
		/* 55 False <- <(FalseKw !BarewordTail)> */
		func() bool {
			position303, tokenIndex303 := position, tokenIndex
			{
				position304 := position
				if !_rules[ruleFalseKw]() {
					goto l303
				}
				{
					position305, tokenIndex305 := position, tokenIndex
					if !_rules[ruleBarewordTail]() {
						goto l305
					}
					goto l303
				l305:
					position, tokenIndex = position305, tokenIndex305
				}
				add(ruleFalse, position304)
			}
			return true
		l303:
			position, tokenIndex = position303, tokenIndex303
			return false
		},
		/* 56 Sentinel <- <';'> */
		func() bool {
			position306, tokenIndex306 := position, tokenIndex
			{
				position307 := position
				if buffer[position] != rune(';') {
					goto l306
				}
				position++
				add(ruleSentinel, position307)
			}
			return true
		l306:
			position, tokenIndex = position306, tokenIndex306
			return false
		},
		/* 57 Space <- <((' ' / '\t' / '\n' / '\r')+ / Comment)> */
		func() bool {
			position308, tokenIndex308 := position, tokenIndex
			{
				position309 := position
				{
					position310, tokenIndex310 := position, tokenIndex
					{
						position314, tokenIndex314 := position, tokenIndex
						if buffer[position] != rune(' ') {
							goto l315
						}
						position++
						goto l314
					l315:
						position, tokenIndex = position314, tokenIndex314
						if buffer[position] != rune('\t') {
							goto l316
						}
						position++
						goto l314
					l316:
						position, tokenIndex = position314, tokenIndex314
						if buffer[position] != rune('\n') {
							goto l317
						}
						position++
						goto l314
					l317:
						position, tokenIndex = position314, tokenIndex314
						if buffer[position] != rune('\r') {
							goto l311
						}
						position++
					}
				l314:
				l312:
					{
						position313, tokenIndex313 := position, tokenIndex
						{
							position318, tokenIndex318 := position, tokenIndex
							if buffer[position] != rune(' ') {
								goto l319
							}
							position++
							goto l318
						l319:
							position, tokenIndex = position318, tokenIndex318
							if buffer[position] != rune('\t') {
								goto l320
							}
							position++
							goto l318
						l320:
							position, tokenIndex = position318, tokenIndex318
							if buffer[position] != rune('\n') {
								goto l321
							}
							position++
							goto l318
						l321:
							position, tokenIndex = position318, tokenIndex318
							if buffer[position] != rune('\r') {
								goto l313
							}
							position++
						}
					l318:
						goto l312
					l313:
						position, tokenIndex = position313, tokenIndex313
					}
					goto l310
				l311:
					position, tokenIndex = position310, tokenIndex310
					if !_rules[ruleComment]() {
						goto l308
					}
				}
			l310:
				add(ruleSpace, position309)
			}
			return true
		l308:
			position, tokenIndex = position308, tokenIndex308
			return false
		},
		/* 58 ReqSpace <- <Space+> */
		func() bool {
			position322, tokenIndex322 := position, tokenIndex
			{
				position323 := position
				if !_rules[ruleSpace]() {
					goto l322
				}
			l324:
				{
					position325, tokenIndex325 := position, tokenIndex
					if !_rules[ruleSpace]() {
						goto l325
					}
					goto l324
				l325:
					position, tokenIndex = position325, tokenIndex325
				}
				add(ruleReqSpace, position323)
			}
			return true
		l322:
			position, tokenIndex = position322, tokenIndex322
			return false
		},
		/* 59 OptSpace <- <Space*> */
		func() bool {
			{
				position327 := position
			l328:
				{
					position329, tokenIndex329 := position, tokenIndex
					if !_rules[ruleSpace]() {
						goto l329
					}
					goto l328
				l329:
					position, tokenIndex = position329, tokenIndex329
				}
				add(ruleOptSpace, position327)
			}
			return true
		},
		/* 60 Comment <- <('\'' (!EOL .)* (EOL / EOF))> */
		func() bool {
			position330, tokenIndex330 := position, tokenIndex
			{
				position331 := position
				if buffer[position] != rune('\'') {
					goto l330
				}
				position++
			l332:
				{
					position333, tokenIndex333 := position, tokenIndex
					{
						position334, tokenIndex334 := position, tokenIndex
						if !_rules[ruleEOL]() {
							goto l334
						}
						goto l333
					l334:
						position, tokenIndex = position334, tokenIndex334
					}
					if !matchDot() {
						goto l333
					}
					goto l332
				l333:
					position, tokenIndex = position333, tokenIndex333
				}
				{
					position335, tokenIndex335 := position, tokenIndex
					if !_rules[ruleEOL]() {
						goto l336
					}
					goto l335
				l336:
					position, tokenIndex = position335, tokenIndex335
					if !_rules[ruleEOF]() {
						goto l330
					}
				}
			l335:
				add(ruleComment, position331)
			}
			return true
		l330:
			position, tokenIndex = position330, tokenIndex330
			return false
		},
		/* 61 EOL <- <(('\r' '\n') / '\n' / '\r')> */
		func() bool {
			position337, tokenIndex337 := position, tokenIndex
			{
				position338 := position
				{
					position339, tokenIndex339 := position, tokenIndex
					if buffer[position] != rune('\r') {
						goto l340
					}
					position++
					if buffer[position] != rune('\n') {
						goto l340
					}
					position++
					goto l339
				l340:
					position, tokenIndex = position339, tokenIndex339
					if buffer[position] != rune('\n') {
						goto l341
					}
					position++
					goto l339
				l341:
					position, tokenIndex = position339, tokenIndex339
					if buffer[position] != rune('\r') {
						goto l337
					}
					position++
				}
			l339:
				add(ruleEOL, position338)
			}
			return true
		l337:
			position, tokenIndex = position337, tokenIndex337
			return false
		},
		/* 62 EOF <- <!.> */
		func() bool {
			position342, tokenIndex342 := position, tokenIndex
			{
				position343 := position
				{
					position344, tokenIndex344 := position, tokenIndex
					if !matchDot() {
						goto l344
					}
					goto l342
				l344:
					position, tokenIndex = position344, tokenIndex344
				}
				add(ruleEOF, position343)
			}
			return true
		l342:
			position, tokenIndex = position342, tokenIndex342
			return false
		},
		/* 64 Action0 <- <{ p.init() }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 65 Action1 <- <{ p.beginStatement(text) }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		/* 66 Action2 <- <{ p.closeStatement() }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
		/* 67 Action3 <- <{ p.beginSection(text) }> */
		func() bool {
			{
				add(ruleAction3, position)
			}
			return true
		},
		/* 68 Action4 <- <{ p.closeSection() }> */
		func() bool {
			{
				add(ruleAction4, position)
			}
			return true
		},
		/* 69 Action5 <- <{ p.sign = 1 }> */
		func() bool {
			{
				add(ruleAction5, position)
			}
			return true
		},
		/* 70 Action6 <- <{ p.beginArray() }> */
		func() bool {
			{
				add(ruleAction6, position)
			}
			return true
		},
		/* 71 Action7 <- <{ p.closeArray() }> */
		func() bool {
			{
				add(ruleAction7, position)
			}
			return true
		},
		/* 72 Action8 <- <{ p.beginMap() }> */
		func() bool {
			{
				add(ruleAction8, position)
			}
			return true
		},
		/* 73 Action9 <- <{ p.closeMap() }> */
		func() bool {
			{
				add(ruleAction9, position)
			}
			return true
		},
		/* 74 Action10 <- <{ p.consume(Symbol(text)) }> */
		func() bool {
			{
				add(ruleAction10, position)
			}
			return true
		},
		/* 75 Action11 <- <{ p.consume(Symbol(unquote(typeSymbol, text))) }> */
		func() bool {
			{
				add(ruleAction11, position)
			}
			return true
		},
		nil,
		/* 77 Action12 <- <{ p.beginRegexp() }> */
		func() bool {
			{
				add(ruleAction12, position)
			}
			return true
		},
		/* 78 Action13 <- <{ p.closeRegexp() }> */
		func() bool {
			{
				add(ruleAction13, position)
			}
			return true
		},
		/* 79 Action14 <- <{ p.tip().(*regexpBuilder).add(text) }> */
		func() bool {
			{
				add(ruleAction14, position)
			}
			return true
		},
		/* 80 Action15 <- <{ p.tip().(*regexpBuilder).add("/") }> */
		func() bool {
			{
				add(ruleAction15, position)
			}
			return true
		},
		/* 81 Action16 <- <{ p.consume(String(unquote(typeString, text))) }> */
		func() bool {
			{
				add(ruleAction16, position)
			}
			return true
		},
		/* 82 Action17 <- <{ p.consumeRational(text) }> */
		func() bool {
			{
				add(ruleAction17, position)
			}
			return true
		},
		/* 83 Action18 <- <{ p.consumeFloat(text) }> */
		func() bool {
			{
				add(ruleAction18, position)
			}
			return true
		},
		/* 84 Action19 <- <{ p.consumeFloat(text) }> */
		func() bool {
			{
				add(ruleAction19, position)
			}
			return true
		},
		/* 85 Action20 <- <{ p.consumeInteger(text, 10) }> */
		func() bool {
			{
				add(ruleAction20, position)
			}
			return true
		},
		/* 86 Action21 <- <{ p.parseBase(text) }> */
		func() bool {
			{
				add(ruleAction21, position)
			}
			return true
		},
		/* 87 Action22 <- <{ p.consumeInteger(text, p.baseIntBase) }> */
		func() bool {
			{
				add(ruleAction22, position)
			}
			return true
		},
		/* 88 Action23 <- <{ p.consumeInteger(text, 16) }> */
		func() bool {
			{
				add(ruleAction23, position)
			}
			return true
		},
		/* 89 Action24 <- <{ p.consumeInteger(text, 8) }> */
		func() bool {
			{
				add(ruleAction24, position)
			}
			return true
		},
		/* 90 Action25 <- <{ p.consumeInteger(text, 2) }> */
		func() bool {
			{
				add(ruleAction25, position)
			}
			return true
		},
		/* 91 Action26 <- <{ p.sign = -1 }> */
		func() bool {
			{
				add(ruleAction26, position)
			}
			return true
		},
		/* 92 Action27 <- <{ p.sign = 1 }> */
		func() bool {
			{
				add(ruleAction27, position)
			}
			return true
		},
		/* 93 Action28 <- <{ p.consume(Bool(true)) }> */
		func() bool {
			{
				add(ruleAction28, position)
			}
			return true
		},
		/* 94 Action29 <- <{ p.consume(Bool(false)) }> */
		func() bool {
			{
				add(ruleAction29, position)
			}
			return true
		},
	}
	p.rules = _rules
}
