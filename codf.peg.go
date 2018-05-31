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
	ruleNil
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
	ruleAction30
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
	"Nil",
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
	rules  [97]func() bool
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
		/* 8 Literal <- <((Action5 Number) / Boolean / Nil / Regexp / String / Symbol / Map / Array)> */
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
					if !_rules[ruleNil]() {
						goto l34
					}
					goto l31
				l34:
					position, tokenIndex = position31, tokenIndex31
					if !_rules[ruleRegexp]() {
						goto l35
					}
					goto l31
				l35:
					position, tokenIndex = position31, tokenIndex31
					if !_rules[ruleString]() {
						goto l36
					}
					goto l31
				l36:
					position, tokenIndex = position31, tokenIndex31
					if !_rules[ruleSymbol]() {
						goto l37
					}
					goto l31
				l37:
					position, tokenIndex = position31, tokenIndex31
					if !_rules[ruleMap]() {
						goto l38
					}
					goto l31
				l38:
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
			position39, tokenIndex39 := position, tokenIndex
			{
				position40 := position
				if buffer[position] != rune('[') {
					goto l39
				}
				position++
				if !_rules[ruleOptSpace]() {
					goto l39
				}
				if !_rules[ruleAction6]() {
					goto l39
				}
				{
					position41, tokenIndex41 := position, tokenIndex
					if !_rules[ruleExprList]() {
						goto l41
					}
					goto l42
				l41:
					position, tokenIndex = position41, tokenIndex41
				}
			l42:
				if !_rules[ruleOptSpace]() {
					goto l39
				}
				if buffer[position] != rune(']') {
					goto l39
				}
				position++
				if !_rules[ruleAction7]() {
					goto l39
				}
				add(ruleArray, position40)
			}
			return true
		l39:
			position, tokenIndex = position39, tokenIndex39
			return false
		},
		/* 10 Map <- <('#' '{' OptSpace Action8 MapPairs? OptSpace '}' Action9)> */
		func() bool {
			position43, tokenIndex43 := position, tokenIndex
			{
				position44 := position
				if buffer[position] != rune('#') {
					goto l43
				}
				position++
				if buffer[position] != rune('{') {
					goto l43
				}
				position++
				if !_rules[ruleOptSpace]() {
					goto l43
				}
				if !_rules[ruleAction8]() {
					goto l43
				}
				{
					position45, tokenIndex45 := position, tokenIndex
					if !_rules[ruleMapPairs]() {
						goto l45
					}
					goto l46
				l45:
					position, tokenIndex = position45, tokenIndex45
				}
			l46:
				if !_rules[ruleOptSpace]() {
					goto l43
				}
				if buffer[position] != rune('}') {
					goto l43
				}
				position++
				if !_rules[ruleAction9]() {
					goto l43
				}
				add(ruleMap, position44)
			}
			return true
		l43:
			position, tokenIndex = position43, tokenIndex43
			return false
		},
		/* 11 MapPairs <- <(MapPair (ReqSpace MapPair)*)> */
		func() bool {
			position47, tokenIndex47 := position, tokenIndex
			{
				position48 := position
				if !_rules[ruleMapPair]() {
					goto l47
				}
			l49:
				{
					position50, tokenIndex50 := position, tokenIndex
					if !_rules[ruleReqSpace]() {
						goto l50
					}
					if !_rules[ruleMapPair]() {
						goto l50
					}
					goto l49
				l50:
					position, tokenIndex = position50, tokenIndex50
				}
				add(ruleMapPairs, position48)
			}
			return true
		l47:
			position, tokenIndex = position47, tokenIndex47
			return false
		},
		/* 12 MapPair <- <(MapKey ReqSpace MapValue)> */
		func() bool {
			position51, tokenIndex51 := position, tokenIndex
			{
				position52 := position
				if !_rules[ruleMapKey]() {
					goto l51
				}
				if !_rules[ruleReqSpace]() {
					goto l51
				}
				if !_rules[ruleMapValue]() {
					goto l51
				}
				add(ruleMapPair, position52)
			}
			return true
		l51:
			position, tokenIndex = position51, tokenIndex51
			return false
		},
		/* 13 MapKey <- <(String / Symbol)> */
		func() bool {
			position53, tokenIndex53 := position, tokenIndex
			{
				position54 := position
				{
					position55, tokenIndex55 := position, tokenIndex
					if !_rules[ruleString]() {
						goto l56
					}
					goto l55
				l56:
					position, tokenIndex = position55, tokenIndex55
					if !_rules[ruleSymbol]() {
						goto l53
					}
				}
			l55:
				add(ruleMapKey, position54)
			}
			return true
		l53:
			position, tokenIndex = position53, tokenIndex53
			return false
		},
		/* 14 MapValue <- <Expr> */
		func() bool {
			position57, tokenIndex57 := position, tokenIndex
			{
				position58 := position
				if !_rules[ruleExpr]() {
					goto l57
				}
				add(ruleMapValue, position58)
			}
			return true
		l57:
			position, tokenIndex = position57, tokenIndex57
			return false
		},
		/* 15 Symbol <- <((Bareword Action10) / ('#' QuotedString Action11))> */
		func() bool {
			position59, tokenIndex59 := position, tokenIndex
			{
				position60 := position
				{
					position61, tokenIndex61 := position, tokenIndex
					if !_rules[ruleBareword]() {
						goto l62
					}
					if !_rules[ruleAction10]() {
						goto l62
					}
					goto l61
				l62:
					position, tokenIndex = position61, tokenIndex61
					if buffer[position] != rune('#') {
						goto l59
					}
					position++
					if !_rules[ruleQuotedString]() {
						goto l59
					}
					if !_rules[ruleAction11]() {
						goto l59
					}
				}
			l61:
				add(ruleSymbol, position60)
			}
			return true
		l59:
			position, tokenIndex = position59, tokenIndex59
			return false
		},
		/* 16 Bareword <- <<(BarewordInitial BarewordTail*)>> */
		func() bool {
			position63, tokenIndex63 := position, tokenIndex
			{
				position64 := position
				{
					position65 := position
					if !_rules[ruleBarewordInitial]() {
						goto l63
					}
				l66:
					{
						position67, tokenIndex67 := position, tokenIndex
						if !_rules[ruleBarewordTail]() {
							goto l67
						}
						goto l66
					l67:
						position, tokenIndex = position67, tokenIndex67
					}
					add(rulePegText, position65)
				}
				add(ruleBareword, position64)
			}
			return true
		l63:
			position, tokenIndex = position63, tokenIndex63
			return false
		},
		/* 17 BarewordInitial <- <('.' / '?' / '/' / '!' / '@' / '$' / '%' / '^' / '&' / '*' / '|' / '_' / ([a-z] / [A-Z]))> */
		func() bool {
			position68, tokenIndex68 := position, tokenIndex
			{
				position69 := position
				{
					position70, tokenIndex70 := position, tokenIndex
					if buffer[position] != rune('.') {
						goto l71
					}
					position++
					goto l70
				l71:
					position, tokenIndex = position70, tokenIndex70
					if buffer[position] != rune('?') {
						goto l72
					}
					position++
					goto l70
				l72:
					position, tokenIndex = position70, tokenIndex70
					if buffer[position] != rune('/') {
						goto l73
					}
					position++
					goto l70
				l73:
					position, tokenIndex = position70, tokenIndex70
					if buffer[position] != rune('!') {
						goto l74
					}
					position++
					goto l70
				l74:
					position, tokenIndex = position70, tokenIndex70
					if buffer[position] != rune('@') {
						goto l75
					}
					position++
					goto l70
				l75:
					position, tokenIndex = position70, tokenIndex70
					if buffer[position] != rune('$') {
						goto l76
					}
					position++
					goto l70
				l76:
					position, tokenIndex = position70, tokenIndex70
					if buffer[position] != rune('%') {
						goto l77
					}
					position++
					goto l70
				l77:
					position, tokenIndex = position70, tokenIndex70
					if buffer[position] != rune('^') {
						goto l78
					}
					position++
					goto l70
				l78:
					position, tokenIndex = position70, tokenIndex70
					if buffer[position] != rune('&') {
						goto l79
					}
					position++
					goto l70
				l79:
					position, tokenIndex = position70, tokenIndex70
					if buffer[position] != rune('*') {
						goto l80
					}
					position++
					goto l70
				l80:
					position, tokenIndex = position70, tokenIndex70
					if buffer[position] != rune('|') {
						goto l81
					}
					position++
					goto l70
				l81:
					position, tokenIndex = position70, tokenIndex70
					if buffer[position] != rune('_') {
						goto l82
					}
					position++
					goto l70
				l82:
					position, tokenIndex = position70, tokenIndex70
					{
						position83, tokenIndex83 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l84
						}
						position++
						goto l83
					l84:
						position, tokenIndex = position83, tokenIndex83
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l68
						}
						position++
					}
				l83:
				}
			l70:
				add(ruleBarewordInitial, position69)
			}
			return true
		l68:
			position, tokenIndex = position68, tokenIndex68
			return false
		},
		/* 18 BarewordTail <- <('-' / '+' / '=' / '<' / '>' / '.' / '?' / '/' / '!' / '@' / '$' / '%' / '^' / '&' / '*' / '#' / '|' / ':' / '_' / ([a-z] / [A-Z]) / ([0-9] / [0-9]))> */
		func() bool {
			position85, tokenIndex85 := position, tokenIndex
			{
				position86 := position
				{
					position87, tokenIndex87 := position, tokenIndex
					if buffer[position] != rune('-') {
						goto l88
					}
					position++
					goto l87
				l88:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('+') {
						goto l89
					}
					position++
					goto l87
				l89:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('=') {
						goto l90
					}
					position++
					goto l87
				l90:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('<') {
						goto l91
					}
					position++
					goto l87
				l91:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('>') {
						goto l92
					}
					position++
					goto l87
				l92:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('.') {
						goto l93
					}
					position++
					goto l87
				l93:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('?') {
						goto l94
					}
					position++
					goto l87
				l94:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('/') {
						goto l95
					}
					position++
					goto l87
				l95:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('!') {
						goto l96
					}
					position++
					goto l87
				l96:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('@') {
						goto l97
					}
					position++
					goto l87
				l97:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('$') {
						goto l98
					}
					position++
					goto l87
				l98:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('%') {
						goto l99
					}
					position++
					goto l87
				l99:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('^') {
						goto l100
					}
					position++
					goto l87
				l100:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('&') {
						goto l101
					}
					position++
					goto l87
				l101:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('*') {
						goto l102
					}
					position++
					goto l87
				l102:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('#') {
						goto l103
					}
					position++
					goto l87
				l103:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('|') {
						goto l104
					}
					position++
					goto l87
				l104:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune(':') {
						goto l105
					}
					position++
					goto l87
				l105:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('_') {
						goto l106
					}
					position++
					goto l87
				l106:
					position, tokenIndex = position87, tokenIndex87
					{
						position108, tokenIndex108 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l109
						}
						position++
						goto l108
					l109:
						position, tokenIndex = position108, tokenIndex108
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l107
						}
						position++
					}
				l108:
					goto l87
				l107:
					position, tokenIndex = position87, tokenIndex87
					{
						position110, tokenIndex110 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l111
						}
						position++
						goto l110
					l111:
						position, tokenIndex = position110, tokenIndex110
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l85
						}
						position++
					}
				l110:
				}
			l87:
				add(ruleBarewordTail, position86)
			}
			return true
		l85:
			position, tokenIndex = position85, tokenIndex85
			return false
		},
		/* 19 Regexp <- <('#' '/' Action12 regexpBody* '/' Action13)> */
		func() bool {
			position112, tokenIndex112 := position, tokenIndex
			{
				position113 := position
				if buffer[position] != rune('#') {
					goto l112
				}
				position++
				if buffer[position] != rune('/') {
					goto l112
				}
				position++
				if !_rules[ruleAction12]() {
					goto l112
				}
			l114:
				{
					position115, tokenIndex115 := position, tokenIndex
					if !_rules[ruleregexpBody]() {
						goto l115
					}
					goto l114
				l115:
					position, tokenIndex = position115, tokenIndex115
				}
				if buffer[position] != rune('/') {
					goto l112
				}
				position++
				if !_rules[ruleAction13]() {
					goto l112
				}
				add(ruleRegexp, position113)
			}
			return true
		l112:
			position, tokenIndex = position112, tokenIndex112
			return false
		},
		/* 20 regexpBody <- <(regexpEscape / regexpTail)> */
		func() bool {
			position116, tokenIndex116 := position, tokenIndex
			{
				position117 := position
				{
					position118, tokenIndex118 := position, tokenIndex
					if !_rules[ruleregexpEscape]() {
						goto l119
					}
					goto l118
				l119:
					position, tokenIndex = position118, tokenIndex118
					if !_rules[ruleregexpTail]() {
						goto l116
					}
				}
			l118:
				add(ruleregexpBody, position117)
			}
			return true
		l116:
			position, tokenIndex = position116, tokenIndex116
			return false
		},
		/* 21 regexpTail <- <(<(!('\\' / '/') .)+> Action14)> */
		func() bool {
			position120, tokenIndex120 := position, tokenIndex
			{
				position121 := position
				{
					position122 := position
					{
						position125, tokenIndex125 := position, tokenIndex
						{
							position126, tokenIndex126 := position, tokenIndex
							if buffer[position] != rune('\\') {
								goto l127
							}
							position++
							goto l126
						l127:
							position, tokenIndex = position126, tokenIndex126
							if buffer[position] != rune('/') {
								goto l125
							}
							position++
						}
					l126:
						goto l120
					l125:
						position, tokenIndex = position125, tokenIndex125
					}
					if !matchDot() {
						goto l120
					}
				l123:
					{
						position124, tokenIndex124 := position, tokenIndex
						{
							position128, tokenIndex128 := position, tokenIndex
							{
								position129, tokenIndex129 := position, tokenIndex
								if buffer[position] != rune('\\') {
									goto l130
								}
								position++
								goto l129
							l130:
								position, tokenIndex = position129, tokenIndex129
								if buffer[position] != rune('/') {
									goto l128
								}
								position++
							}
						l129:
							goto l124
						l128:
							position, tokenIndex = position128, tokenIndex128
						}
						if !matchDot() {
							goto l124
						}
						goto l123
					l124:
						position, tokenIndex = position124, tokenIndex124
					}
					add(rulePegText, position122)
				}
				if !_rules[ruleAction14]() {
					goto l120
				}
				add(ruleregexpTail, position121)
			}
			return true
		l120:
			position, tokenIndex = position120, tokenIndex120
			return false
		},
		/* 22 regexpEscape <- <('\\' '/' Action15)> */
		func() bool {
			position131, tokenIndex131 := position, tokenIndex
			{
				position132 := position
				if buffer[position] != rune('\\') {
					goto l131
				}
				position++
				if buffer[position] != rune('/') {
					goto l131
				}
				position++
				if !_rules[ruleAction15]() {
					goto l131
				}
				add(ruleregexpEscape, position132)
			}
			return true
		l131:
			position, tokenIndex = position131, tokenIndex131
			return false
		},
		/* 23 String <- <(QuotedString Action16)> */
		func() bool {
			position133, tokenIndex133 := position, tokenIndex
			{
				position134 := position
				if !_rules[ruleQuotedString]() {
					goto l133
				}
				if !_rules[ruleAction16]() {
					goto l133
				}
				add(ruleString, position134)
			}
			return true
		l133:
			position, tokenIndex = position133, tokenIndex133
			return false
		},
		/* 24 QuotedString <- <<('"' StringBody* '"')>> */
		func() bool {
			position135, tokenIndex135 := position, tokenIndex
			{
				position136 := position
				{
					position137 := position
					if buffer[position] != rune('"') {
						goto l135
					}
					position++
				l138:
					{
						position139, tokenIndex139 := position, tokenIndex
						if !_rules[ruleStringBody]() {
							goto l139
						}
						goto l138
					l139:
						position, tokenIndex = position139, tokenIndex139
					}
					if buffer[position] != rune('"') {
						goto l135
					}
					position++
					add(rulePegText, position137)
				}
				add(ruleQuotedString, position136)
			}
			return true
		l135:
			position, tokenIndex = position135, tokenIndex135
			return false
		},
		/* 25 StringBody <- <(Escape / ('\\' '"') / (!'"' .))> */
		func() bool {
			position140, tokenIndex140 := position, tokenIndex
			{
				position141 := position
				{
					position142, tokenIndex142 := position, tokenIndex
					if !_rules[ruleEscape]() {
						goto l143
					}
					goto l142
				l143:
					position, tokenIndex = position142, tokenIndex142
					if buffer[position] != rune('\\') {
						goto l144
					}
					position++
					if buffer[position] != rune('"') {
						goto l144
					}
					position++
					goto l142
				l144:
					position, tokenIndex = position142, tokenIndex142
					{
						position145, tokenIndex145 := position, tokenIndex
						if buffer[position] != rune('"') {
							goto l145
						}
						position++
						goto l140
					l145:
						position, tokenIndex = position145, tokenIndex145
					}
					if !matchDot() {
						goto l140
					}
				}
			l142:
				add(ruleStringBody, position141)
			}
			return true
		l140:
			position, tokenIndex = position140, tokenIndex140
			return false
		},
		/* 26 Escape <- <(('\\' 'a') / ('\\' 'b') / ('\\' 'f') / ('\\' 'n') / ('\\' 'r') / ('\\' 't') / ('\\' 'v') / OctEscape / HexEscape / UnicodeShortEscape / UnicodeWideEscape)> */
		func() bool {
			position146, tokenIndex146 := position, tokenIndex
			{
				position147 := position
				{
					position148, tokenIndex148 := position, tokenIndex
					if buffer[position] != rune('\\') {
						goto l149
					}
					position++
					if buffer[position] != rune('a') {
						goto l149
					}
					position++
					goto l148
				l149:
					position, tokenIndex = position148, tokenIndex148
					if buffer[position] != rune('\\') {
						goto l150
					}
					position++
					if buffer[position] != rune('b') {
						goto l150
					}
					position++
					goto l148
				l150:
					position, tokenIndex = position148, tokenIndex148
					if buffer[position] != rune('\\') {
						goto l151
					}
					position++
					if buffer[position] != rune('f') {
						goto l151
					}
					position++
					goto l148
				l151:
					position, tokenIndex = position148, tokenIndex148
					if buffer[position] != rune('\\') {
						goto l152
					}
					position++
					if buffer[position] != rune('n') {
						goto l152
					}
					position++
					goto l148
				l152:
					position, tokenIndex = position148, tokenIndex148
					if buffer[position] != rune('\\') {
						goto l153
					}
					position++
					if buffer[position] != rune('r') {
						goto l153
					}
					position++
					goto l148
				l153:
					position, tokenIndex = position148, tokenIndex148
					if buffer[position] != rune('\\') {
						goto l154
					}
					position++
					if buffer[position] != rune('t') {
						goto l154
					}
					position++
					goto l148
				l154:
					position, tokenIndex = position148, tokenIndex148
					if buffer[position] != rune('\\') {
						goto l155
					}
					position++
					if buffer[position] != rune('v') {
						goto l155
					}
					position++
					goto l148
				l155:
					position, tokenIndex = position148, tokenIndex148
					if !_rules[ruleOctEscape]() {
						goto l156
					}
					goto l148
				l156:
					position, tokenIndex = position148, tokenIndex148
					if !_rules[ruleHexEscape]() {
						goto l157
					}
					goto l148
				l157:
					position, tokenIndex = position148, tokenIndex148
					if !_rules[ruleUnicodeShortEscape]() {
						goto l158
					}
					goto l148
				l158:
					position, tokenIndex = position148, tokenIndex148
					if !_rules[ruleUnicodeWideEscape]() {
						goto l146
					}
				}
			l148:
				add(ruleEscape, position147)
			}
			return true
		l146:
			position, tokenIndex = position146, tokenIndex146
			return false
		},
		/* 27 OctEscape <- <('\\' OctDigit OctDigit OctDigit)> */
		func() bool {
			position159, tokenIndex159 := position, tokenIndex
			{
				position160 := position
				if buffer[position] != rune('\\') {
					goto l159
				}
				position++
				if !_rules[ruleOctDigit]() {
					goto l159
				}
				if !_rules[ruleOctDigit]() {
					goto l159
				}
				if !_rules[ruleOctDigit]() {
					goto l159
				}
				add(ruleOctEscape, position160)
			}
			return true
		l159:
			position, tokenIndex = position159, tokenIndex159
			return false
		},
		/* 28 HexEscape <- <('\\' 'x' HexByte)> */
		func() bool {
			position161, tokenIndex161 := position, tokenIndex
			{
				position162 := position
				if buffer[position] != rune('\\') {
					goto l161
				}
				position++
				if buffer[position] != rune('x') {
					goto l161
				}
				position++
				if !_rules[ruleHexByte]() {
					goto l161
				}
				add(ruleHexEscape, position162)
			}
			return true
		l161:
			position, tokenIndex = position161, tokenIndex161
			return false
		},
		/* 29 UnicodeShortEscape <- <('\\' 'u' HexByte HexByte)> */
		func() bool {
			position163, tokenIndex163 := position, tokenIndex
			{
				position164 := position
				if buffer[position] != rune('\\') {
					goto l163
				}
				position++
				if buffer[position] != rune('u') {
					goto l163
				}
				position++
				if !_rules[ruleHexByte]() {
					goto l163
				}
				if !_rules[ruleHexByte]() {
					goto l163
				}
				add(ruleUnicodeShortEscape, position164)
			}
			return true
		l163:
			position, tokenIndex = position163, tokenIndex163
			return false
		},
		/* 30 UnicodeWideEscape <- <('\\' 'U' HexByte HexByte HexByte HexByte)> */
		func() bool {
			position165, tokenIndex165 := position, tokenIndex
			{
				position166 := position
				if buffer[position] != rune('\\') {
					goto l165
				}
				position++
				if buffer[position] != rune('U') {
					goto l165
				}
				position++
				if !_rules[ruleHexByte]() {
					goto l165
				}
				if !_rules[ruleHexByte]() {
					goto l165
				}
				if !_rules[ruleHexByte]() {
					goto l165
				}
				if !_rules[ruleHexByte]() {
					goto l165
				}
				add(ruleUnicodeWideEscape, position166)
			}
			return true
		l165:
			position, tokenIndex = position165, tokenIndex165
			return false
		},
		/* 31 Number <- <(Sign? (Decimal / Rational / Integer))> */
		func() bool {
			position167, tokenIndex167 := position, tokenIndex
			{
				position168 := position
				{
					position169, tokenIndex169 := position, tokenIndex
					if !_rules[ruleSign]() {
						goto l169
					}
					goto l170
				l169:
					position, tokenIndex = position169, tokenIndex169
				}
			l170:
				{
					position171, tokenIndex171 := position, tokenIndex
					if !_rules[ruleDecimal]() {
						goto l172
					}
					goto l171
				l172:
					position, tokenIndex = position171, tokenIndex171
					if !_rules[ruleRational]() {
						goto l173
					}
					goto l171
				l173:
					position, tokenIndex = position171, tokenIndex171
					if !_rules[ruleInteger]() {
						goto l167
					}
				}
			l171:
				add(ruleNumber, position168)
			}
			return true
		l167:
			position, tokenIndex = position167, tokenIndex167
			return false
		},
		/* 32 OctDigit <- <[0-8]> */
		func() bool {
			position174, tokenIndex174 := position, tokenIndex
			{
				position175 := position
				if c := buffer[position]; c < rune('0') || c > rune('8') {
					goto l174
				}
				position++
				add(ruleOctDigit, position175)
			}
			return true
		l174:
			position, tokenIndex = position174, tokenIndex174
			return false
		},
		/* 33 HexDigit <- <([0-9] / [0-9] / ([a-f] / [A-F]))> */
		func() bool {
			position176, tokenIndex176 := position, tokenIndex
			{
				position177 := position
				{
					position178, tokenIndex178 := position, tokenIndex
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l179
					}
					position++
					goto l178
				l179:
					position, tokenIndex = position178, tokenIndex178
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l180
					}
					position++
					goto l178
				l180:
					position, tokenIndex = position178, tokenIndex178
					{
						position181, tokenIndex181 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('f') {
							goto l182
						}
						position++
						goto l181
					l182:
						position, tokenIndex = position181, tokenIndex181
						if c := buffer[position]; c < rune('A') || c > rune('F') {
							goto l176
						}
						position++
					}
				l181:
				}
			l178:
				add(ruleHexDigit, position177)
			}
			return true
		l176:
			position, tokenIndex = position176, tokenIndex176
			return false
		},
		/* 34 HexByte <- <(HexDigit HexDigit)> */
		func() bool {
			position183, tokenIndex183 := position, tokenIndex
			{
				position184 := position
				if !_rules[ruleHexDigit]() {
					goto l183
				}
				if !_rules[ruleHexDigit]() {
					goto l183
				}
				add(ruleHexByte, position184)
			}
			return true
		l183:
			position, tokenIndex = position183, tokenIndex183
			return false
		},
		/* 35 Rational <- <(<(Int '/' Int)> Action17)> */
		func() bool {
			position185, tokenIndex185 := position, tokenIndex
			{
				position186 := position
				{
					position187 := position
					if !_rules[ruleInt]() {
						goto l185
					}
					if buffer[position] != rune('/') {
						goto l185
					}
					position++
					if !_rules[ruleInt]() {
						goto l185
					}
					add(rulePegText, position187)
				}
				if !_rules[ruleAction17]() {
					goto l185
				}
				add(ruleRational, position186)
			}
			return true
		l185:
			position, tokenIndex = position185, tokenIndex185
			return false
		},
		/* 36 Decimal <- <((PointDecimal Action18) / (ExpDecimal Action19))> */
		func() bool {
			position188, tokenIndex188 := position, tokenIndex
			{
				position189 := position
				{
					position190, tokenIndex190 := position, tokenIndex
					if !_rules[rulePointDecimal]() {
						goto l191
					}
					if !_rules[ruleAction18]() {
						goto l191
					}
					goto l190
				l191:
					position, tokenIndex = position190, tokenIndex190
					if !_rules[ruleExpDecimal]() {
						goto l188
					}
					if !_rules[ruleAction19]() {
						goto l188
					}
				}
			l190:
				add(ruleDecimal, position189)
			}
			return true
		l188:
			position, tokenIndex = position188, tokenIndex188
			return false
		},
		/* 37 PointDecimal <- <<(Int '.' Int Exponent?)>> */
		func() bool {
			position192, tokenIndex192 := position, tokenIndex
			{
				position193 := position
				{
					position194 := position
					if !_rules[ruleInt]() {
						goto l192
					}
					if buffer[position] != rune('.') {
						goto l192
					}
					position++
					if !_rules[ruleInt]() {
						goto l192
					}
					{
						position195, tokenIndex195 := position, tokenIndex
						if !_rules[ruleExponent]() {
							goto l195
						}
						goto l196
					l195:
						position, tokenIndex = position195, tokenIndex195
					}
				l196:
					add(rulePegText, position194)
				}
				add(rulePointDecimal, position193)
			}
			return true
		l192:
			position, tokenIndex = position192, tokenIndex192
			return false
		},
		/* 38 ExpDecimal <- <<(Int Exponent)>> */
		func() bool {
			position197, tokenIndex197 := position, tokenIndex
			{
				position198 := position
				{
					position199 := position
					if !_rules[ruleInt]() {
						goto l197
					}
					if !_rules[ruleExponent]() {
						goto l197
					}
					add(rulePegText, position199)
				}
				add(ruleExpDecimal, position198)
			}
			return true
		l197:
			position, tokenIndex = position197, tokenIndex197
			return false
		},
		/* 39 Exponent <- <(('E' / 'e') ('-' / '+')? Int)> */
		func() bool {
			position200, tokenIndex200 := position, tokenIndex
			{
				position201 := position
				{
					position202, tokenIndex202 := position, tokenIndex
					if buffer[position] != rune('E') {
						goto l203
					}
					position++
					goto l202
				l203:
					position, tokenIndex = position202, tokenIndex202
					if buffer[position] != rune('e') {
						goto l200
					}
					position++
				}
			l202:
				{
					position204, tokenIndex204 := position, tokenIndex
					{
						position206, tokenIndex206 := position, tokenIndex
						if buffer[position] != rune('-') {
							goto l207
						}
						position++
						goto l206
					l207:
						position, tokenIndex = position206, tokenIndex206
						if buffer[position] != rune('+') {
							goto l204
						}
						position++
					}
				l206:
					goto l205
				l204:
					position, tokenIndex = position204, tokenIndex204
				}
			l205:
				if !_rules[ruleInt]() {
					goto l200
				}
				add(ruleExponent, position201)
			}
			return true
		l200:
			position, tokenIndex = position200, tokenIndex200
			return false
		},
		/* 40 Integer <- <(BaseInt / HexLit / BinLit / OctLit / DecInt)> */
		func() bool {
			position208, tokenIndex208 := position, tokenIndex
			{
				position209 := position
				{
					position210, tokenIndex210 := position, tokenIndex
					if !_rules[ruleBaseInt]() {
						goto l211
					}
					goto l210
				l211:
					position, tokenIndex = position210, tokenIndex210
					if !_rules[ruleHexLit]() {
						goto l212
					}
					goto l210
				l212:
					position, tokenIndex = position210, tokenIndex210
					if !_rules[ruleBinLit]() {
						goto l213
					}
					goto l210
				l213:
					position, tokenIndex = position210, tokenIndex210
					if !_rules[ruleOctLit]() {
						goto l214
					}
					goto l210
				l214:
					position, tokenIndex = position210, tokenIndex210
					if !_rules[ruleDecInt]() {
						goto l208
					}
				}
			l210:
				add(ruleInteger, position209)
			}
			return true
		l208:
			position, tokenIndex = position208, tokenIndex208
			return false
		},
		/* 41 Int <- <('0' / PosInt)> */
		func() bool {
			position215, tokenIndex215 := position, tokenIndex
			{
				position216 := position
				{
					position217, tokenIndex217 := position, tokenIndex
					if buffer[position] != rune('0') {
						goto l218
					}
					position++
					goto l217
				l218:
					position, tokenIndex = position217, tokenIndex217
					if !_rules[rulePosInt]() {
						goto l215
					}
				}
			l217:
				add(ruleInt, position216)
			}
			return true
		l215:
			position, tokenIndex = position215, tokenIndex215
			return false
		},
		/* 42 PosInt <- <([1-9] [0-9]*)> */
		func() bool {
			position219, tokenIndex219 := position, tokenIndex
			{
				position220 := position
				if c := buffer[position]; c < rune('1') || c > rune('9') {
					goto l219
				}
				position++
			l221:
				{
					position222, tokenIndex222 := position, tokenIndex
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l222
					}
					position++
					goto l221
				l222:
					position, tokenIndex = position222, tokenIndex222
				}
				add(rulePosInt, position220)
			}
			return true
		l219:
			position, tokenIndex = position219, tokenIndex219
			return false
		},
		/* 43 DecInt <- <(<Int> Action20)> */
		func() bool {
			position223, tokenIndex223 := position, tokenIndex
			{
				position224 := position
				{
					position225 := position
					if !_rules[ruleInt]() {
						goto l223
					}
					add(rulePegText, position225)
				}
				if !_rules[ruleAction20]() {
					goto l223
				}
				add(ruleDecInt, position224)
			}
			return true
		l223:
			position, tokenIndex = position223, tokenIndex223
			return false
		},
		/* 44 BaseInt <- <(NumBase Action21 '#' NumLit Action22)> */
		func() bool {
			position226, tokenIndex226 := position, tokenIndex
			{
				position227 := position
				if !_rules[ruleNumBase]() {
					goto l226
				}
				if !_rules[ruleAction21]() {
					goto l226
				}
				if buffer[position] != rune('#') {
					goto l226
				}
				position++
				if !_rules[ruleNumLit]() {
					goto l226
				}
				if !_rules[ruleAction22]() {
					goto l226
				}
				add(ruleBaseInt, position227)
			}
			return true
		l226:
			position, tokenIndex = position226, tokenIndex226
			return false
		},
		/* 45 NumBase <- <<((('1' / '2') [0-9]) / ('3' [0-6]) / ([2-9] ![0-9]))>> */
		func() bool {
			position228, tokenIndex228 := position, tokenIndex
			{
				position229 := position
				{
					position230 := position
					{
						position231, tokenIndex231 := position, tokenIndex
						{
							position233, tokenIndex233 := position, tokenIndex
							if buffer[position] != rune('1') {
								goto l234
							}
							position++
							goto l233
						l234:
							position, tokenIndex = position233, tokenIndex233
							if buffer[position] != rune('2') {
								goto l232
							}
							position++
						}
					l233:
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l232
						}
						position++
						goto l231
					l232:
						position, tokenIndex = position231, tokenIndex231
						if buffer[position] != rune('3') {
							goto l235
						}
						position++
						if c := buffer[position]; c < rune('0') || c > rune('6') {
							goto l235
						}
						position++
						goto l231
					l235:
						position, tokenIndex = position231, tokenIndex231
						if c := buffer[position]; c < rune('2') || c > rune('9') {
							goto l228
						}
						position++
						{
							position236, tokenIndex236 := position, tokenIndex
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l236
							}
							position++
							goto l228
						l236:
							position, tokenIndex = position236, tokenIndex236
						}
					}
				l231:
					add(rulePegText, position230)
				}
				add(ruleNumBase, position229)
			}
			return true
		l228:
			position, tokenIndex = position228, tokenIndex228
			return false
		},
		/* 46 NumLit <- <<([a-z] / [A-Z] / ([0-9] / [0-9]))+>> */
		func() bool {
			position237, tokenIndex237 := position, tokenIndex
			{
				position238 := position
				{
					position239 := position
					{
						position242, tokenIndex242 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l243
						}
						position++
						goto l242
					l243:
						position, tokenIndex = position242, tokenIndex242
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l244
						}
						position++
						goto l242
					l244:
						position, tokenIndex = position242, tokenIndex242
						{
							position245, tokenIndex245 := position, tokenIndex
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l246
							}
							position++
							goto l245
						l246:
							position, tokenIndex = position245, tokenIndex245
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l237
							}
							position++
						}
					l245:
					}
				l242:
				l240:
					{
						position241, tokenIndex241 := position, tokenIndex
						{
							position247, tokenIndex247 := position, tokenIndex
							if c := buffer[position]; c < rune('a') || c > rune('z') {
								goto l248
							}
							position++
							goto l247
						l248:
							position, tokenIndex = position247, tokenIndex247
							if c := buffer[position]; c < rune('A') || c > rune('Z') {
								goto l249
							}
							position++
							goto l247
						l249:
							position, tokenIndex = position247, tokenIndex247
							{
								position250, tokenIndex250 := position, tokenIndex
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l251
								}
								position++
								goto l250
							l251:
								position, tokenIndex = position250, tokenIndex250
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l241
								}
								position++
							}
						l250:
						}
					l247:
						goto l240
					l241:
						position, tokenIndex = position241, tokenIndex241
					}
					add(rulePegText, position239)
				}
				add(ruleNumLit, position238)
			}
			return true
		l237:
			position, tokenIndex = position237, tokenIndex237
			return false
		},
		/* 47 HexLit <- <('0' ('x' / 'X') <HexDigit+> Action23)> */
		func() bool {
			position252, tokenIndex252 := position, tokenIndex
			{
				position253 := position
				if buffer[position] != rune('0') {
					goto l252
				}
				position++
				{
					position254, tokenIndex254 := position, tokenIndex
					if buffer[position] != rune('x') {
						goto l255
					}
					position++
					goto l254
				l255:
					position, tokenIndex = position254, tokenIndex254
					if buffer[position] != rune('X') {
						goto l252
					}
					position++
				}
			l254:
				{
					position256 := position
					if !_rules[ruleHexDigit]() {
						goto l252
					}
				l257:
					{
						position258, tokenIndex258 := position, tokenIndex
						if !_rules[ruleHexDigit]() {
							goto l258
						}
						goto l257
					l258:
						position, tokenIndex = position258, tokenIndex258
					}
					add(rulePegText, position256)
				}
				if !_rules[ruleAction23]() {
					goto l252
				}
				add(ruleHexLit, position253)
			}
			return true
		l252:
			position, tokenIndex = position252, tokenIndex252
			return false
		},
		/* 48 OctLit <- <('0' <[0-7]+> ![8-9] Action24)> */
		func() bool {
			position259, tokenIndex259 := position, tokenIndex
			{
				position260 := position
				if buffer[position] != rune('0') {
					goto l259
				}
				position++
				{
					position261 := position
					if c := buffer[position]; c < rune('0') || c > rune('7') {
						goto l259
					}
					position++
				l262:
					{
						position263, tokenIndex263 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('7') {
							goto l263
						}
						position++
						goto l262
					l263:
						position, tokenIndex = position263, tokenIndex263
					}
					add(rulePegText, position261)
				}
				{
					position264, tokenIndex264 := position, tokenIndex
					if c := buffer[position]; c < rune('8') || c > rune('9') {
						goto l264
					}
					position++
					goto l259
				l264:
					position, tokenIndex = position264, tokenIndex264
				}
				if !_rules[ruleAction24]() {
					goto l259
				}
				add(ruleOctLit, position260)
			}
			return true
		l259:
			position, tokenIndex = position259, tokenIndex259
			return false
		},
		/* 49 BinLit <- <('0' ('b' / 'B') <('0' / '1')+> ![2-9] Action25)> */
		func() bool {
			position265, tokenIndex265 := position, tokenIndex
			{
				position266 := position
				if buffer[position] != rune('0') {
					goto l265
				}
				position++
				{
					position267, tokenIndex267 := position, tokenIndex
					if buffer[position] != rune('b') {
						goto l268
					}
					position++
					goto l267
				l268:
					position, tokenIndex = position267, tokenIndex267
					if buffer[position] != rune('B') {
						goto l265
					}
					position++
				}
			l267:
				{
					position269 := position
					{
						position272, tokenIndex272 := position, tokenIndex
						if buffer[position] != rune('0') {
							goto l273
						}
						position++
						goto l272
					l273:
						position, tokenIndex = position272, tokenIndex272
						if buffer[position] != rune('1') {
							goto l265
						}
						position++
					}
				l272:
				l270:
					{
						position271, tokenIndex271 := position, tokenIndex
						{
							position274, tokenIndex274 := position, tokenIndex
							if buffer[position] != rune('0') {
								goto l275
							}
							position++
							goto l274
						l275:
							position, tokenIndex = position274, tokenIndex274
							if buffer[position] != rune('1') {
								goto l271
							}
							position++
						}
					l274:
						goto l270
					l271:
						position, tokenIndex = position271, tokenIndex271
					}
					add(rulePegText, position269)
				}
				{
					position276, tokenIndex276 := position, tokenIndex
					if c := buffer[position]; c < rune('2') || c > rune('9') {
						goto l276
					}
					position++
					goto l265
				l276:
					position, tokenIndex = position276, tokenIndex276
				}
				if !_rules[ruleAction25]() {
					goto l265
				}
				add(ruleBinLit, position266)
			}
			return true
		l265:
			position, tokenIndex = position265, tokenIndex265
			return false
		},
		/* 50 Sign <- <(('-' Action26) / ('+' Action27))> */
		func() bool {
			position277, tokenIndex277 := position, tokenIndex
			{
				position278 := position
				{
					position279, tokenIndex279 := position, tokenIndex
					if buffer[position] != rune('-') {
						goto l280
					}
					position++
					if !_rules[ruleAction26]() {
						goto l280
					}
					goto l279
				l280:
					position, tokenIndex = position279, tokenIndex279
					if buffer[position] != rune('+') {
						goto l277
					}
					position++
					if !_rules[ruleAction27]() {
						goto l277
					}
				}
			l279:
				add(ruleSign, position278)
			}
			return true
		l277:
			position, tokenIndex = position277, tokenIndex277
			return false
		},
		/* 51 Boolean <- <((True Action28) / (False Action29))> */
		func() bool {
			position281, tokenIndex281 := position, tokenIndex
			{
				position282 := position
				{
					position283, tokenIndex283 := position, tokenIndex
					if !_rules[ruleTrue]() {
						goto l284
					}
					if !_rules[ruleAction28]() {
						goto l284
					}
					goto l283
				l284:
					position, tokenIndex = position283, tokenIndex283
					if !_rules[ruleFalse]() {
						goto l281
					}
					if !_rules[ruleAction29]() {
						goto l281
					}
				}
			l283:
				add(ruleBoolean, position282)
			}
			return true
		l281:
			position, tokenIndex = position281, tokenIndex281
			return false
		},
		/* 52 TrueKw <- <(('t' 'r' 'u' 'e') / ('T' 'r' 'u' 'e') / ('T' 'R' 'U' 'E') / ('y' 'e' 's') / ('Y' 'e' 's') / ('Y' 'E' 'S'))> */
		func() bool {
			position285, tokenIndex285 := position, tokenIndex
			{
				position286 := position
				{
					position287, tokenIndex287 := position, tokenIndex
					if buffer[position] != rune('t') {
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
					goto l287
				l288:
					position, tokenIndex = position287, tokenIndex287
					if buffer[position] != rune('T') {
						goto l289
					}
					position++
					if buffer[position] != rune('r') {
						goto l289
					}
					position++
					if buffer[position] != rune('u') {
						goto l289
					}
					position++
					if buffer[position] != rune('e') {
						goto l289
					}
					position++
					goto l287
				l289:
					position, tokenIndex = position287, tokenIndex287
					if buffer[position] != rune('T') {
						goto l290
					}
					position++
					if buffer[position] != rune('R') {
						goto l290
					}
					position++
					if buffer[position] != rune('U') {
						goto l290
					}
					position++
					if buffer[position] != rune('E') {
						goto l290
					}
					position++
					goto l287
				l290:
					position, tokenIndex = position287, tokenIndex287
					if buffer[position] != rune('y') {
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
					goto l287
				l291:
					position, tokenIndex = position287, tokenIndex287
					if buffer[position] != rune('Y') {
						goto l292
					}
					position++
					if buffer[position] != rune('e') {
						goto l292
					}
					position++
					if buffer[position] != rune('s') {
						goto l292
					}
					position++
					goto l287
				l292:
					position, tokenIndex = position287, tokenIndex287
					if buffer[position] != rune('Y') {
						goto l285
					}
					position++
					if buffer[position] != rune('E') {
						goto l285
					}
					position++
					if buffer[position] != rune('S') {
						goto l285
					}
					position++
				}
			l287:
				add(ruleTrueKw, position286)
			}
			return true
		l285:
			position, tokenIndex = position285, tokenIndex285
			return false
		},
		/* 53 FalseKw <- <(('f' 'a' 'l' 's' 'e') / ('F' 'a' 'l' 's' 'e') / ('F' 'A' 'L' 'S' 'E') / ('n' 'o') / ('N' 'o') / ('N' 'O'))> */
		func() bool {
			position293, tokenIndex293 := position, tokenIndex
			{
				position294 := position
				{
					position295, tokenIndex295 := position, tokenIndex
					if buffer[position] != rune('f') {
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
					goto l295
				l296:
					position, tokenIndex = position295, tokenIndex295
					if buffer[position] != rune('F') {
						goto l297
					}
					position++
					if buffer[position] != rune('a') {
						goto l297
					}
					position++
					if buffer[position] != rune('l') {
						goto l297
					}
					position++
					if buffer[position] != rune('s') {
						goto l297
					}
					position++
					if buffer[position] != rune('e') {
						goto l297
					}
					position++
					goto l295
				l297:
					position, tokenIndex = position295, tokenIndex295
					if buffer[position] != rune('F') {
						goto l298
					}
					position++
					if buffer[position] != rune('A') {
						goto l298
					}
					position++
					if buffer[position] != rune('L') {
						goto l298
					}
					position++
					if buffer[position] != rune('S') {
						goto l298
					}
					position++
					if buffer[position] != rune('E') {
						goto l298
					}
					position++
					goto l295
				l298:
					position, tokenIndex = position295, tokenIndex295
					if buffer[position] != rune('n') {
						goto l299
					}
					position++
					if buffer[position] != rune('o') {
						goto l299
					}
					position++
					goto l295
				l299:
					position, tokenIndex = position295, tokenIndex295
					if buffer[position] != rune('N') {
						goto l300
					}
					position++
					if buffer[position] != rune('o') {
						goto l300
					}
					position++
					goto l295
				l300:
					position, tokenIndex = position295, tokenIndex295
					if buffer[position] != rune('N') {
						goto l293
					}
					position++
					if buffer[position] != rune('O') {
						goto l293
					}
					position++
				}
			l295:
				add(ruleFalseKw, position294)
			}
			return true
		l293:
			position, tokenIndex = position293, tokenIndex293
			return false
		},
		/* 54 True <- <(TrueKw !BarewordTail)> */
		func() bool {
			position301, tokenIndex301 := position, tokenIndex
			{
				position302 := position
				if !_rules[ruleTrueKw]() {
					goto l301
				}
				{
					position303, tokenIndex303 := position, tokenIndex
					if !_rules[ruleBarewordTail]() {
						goto l303
					}
					goto l301
				l303:
					position, tokenIndex = position303, tokenIndex303
				}
				add(ruleTrue, position302)
			}
			return true
		l301:
			position, tokenIndex = position301, tokenIndex301
			return false
		},
		/* 55 False <- <(FalseKw !BarewordTail)> */
		func() bool {
			position304, tokenIndex304 := position, tokenIndex
			{
				position305 := position
				if !_rules[ruleFalseKw]() {
					goto l304
				}
				{
					position306, tokenIndex306 := position, tokenIndex
					if !_rules[ruleBarewordTail]() {
						goto l306
					}
					goto l304
				l306:
					position, tokenIndex = position306, tokenIndex306
				}
				add(ruleFalse, position305)
			}
			return true
		l304:
			position, tokenIndex = position304, tokenIndex304
			return false
		},
		/* 56 Nil <- <(('n' / 'N') ('i' / 'I') ('l' / 'L') !BarewordTail Action30)> */
		func() bool {
			position307, tokenIndex307 := position, tokenIndex
			{
				position308 := position
				{
					position309, tokenIndex309 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l310
					}
					position++
					goto l309
				l310:
					position, tokenIndex = position309, tokenIndex309
					if buffer[position] != rune('N') {
						goto l307
					}
					position++
				}
			l309:
				{
					position311, tokenIndex311 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l312
					}
					position++
					goto l311
				l312:
					position, tokenIndex = position311, tokenIndex311
					if buffer[position] != rune('I') {
						goto l307
					}
					position++
				}
			l311:
				{
					position313, tokenIndex313 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l314
					}
					position++
					goto l313
				l314:
					position, tokenIndex = position313, tokenIndex313
					if buffer[position] != rune('L') {
						goto l307
					}
					position++
				}
			l313:
				{
					position315, tokenIndex315 := position, tokenIndex
					if !_rules[ruleBarewordTail]() {
						goto l315
					}
					goto l307
				l315:
					position, tokenIndex = position315, tokenIndex315
				}
				if !_rules[ruleAction30]() {
					goto l307
				}
				add(ruleNil, position308)
			}
			return true
		l307:
			position, tokenIndex = position307, tokenIndex307
			return false
		},
		/* 57 Sentinel <- <';'> */
		func() bool {
			position316, tokenIndex316 := position, tokenIndex
			{
				position317 := position
				if buffer[position] != rune(';') {
					goto l316
				}
				position++
				add(ruleSentinel, position317)
			}
			return true
		l316:
			position, tokenIndex = position316, tokenIndex316
			return false
		},
		/* 58 Space <- <((' ' / '\t' / '\n' / '\r')+ / Comment)> */
		func() bool {
			position318, tokenIndex318 := position, tokenIndex
			{
				position319 := position
				{
					position320, tokenIndex320 := position, tokenIndex
					{
						position324, tokenIndex324 := position, tokenIndex
						if buffer[position] != rune(' ') {
							goto l325
						}
						position++
						goto l324
					l325:
						position, tokenIndex = position324, tokenIndex324
						if buffer[position] != rune('\t') {
							goto l326
						}
						position++
						goto l324
					l326:
						position, tokenIndex = position324, tokenIndex324
						if buffer[position] != rune('\n') {
							goto l327
						}
						position++
						goto l324
					l327:
						position, tokenIndex = position324, tokenIndex324
						if buffer[position] != rune('\r') {
							goto l321
						}
						position++
					}
				l324:
				l322:
					{
						position323, tokenIndex323 := position, tokenIndex
						{
							position328, tokenIndex328 := position, tokenIndex
							if buffer[position] != rune(' ') {
								goto l329
							}
							position++
							goto l328
						l329:
							position, tokenIndex = position328, tokenIndex328
							if buffer[position] != rune('\t') {
								goto l330
							}
							position++
							goto l328
						l330:
							position, tokenIndex = position328, tokenIndex328
							if buffer[position] != rune('\n') {
								goto l331
							}
							position++
							goto l328
						l331:
							position, tokenIndex = position328, tokenIndex328
							if buffer[position] != rune('\r') {
								goto l323
							}
							position++
						}
					l328:
						goto l322
					l323:
						position, tokenIndex = position323, tokenIndex323
					}
					goto l320
				l321:
					position, tokenIndex = position320, tokenIndex320
					if !_rules[ruleComment]() {
						goto l318
					}
				}
			l320:
				add(ruleSpace, position319)
			}
			return true
		l318:
			position, tokenIndex = position318, tokenIndex318
			return false
		},
		/* 59 ReqSpace <- <Space+> */
		func() bool {
			position332, tokenIndex332 := position, tokenIndex
			{
				position333 := position
				if !_rules[ruleSpace]() {
					goto l332
				}
			l334:
				{
					position335, tokenIndex335 := position, tokenIndex
					if !_rules[ruleSpace]() {
						goto l335
					}
					goto l334
				l335:
					position, tokenIndex = position335, tokenIndex335
				}
				add(ruleReqSpace, position333)
			}
			return true
		l332:
			position, tokenIndex = position332, tokenIndex332
			return false
		},
		/* 60 OptSpace <- <Space*> */
		func() bool {
			{
				position337 := position
			l338:
				{
					position339, tokenIndex339 := position, tokenIndex
					if !_rules[ruleSpace]() {
						goto l339
					}
					goto l338
				l339:
					position, tokenIndex = position339, tokenIndex339
				}
				add(ruleOptSpace, position337)
			}
			return true
		},
		/* 61 Comment <- <('\'' (!EOL .)* EOL)> */
		func() bool {
			position340, tokenIndex340 := position, tokenIndex
			{
				position341 := position
				if buffer[position] != rune('\'') {
					goto l340
				}
				position++
			l342:
				{
					position343, tokenIndex343 := position, tokenIndex
					{
						position344, tokenIndex344 := position, tokenIndex
						if !_rules[ruleEOL]() {
							goto l344
						}
						goto l343
					l344:
						position, tokenIndex = position344, tokenIndex344
					}
					if !matchDot() {
						goto l343
					}
					goto l342
				l343:
					position, tokenIndex = position343, tokenIndex343
				}
				if !_rules[ruleEOL]() {
					goto l340
				}
				add(ruleComment, position341)
			}
			return true
		l340:
			position, tokenIndex = position340, tokenIndex340
			return false
		},
		/* 62 EOL <- <(('\r' '\n') / '\n' / '\r')> */
		func() bool {
			position345, tokenIndex345 := position, tokenIndex
			{
				position346 := position
				{
					position347, tokenIndex347 := position, tokenIndex
					if buffer[position] != rune('\r') {
						goto l348
					}
					position++
					if buffer[position] != rune('\n') {
						goto l348
					}
					position++
					goto l347
				l348:
					position, tokenIndex = position347, tokenIndex347
					if buffer[position] != rune('\n') {
						goto l349
					}
					position++
					goto l347
				l349:
					position, tokenIndex = position347, tokenIndex347
					if buffer[position] != rune('\r') {
						goto l345
					}
					position++
				}
			l347:
				add(ruleEOL, position346)
			}
			return true
		l345:
			position, tokenIndex = position345, tokenIndex345
			return false
		},
		/* 63 EOF <- <!.> */
		func() bool {
			position350, tokenIndex350 := position, tokenIndex
			{
				position351 := position
				{
					position352, tokenIndex352 := position, tokenIndex
					if !matchDot() {
						goto l352
					}
					goto l350
				l352:
					position, tokenIndex = position352, tokenIndex352
				}
				add(ruleEOF, position351)
			}
			return true
		l350:
			position, tokenIndex = position350, tokenIndex350
			return false
		},
		/* 65 Action0 <- <{ p.init() }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 66 Action1 <- <{ p.beginStatement(text) }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		/* 67 Action2 <- <{ p.closeStatement() }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
		/* 68 Action3 <- <{ p.beginSection(text) }> */
		func() bool {
			{
				add(ruleAction3, position)
			}
			return true
		},
		/* 69 Action4 <- <{ p.closeSection() }> */
		func() bool {
			{
				add(ruleAction4, position)
			}
			return true
		},
		/* 70 Action5 <- <{ p.sign = 1 }> */
		func() bool {
			{
				add(ruleAction5, position)
			}
			return true
		},
		/* 71 Action6 <- <{ p.beginArray() }> */
		func() bool {
			{
				add(ruleAction6, position)
			}
			return true
		},
		/* 72 Action7 <- <{ p.closeArray() }> */
		func() bool {
			{
				add(ruleAction7, position)
			}
			return true
		},
		/* 73 Action8 <- <{ p.beginMap() }> */
		func() bool {
			{
				add(ruleAction8, position)
			}
			return true
		},
		/* 74 Action9 <- <{ p.closeMap() }> */
		func() bool {
			{
				add(ruleAction9, position)
			}
			return true
		},
		/* 75 Action10 <- <{ p.consume(Symbol(text)) }> */
		func() bool {
			{
				add(ruleAction10, position)
			}
			return true
		},
		/* 76 Action11 <- <{ p.consume(Symbol(unquote(typeSymbol, text))) }> */
		func() bool {
			{
				add(ruleAction11, position)
			}
			return true
		},
		nil,
		/* 78 Action12 <- <{ p.beginRegexp() }> */
		func() bool {
			{
				add(ruleAction12, position)
			}
			return true
		},
		/* 79 Action13 <- <{ p.closeRegexp() }> */
		func() bool {
			{
				add(ruleAction13, position)
			}
			return true
		},
		/* 80 Action14 <- <{ p.tip().(*regexpBuilder).add(text) }> */
		func() bool {
			{
				add(ruleAction14, position)
			}
			return true
		},
		/* 81 Action15 <- <{ p.tip().(*regexpBuilder).add("/") }> */
		func() bool {
			{
				add(ruleAction15, position)
			}
			return true
		},
		/* 82 Action16 <- <{ p.consume(String(unquote(typeString, text))) }> */
		func() bool {
			{
				add(ruleAction16, position)
			}
			return true
		},
		/* 83 Action17 <- <{ p.consumeRational(text) }> */
		func() bool {
			{
				add(ruleAction17, position)
			}
			return true
		},
		/* 84 Action18 <- <{ p.consumeFloat(text) }> */
		func() bool {
			{
				add(ruleAction18, position)
			}
			return true
		},
		/* 85 Action19 <- <{ p.consumeFloat(text) }> */
		func() bool {
			{
				add(ruleAction19, position)
			}
			return true
		},
		/* 86 Action20 <- <{ p.consumeInteger(text, 10) }> */
		func() bool {
			{
				add(ruleAction20, position)
			}
			return true
		},
		/* 87 Action21 <- <{ p.parseBase(text) }> */
		func() bool {
			{
				add(ruleAction21, position)
			}
			return true
		},
		/* 88 Action22 <- <{ p.consumeInteger(text, p.baseIntBase) }> */
		func() bool {
			{
				add(ruleAction22, position)
			}
			return true
		},
		/* 89 Action23 <- <{ p.consumeInteger(text, 16) }> */
		func() bool {
			{
				add(ruleAction23, position)
			}
			return true
		},
		/* 90 Action24 <- <{ p.consumeInteger(text, 8) }> */
		func() bool {
			{
				add(ruleAction24, position)
			}
			return true
		},
		/* 91 Action25 <- <{ p.consumeInteger(text, 2) }> */
		func() bool {
			{
				add(ruleAction25, position)
			}
			return true
		},
		/* 92 Action26 <- <{ p.sign = -1 }> */
		func() bool {
			{
				add(ruleAction26, position)
			}
			return true
		},
		/* 93 Action27 <- <{ p.sign = 1 }> */
		func() bool {
			{
				add(ruleAction27, position)
			}
			return true
		},
		/* 94 Action28 <- <{ p.consume(Bool(true)) }> */
		func() bool {
			{
				add(ruleAction28, position)
			}
			return true
		},
		/* 95 Action29 <- <{ p.consume(Bool(false)) }> */
		func() bool {
			{
				add(ruleAction29, position)
			}
			return true
		},
		/* 96 Action30 <- <{ p.consume(Nil) }> */
		func() bool {
			{
				add(ruleAction30, position)
			}
			return true
		},
	}
	p.rules = _rules
}
