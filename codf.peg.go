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
	ruleDuration
	ruleDurSpec
	ruleDurNum
	ruleDurUnit
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
	"Duration",
	"DurSpec",
	"DurNum",
	"DurUnit",
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

	// Float precision
	Precision uint

	// stages []stage
	root *Root

	sign        int
	consumers   []consumer
	baseIntBase int

	Buffer string
	buffer []rune
	rules  [100]func() bool
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
			p.consumeDuration(text)

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
		/* 8 Literal <- <(Duration / (Action5 Number) / Boolean / Regexp / String / Symbol / Map / Array)> */
		func() bool {
			position29, tokenIndex29 := position, tokenIndex
			{
				position30 := position
				{
					position31, tokenIndex31 := position, tokenIndex
					if !_rules[ruleDuration]() {
						goto l32
					}
					goto l31
				l32:
					position, tokenIndex = position31, tokenIndex31
					if !_rules[ruleAction5]() {
						goto l33
					}
					if !_rules[ruleNumber]() {
						goto l33
					}
					goto l31
				l33:
					position, tokenIndex = position31, tokenIndex31
					if !_rules[ruleBoolean]() {
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
		/* 31 Number <- <(Sign? (Decimal / Rational / Integer) !BarewordTail)> */
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
				{
					position174, tokenIndex174 := position, tokenIndex
					if !_rules[ruleBarewordTail]() {
						goto l174
					}
					goto l167
				l174:
					position, tokenIndex = position174, tokenIndex174
				}
				add(ruleNumber, position168)
			}
			return true
		l167:
			position, tokenIndex = position167, tokenIndex167
			return false
		},
		/* 32 OctDigit <- <[0-8]> */
		func() bool {
			position175, tokenIndex175 := position, tokenIndex
			{
				position176 := position
				if c := buffer[position]; c < rune('0') || c > rune('8') {
					goto l175
				}
				position++
				add(ruleOctDigit, position176)
			}
			return true
		l175:
			position, tokenIndex = position175, tokenIndex175
			return false
		},
		/* 33 HexDigit <- <([0-9] / [0-9] / ([a-f] / [A-F]))> */
		func() bool {
			position177, tokenIndex177 := position, tokenIndex
			{
				position178 := position
				{
					position179, tokenIndex179 := position, tokenIndex
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l180
					}
					position++
					goto l179
				l180:
					position, tokenIndex = position179, tokenIndex179
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l181
					}
					position++
					goto l179
				l181:
					position, tokenIndex = position179, tokenIndex179
					{
						position182, tokenIndex182 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('f') {
							goto l183
						}
						position++
						goto l182
					l183:
						position, tokenIndex = position182, tokenIndex182
						if c := buffer[position]; c < rune('A') || c > rune('F') {
							goto l177
						}
						position++
					}
				l182:
				}
			l179:
				add(ruleHexDigit, position178)
			}
			return true
		l177:
			position, tokenIndex = position177, tokenIndex177
			return false
		},
		/* 34 HexByte <- <(HexDigit HexDigit)> */
		func() bool {
			position184, tokenIndex184 := position, tokenIndex
			{
				position185 := position
				if !_rules[ruleHexDigit]() {
					goto l184
				}
				if !_rules[ruleHexDigit]() {
					goto l184
				}
				add(ruleHexByte, position185)
			}
			return true
		l184:
			position, tokenIndex = position184, tokenIndex184
			return false
		},
		/* 35 Rational <- <(<(Int '/' PosInt)> Action17)> */
		func() bool {
			position186, tokenIndex186 := position, tokenIndex
			{
				position187 := position
				{
					position188 := position
					if !_rules[ruleInt]() {
						goto l186
					}
					if buffer[position] != rune('/') {
						goto l186
					}
					position++
					if !_rules[rulePosInt]() {
						goto l186
					}
					add(rulePegText, position188)
				}
				if !_rules[ruleAction17]() {
					goto l186
				}
				add(ruleRational, position187)
			}
			return true
		l186:
			position, tokenIndex = position186, tokenIndex186
			return false
		},
		/* 36 Decimal <- <((PointDecimal Action18) / (ExpDecimal Action19))> */
		func() bool {
			position189, tokenIndex189 := position, tokenIndex
			{
				position190 := position
				{
					position191, tokenIndex191 := position, tokenIndex
					if !_rules[rulePointDecimal]() {
						goto l192
					}
					if !_rules[ruleAction18]() {
						goto l192
					}
					goto l191
				l192:
					position, tokenIndex = position191, tokenIndex191
					if !_rules[ruleExpDecimal]() {
						goto l189
					}
					if !_rules[ruleAction19]() {
						goto l189
					}
				}
			l191:
				add(ruleDecimal, position190)
			}
			return true
		l189:
			position, tokenIndex = position189, tokenIndex189
			return false
		},
		/* 37 PointDecimal <- <<(Int '.' Int Exponent?)>> */
		func() bool {
			position193, tokenIndex193 := position, tokenIndex
			{
				position194 := position
				{
					position195 := position
					if !_rules[ruleInt]() {
						goto l193
					}
					if buffer[position] != rune('.') {
						goto l193
					}
					position++
					if !_rules[ruleInt]() {
						goto l193
					}
					{
						position196, tokenIndex196 := position, tokenIndex
						if !_rules[ruleExponent]() {
							goto l196
						}
						goto l197
					l196:
						position, tokenIndex = position196, tokenIndex196
					}
				l197:
					add(rulePegText, position195)
				}
				add(rulePointDecimal, position194)
			}
			return true
		l193:
			position, tokenIndex = position193, tokenIndex193
			return false
		},
		/* 38 ExpDecimal <- <<(Int Exponent)>> */
		func() bool {
			position198, tokenIndex198 := position, tokenIndex
			{
				position199 := position
				{
					position200 := position
					if !_rules[ruleInt]() {
						goto l198
					}
					if !_rules[ruleExponent]() {
						goto l198
					}
					add(rulePegText, position200)
				}
				add(ruleExpDecimal, position199)
			}
			return true
		l198:
			position, tokenIndex = position198, tokenIndex198
			return false
		},
		/* 39 Exponent <- <(('E' / 'e') ('-' / '+')? Int)> */
		func() bool {
			position201, tokenIndex201 := position, tokenIndex
			{
				position202 := position
				{
					position203, tokenIndex203 := position, tokenIndex
					if buffer[position] != rune('E') {
						goto l204
					}
					position++
					goto l203
				l204:
					position, tokenIndex = position203, tokenIndex203
					if buffer[position] != rune('e') {
						goto l201
					}
					position++
				}
			l203:
				{
					position205, tokenIndex205 := position, tokenIndex
					{
						position207, tokenIndex207 := position, tokenIndex
						if buffer[position] != rune('-') {
							goto l208
						}
						position++
						goto l207
					l208:
						position, tokenIndex = position207, tokenIndex207
						if buffer[position] != rune('+') {
							goto l205
						}
						position++
					}
				l207:
					goto l206
				l205:
					position, tokenIndex = position205, tokenIndex205
				}
			l206:
				if !_rules[ruleInt]() {
					goto l201
				}
				add(ruleExponent, position202)
			}
			return true
		l201:
			position, tokenIndex = position201, tokenIndex201
			return false
		},
		/* 40 Integer <- <(BaseInt / HexLit / BinLit / OctLit / DecInt)> */
		func() bool {
			position209, tokenIndex209 := position, tokenIndex
			{
				position210 := position
				{
					position211, tokenIndex211 := position, tokenIndex
					if !_rules[ruleBaseInt]() {
						goto l212
					}
					goto l211
				l212:
					position, tokenIndex = position211, tokenIndex211
					if !_rules[ruleHexLit]() {
						goto l213
					}
					goto l211
				l213:
					position, tokenIndex = position211, tokenIndex211
					if !_rules[ruleBinLit]() {
						goto l214
					}
					goto l211
				l214:
					position, tokenIndex = position211, tokenIndex211
					if !_rules[ruleOctLit]() {
						goto l215
					}
					goto l211
				l215:
					position, tokenIndex = position211, tokenIndex211
					if !_rules[ruleDecInt]() {
						goto l209
					}
				}
			l211:
				add(ruleInteger, position210)
			}
			return true
		l209:
			position, tokenIndex = position209, tokenIndex209
			return false
		},
		/* 41 Int <- <('0' / PosInt)> */
		func() bool {
			position216, tokenIndex216 := position, tokenIndex
			{
				position217 := position
				{
					position218, tokenIndex218 := position, tokenIndex
					if buffer[position] != rune('0') {
						goto l219
					}
					position++
					goto l218
				l219:
					position, tokenIndex = position218, tokenIndex218
					if !_rules[rulePosInt]() {
						goto l216
					}
				}
			l218:
				add(ruleInt, position217)
			}
			return true
		l216:
			position, tokenIndex = position216, tokenIndex216
			return false
		},
		/* 42 PosInt <- <([1-9] [0-9]*)> */
		func() bool {
			position220, tokenIndex220 := position, tokenIndex
			{
				position221 := position
				if c := buffer[position]; c < rune('1') || c > rune('9') {
					goto l220
				}
				position++
			l222:
				{
					position223, tokenIndex223 := position, tokenIndex
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l223
					}
					position++
					goto l222
				l223:
					position, tokenIndex = position223, tokenIndex223
				}
				add(rulePosInt, position221)
			}
			return true
		l220:
			position, tokenIndex = position220, tokenIndex220
			return false
		},
		/* 43 DecInt <- <(<Int> Action20)> */
		func() bool {
			position224, tokenIndex224 := position, tokenIndex
			{
				position225 := position
				{
					position226 := position
					if !_rules[ruleInt]() {
						goto l224
					}
					add(rulePegText, position226)
				}
				if !_rules[ruleAction20]() {
					goto l224
				}
				add(ruleDecInt, position225)
			}
			return true
		l224:
			position, tokenIndex = position224, tokenIndex224
			return false
		},
		/* 44 BaseInt <- <(NumBase Action21 '#' NumLit Action22)> */
		func() bool {
			position227, tokenIndex227 := position, tokenIndex
			{
				position228 := position
				if !_rules[ruleNumBase]() {
					goto l227
				}
				if !_rules[ruleAction21]() {
					goto l227
				}
				if buffer[position] != rune('#') {
					goto l227
				}
				position++
				if !_rules[ruleNumLit]() {
					goto l227
				}
				if !_rules[ruleAction22]() {
					goto l227
				}
				add(ruleBaseInt, position228)
			}
			return true
		l227:
			position, tokenIndex = position227, tokenIndex227
			return false
		},
		/* 45 NumBase <- <<((('1' / '2') [0-9]) / ('3' [0-6]) / ([2-9] ![0-9]))>> */
		func() bool {
			position229, tokenIndex229 := position, tokenIndex
			{
				position230 := position
				{
					position231 := position
					{
						position232, tokenIndex232 := position, tokenIndex
						{
							position234, tokenIndex234 := position, tokenIndex
							if buffer[position] != rune('1') {
								goto l235
							}
							position++
							goto l234
						l235:
							position, tokenIndex = position234, tokenIndex234
							if buffer[position] != rune('2') {
								goto l233
							}
							position++
						}
					l234:
						if c := buffer[position]; c < rune('0') || c > rune('9') {
							goto l233
						}
						position++
						goto l232
					l233:
						position, tokenIndex = position232, tokenIndex232
						if buffer[position] != rune('3') {
							goto l236
						}
						position++
						if c := buffer[position]; c < rune('0') || c > rune('6') {
							goto l236
						}
						position++
						goto l232
					l236:
						position, tokenIndex = position232, tokenIndex232
						if c := buffer[position]; c < rune('2') || c > rune('9') {
							goto l229
						}
						position++
						{
							position237, tokenIndex237 := position, tokenIndex
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l237
							}
							position++
							goto l229
						l237:
							position, tokenIndex = position237, tokenIndex237
						}
					}
				l232:
					add(rulePegText, position231)
				}
				add(ruleNumBase, position230)
			}
			return true
		l229:
			position, tokenIndex = position229, tokenIndex229
			return false
		},
		/* 46 NumLit <- <<([a-z] / [A-Z] / ([0-9] / [0-9]))+>> */
		func() bool {
			position238, tokenIndex238 := position, tokenIndex
			{
				position239 := position
				{
					position240 := position
					{
						position243, tokenIndex243 := position, tokenIndex
						if c := buffer[position]; c < rune('a') || c > rune('z') {
							goto l244
						}
						position++
						goto l243
					l244:
						position, tokenIndex = position243, tokenIndex243
						if c := buffer[position]; c < rune('A') || c > rune('Z') {
							goto l245
						}
						position++
						goto l243
					l245:
						position, tokenIndex = position243, tokenIndex243
						{
							position246, tokenIndex246 := position, tokenIndex
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l247
							}
							position++
							goto l246
						l247:
							position, tokenIndex = position246, tokenIndex246
							if c := buffer[position]; c < rune('0') || c > rune('9') {
								goto l238
							}
							position++
						}
					l246:
					}
				l243:
				l241:
					{
						position242, tokenIndex242 := position, tokenIndex
						{
							position248, tokenIndex248 := position, tokenIndex
							if c := buffer[position]; c < rune('a') || c > rune('z') {
								goto l249
							}
							position++
							goto l248
						l249:
							position, tokenIndex = position248, tokenIndex248
							if c := buffer[position]; c < rune('A') || c > rune('Z') {
								goto l250
							}
							position++
							goto l248
						l250:
							position, tokenIndex = position248, tokenIndex248
							{
								position251, tokenIndex251 := position, tokenIndex
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l252
								}
								position++
								goto l251
							l252:
								position, tokenIndex = position251, tokenIndex251
								if c := buffer[position]; c < rune('0') || c > rune('9') {
									goto l242
								}
								position++
							}
						l251:
						}
					l248:
						goto l241
					l242:
						position, tokenIndex = position242, tokenIndex242
					}
					add(rulePegText, position240)
				}
				add(ruleNumLit, position239)
			}
			return true
		l238:
			position, tokenIndex = position238, tokenIndex238
			return false
		},
		/* 47 HexLit <- <('0' ('x' / 'X') <HexDigit+> Action23)> */
		func() bool {
			position253, tokenIndex253 := position, tokenIndex
			{
				position254 := position
				if buffer[position] != rune('0') {
					goto l253
				}
				position++
				{
					position255, tokenIndex255 := position, tokenIndex
					if buffer[position] != rune('x') {
						goto l256
					}
					position++
					goto l255
				l256:
					position, tokenIndex = position255, tokenIndex255
					if buffer[position] != rune('X') {
						goto l253
					}
					position++
				}
			l255:
				{
					position257 := position
					if !_rules[ruleHexDigit]() {
						goto l253
					}
				l258:
					{
						position259, tokenIndex259 := position, tokenIndex
						if !_rules[ruleHexDigit]() {
							goto l259
						}
						goto l258
					l259:
						position, tokenIndex = position259, tokenIndex259
					}
					add(rulePegText, position257)
				}
				if !_rules[ruleAction23]() {
					goto l253
				}
				add(ruleHexLit, position254)
			}
			return true
		l253:
			position, tokenIndex = position253, tokenIndex253
			return false
		},
		/* 48 OctLit <- <('0' <[0-7]+> ![8-9] Action24)> */
		func() bool {
			position260, tokenIndex260 := position, tokenIndex
			{
				position261 := position
				if buffer[position] != rune('0') {
					goto l260
				}
				position++
				{
					position262 := position
					if c := buffer[position]; c < rune('0') || c > rune('7') {
						goto l260
					}
					position++
				l263:
					{
						position264, tokenIndex264 := position, tokenIndex
						if c := buffer[position]; c < rune('0') || c > rune('7') {
							goto l264
						}
						position++
						goto l263
					l264:
						position, tokenIndex = position264, tokenIndex264
					}
					add(rulePegText, position262)
				}
				{
					position265, tokenIndex265 := position, tokenIndex
					if c := buffer[position]; c < rune('8') || c > rune('9') {
						goto l265
					}
					position++
					goto l260
				l265:
					position, tokenIndex = position265, tokenIndex265
				}
				if !_rules[ruleAction24]() {
					goto l260
				}
				add(ruleOctLit, position261)
			}
			return true
		l260:
			position, tokenIndex = position260, tokenIndex260
			return false
		},
		/* 49 BinLit <- <('0' ('b' / 'B') <('0' / '1')+> ![2-9] Action25)> */
		func() bool {
			position266, tokenIndex266 := position, tokenIndex
			{
				position267 := position
				if buffer[position] != rune('0') {
					goto l266
				}
				position++
				{
					position268, tokenIndex268 := position, tokenIndex
					if buffer[position] != rune('b') {
						goto l269
					}
					position++
					goto l268
				l269:
					position, tokenIndex = position268, tokenIndex268
					if buffer[position] != rune('B') {
						goto l266
					}
					position++
				}
			l268:
				{
					position270 := position
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
							goto l266
						}
						position++
					}
				l273:
				l271:
					{
						position272, tokenIndex272 := position, tokenIndex
						{
							position275, tokenIndex275 := position, tokenIndex
							if buffer[position] != rune('0') {
								goto l276
							}
							position++
							goto l275
						l276:
							position, tokenIndex = position275, tokenIndex275
							if buffer[position] != rune('1') {
								goto l272
							}
							position++
						}
					l275:
						goto l271
					l272:
						position, tokenIndex = position272, tokenIndex272
					}
					add(rulePegText, position270)
				}
				{
					position277, tokenIndex277 := position, tokenIndex
					if c := buffer[position]; c < rune('2') || c > rune('9') {
						goto l277
					}
					position++
					goto l266
				l277:
					position, tokenIndex = position277, tokenIndex277
				}
				if !_rules[ruleAction25]() {
					goto l266
				}
				add(ruleBinLit, position267)
			}
			return true
		l266:
			position, tokenIndex = position266, tokenIndex266
			return false
		},
		/* 50 Sign <- <(('-' Action26) / ('+' Action27))> */
		func() bool {
			position278, tokenIndex278 := position, tokenIndex
			{
				position279 := position
				{
					position280, tokenIndex280 := position, tokenIndex
					if buffer[position] != rune('-') {
						goto l281
					}
					position++
					if !_rules[ruleAction26]() {
						goto l281
					}
					goto l280
				l281:
					position, tokenIndex = position280, tokenIndex280
					if buffer[position] != rune('+') {
						goto l278
					}
					position++
					if !_rules[ruleAction27]() {
						goto l278
					}
				}
			l280:
				add(ruleSign, position279)
			}
			return true
		l278:
			position, tokenIndex = position278, tokenIndex278
			return false
		},
		/* 51 Boolean <- <((True Action28) / (False Action29))> */
		func() bool {
			position282, tokenIndex282 := position, tokenIndex
			{
				position283 := position
				{
					position284, tokenIndex284 := position, tokenIndex
					if !_rules[ruleTrue]() {
						goto l285
					}
					if !_rules[ruleAction28]() {
						goto l285
					}
					goto l284
				l285:
					position, tokenIndex = position284, tokenIndex284
					if !_rules[ruleFalse]() {
						goto l282
					}
					if !_rules[ruleAction29]() {
						goto l282
					}
				}
			l284:
				add(ruleBoolean, position283)
			}
			return true
		l282:
			position, tokenIndex = position282, tokenIndex282
			return false
		},
		/* 52 TrueKw <- <(('t' 'r' 'u' 'e') / ('T' 'r' 'u' 'e') / ('T' 'R' 'U' 'E') / ('y' 'e' 's') / ('Y' 'e' 's') / ('Y' 'E' 'S'))> */
		func() bool {
			position286, tokenIndex286 := position, tokenIndex
			{
				position287 := position
				{
					position288, tokenIndex288 := position, tokenIndex
					if buffer[position] != rune('t') {
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
					goto l288
				l289:
					position, tokenIndex = position288, tokenIndex288
					if buffer[position] != rune('T') {
						goto l290
					}
					position++
					if buffer[position] != rune('r') {
						goto l290
					}
					position++
					if buffer[position] != rune('u') {
						goto l290
					}
					position++
					if buffer[position] != rune('e') {
						goto l290
					}
					position++
					goto l288
				l290:
					position, tokenIndex = position288, tokenIndex288
					if buffer[position] != rune('T') {
						goto l291
					}
					position++
					if buffer[position] != rune('R') {
						goto l291
					}
					position++
					if buffer[position] != rune('U') {
						goto l291
					}
					position++
					if buffer[position] != rune('E') {
						goto l291
					}
					position++
					goto l288
				l291:
					position, tokenIndex = position288, tokenIndex288
					if buffer[position] != rune('y') {
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
					goto l288
				l292:
					position, tokenIndex = position288, tokenIndex288
					if buffer[position] != rune('Y') {
						goto l293
					}
					position++
					if buffer[position] != rune('e') {
						goto l293
					}
					position++
					if buffer[position] != rune('s') {
						goto l293
					}
					position++
					goto l288
				l293:
					position, tokenIndex = position288, tokenIndex288
					if buffer[position] != rune('Y') {
						goto l286
					}
					position++
					if buffer[position] != rune('E') {
						goto l286
					}
					position++
					if buffer[position] != rune('S') {
						goto l286
					}
					position++
				}
			l288:
				add(ruleTrueKw, position287)
			}
			return true
		l286:
			position, tokenIndex = position286, tokenIndex286
			return false
		},
		/* 53 FalseKw <- <(('f' 'a' 'l' 's' 'e') / ('F' 'a' 'l' 's' 'e') / ('F' 'A' 'L' 'S' 'E') / ('n' 'o') / ('N' 'o') / ('N' 'O'))> */
		func() bool {
			position294, tokenIndex294 := position, tokenIndex
			{
				position295 := position
				{
					position296, tokenIndex296 := position, tokenIndex
					if buffer[position] != rune('f') {
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
					goto l296
				l297:
					position, tokenIndex = position296, tokenIndex296
					if buffer[position] != rune('F') {
						goto l298
					}
					position++
					if buffer[position] != rune('a') {
						goto l298
					}
					position++
					if buffer[position] != rune('l') {
						goto l298
					}
					position++
					if buffer[position] != rune('s') {
						goto l298
					}
					position++
					if buffer[position] != rune('e') {
						goto l298
					}
					position++
					goto l296
				l298:
					position, tokenIndex = position296, tokenIndex296
					if buffer[position] != rune('F') {
						goto l299
					}
					position++
					if buffer[position] != rune('A') {
						goto l299
					}
					position++
					if buffer[position] != rune('L') {
						goto l299
					}
					position++
					if buffer[position] != rune('S') {
						goto l299
					}
					position++
					if buffer[position] != rune('E') {
						goto l299
					}
					position++
					goto l296
				l299:
					position, tokenIndex = position296, tokenIndex296
					if buffer[position] != rune('n') {
						goto l300
					}
					position++
					if buffer[position] != rune('o') {
						goto l300
					}
					position++
					goto l296
				l300:
					position, tokenIndex = position296, tokenIndex296
					if buffer[position] != rune('N') {
						goto l301
					}
					position++
					if buffer[position] != rune('o') {
						goto l301
					}
					position++
					goto l296
				l301:
					position, tokenIndex = position296, tokenIndex296
					if buffer[position] != rune('N') {
						goto l294
					}
					position++
					if buffer[position] != rune('O') {
						goto l294
					}
					position++
				}
			l296:
				add(ruleFalseKw, position295)
			}
			return true
		l294:
			position, tokenIndex = position294, tokenIndex294
			return false
		},
		/* 54 True <- <(TrueKw !BarewordTail)> */
		func() bool {
			position302, tokenIndex302 := position, tokenIndex
			{
				position303 := position
				if !_rules[ruleTrueKw]() {
					goto l302
				}
				{
					position304, tokenIndex304 := position, tokenIndex
					if !_rules[ruleBarewordTail]() {
						goto l304
					}
					goto l302
				l304:
					position, tokenIndex = position304, tokenIndex304
				}
				add(ruleTrue, position303)
			}
			return true
		l302:
			position, tokenIndex = position302, tokenIndex302
			return false
		},
		/* 55 False <- <(FalseKw !BarewordTail)> */
		func() bool {
			position305, tokenIndex305 := position, tokenIndex
			{
				position306 := position
				if !_rules[ruleFalseKw]() {
					goto l305
				}
				{
					position307, tokenIndex307 := position, tokenIndex
					if !_rules[ruleBarewordTail]() {
						goto l307
					}
					goto l305
				l307:
					position, tokenIndex = position307, tokenIndex307
				}
				add(ruleFalse, position306)
			}
			return true
		l305:
			position, tokenIndex = position305, tokenIndex305
			return false
		},
		/* 56 Duration <- <(DurSpec !BarewordTail Action30)> */
		func() bool {
			position308, tokenIndex308 := position, tokenIndex
			{
				position309 := position
				if !_rules[ruleDurSpec]() {
					goto l308
				}
				{
					position310, tokenIndex310 := position, tokenIndex
					if !_rules[ruleBarewordTail]() {
						goto l310
					}
					goto l308
				l310:
					position, tokenIndex = position310, tokenIndex310
				}
				if !_rules[ruleAction30]() {
					goto l308
				}
				add(ruleDuration, position309)
			}
			return true
		l308:
			position, tokenIndex = position308, tokenIndex308
			return false
		},
		/* 57 DurSpec <- <<(('-' / '+')? (DurNum DurUnit)+)>> */
		func() bool {
			position311, tokenIndex311 := position, tokenIndex
			{
				position312 := position
				{
					position313 := position
					{
						position314, tokenIndex314 := position, tokenIndex
						{
							position316, tokenIndex316 := position, tokenIndex
							if buffer[position] != rune('-') {
								goto l317
							}
							position++
							goto l316
						l317:
							position, tokenIndex = position316, tokenIndex316
							if buffer[position] != rune('+') {
								goto l314
							}
							position++
						}
					l316:
						goto l315
					l314:
						position, tokenIndex = position314, tokenIndex314
					}
				l315:
					if !_rules[ruleDurNum]() {
						goto l311
					}
					if !_rules[ruleDurUnit]() {
						goto l311
					}
				l318:
					{
						position319, tokenIndex319 := position, tokenIndex
						if !_rules[ruleDurNum]() {
							goto l319
						}
						if !_rules[ruleDurUnit]() {
							goto l319
						}
						goto l318
					l319:
						position, tokenIndex = position319, tokenIndex319
					}
					add(rulePegText, position313)
				}
				add(ruleDurSpec, position312)
			}
			return true
		l311:
			position, tokenIndex = position311, tokenIndex311
			return false
		},
		/* 58 DurNum <- <(Int ('.' Int)?)> */
		func() bool {
			position320, tokenIndex320 := position, tokenIndex
			{
				position321 := position
				if !_rules[ruleInt]() {
					goto l320
				}
				{
					position322, tokenIndex322 := position, tokenIndex
					if buffer[position] != rune('.') {
						goto l322
					}
					position++
					if !_rules[ruleInt]() {
						goto l322
					}
					goto l323
				l322:
					position, tokenIndex = position322, tokenIndex322
				}
			l323:
				add(ruleDurNum, position321)
			}
			return true
		l320:
			position, tokenIndex = position320, tokenIndex320
			return false
		},
		/* 59 DurUnit <- <(('m' 's') / ('n' 's') / ('u' 's') / ('µ' 's') / 'h' / 'm' / 's')> */
		func() bool {
			position324, tokenIndex324 := position, tokenIndex
			{
				position325 := position
				{
					position326, tokenIndex326 := position, tokenIndex
					if buffer[position] != rune('m') {
						goto l327
					}
					position++
					if buffer[position] != rune('s') {
						goto l327
					}
					position++
					goto l326
				l327:
					position, tokenIndex = position326, tokenIndex326
					if buffer[position] != rune('n') {
						goto l328
					}
					position++
					if buffer[position] != rune('s') {
						goto l328
					}
					position++
					goto l326
				l328:
					position, tokenIndex = position326, tokenIndex326
					if buffer[position] != rune('u') {
						goto l329
					}
					position++
					if buffer[position] != rune('s') {
						goto l329
					}
					position++
					goto l326
				l329:
					position, tokenIndex = position326, tokenIndex326
					if buffer[position] != rune('µ') {
						goto l330
					}
					position++
					if buffer[position] != rune('s') {
						goto l330
					}
					position++
					goto l326
				l330:
					position, tokenIndex = position326, tokenIndex326
					if buffer[position] != rune('h') {
						goto l331
					}
					position++
					goto l326
				l331:
					position, tokenIndex = position326, tokenIndex326
					if buffer[position] != rune('m') {
						goto l332
					}
					position++
					goto l326
				l332:
					position, tokenIndex = position326, tokenIndex326
					if buffer[position] != rune('s') {
						goto l324
					}
					position++
				}
			l326:
				add(ruleDurUnit, position325)
			}
			return true
		l324:
			position, tokenIndex = position324, tokenIndex324
			return false
		},
		/* 60 Sentinel <- <';'> */
		func() bool {
			position333, tokenIndex333 := position, tokenIndex
			{
				position334 := position
				if buffer[position] != rune(';') {
					goto l333
				}
				position++
				add(ruleSentinel, position334)
			}
			return true
		l333:
			position, tokenIndex = position333, tokenIndex333
			return false
		},
		/* 61 Space <- <((' ' / '\t' / '\n' / '\r')+ / Comment)> */
		func() bool {
			position335, tokenIndex335 := position, tokenIndex
			{
				position336 := position
				{
					position337, tokenIndex337 := position, tokenIndex
					{
						position341, tokenIndex341 := position, tokenIndex
						if buffer[position] != rune(' ') {
							goto l342
						}
						position++
						goto l341
					l342:
						position, tokenIndex = position341, tokenIndex341
						if buffer[position] != rune('\t') {
							goto l343
						}
						position++
						goto l341
					l343:
						position, tokenIndex = position341, tokenIndex341
						if buffer[position] != rune('\n') {
							goto l344
						}
						position++
						goto l341
					l344:
						position, tokenIndex = position341, tokenIndex341
						if buffer[position] != rune('\r') {
							goto l338
						}
						position++
					}
				l341:
				l339:
					{
						position340, tokenIndex340 := position, tokenIndex
						{
							position345, tokenIndex345 := position, tokenIndex
							if buffer[position] != rune(' ') {
								goto l346
							}
							position++
							goto l345
						l346:
							position, tokenIndex = position345, tokenIndex345
							if buffer[position] != rune('\t') {
								goto l347
							}
							position++
							goto l345
						l347:
							position, tokenIndex = position345, tokenIndex345
							if buffer[position] != rune('\n') {
								goto l348
							}
							position++
							goto l345
						l348:
							position, tokenIndex = position345, tokenIndex345
							if buffer[position] != rune('\r') {
								goto l340
							}
							position++
						}
					l345:
						goto l339
					l340:
						position, tokenIndex = position340, tokenIndex340
					}
					goto l337
				l338:
					position, tokenIndex = position337, tokenIndex337
					if !_rules[ruleComment]() {
						goto l335
					}
				}
			l337:
				add(ruleSpace, position336)
			}
			return true
		l335:
			position, tokenIndex = position335, tokenIndex335
			return false
		},
		/* 62 ReqSpace <- <Space+> */
		func() bool {
			position349, tokenIndex349 := position, tokenIndex
			{
				position350 := position
				if !_rules[ruleSpace]() {
					goto l349
				}
			l351:
				{
					position352, tokenIndex352 := position, tokenIndex
					if !_rules[ruleSpace]() {
						goto l352
					}
					goto l351
				l352:
					position, tokenIndex = position352, tokenIndex352
				}
				add(ruleReqSpace, position350)
			}
			return true
		l349:
			position, tokenIndex = position349, tokenIndex349
			return false
		},
		/* 63 OptSpace <- <Space*> */
		func() bool {
			{
				position354 := position
			l355:
				{
					position356, tokenIndex356 := position, tokenIndex
					if !_rules[ruleSpace]() {
						goto l356
					}
					goto l355
				l356:
					position, tokenIndex = position356, tokenIndex356
				}
				add(ruleOptSpace, position354)
			}
			return true
		},
		/* 64 Comment <- <('\'' (!EOL .)* (EOL / EOF))> */
		func() bool {
			position357, tokenIndex357 := position, tokenIndex
			{
				position358 := position
				if buffer[position] != rune('\'') {
					goto l357
				}
				position++
			l359:
				{
					position360, tokenIndex360 := position, tokenIndex
					{
						position361, tokenIndex361 := position, tokenIndex
						if !_rules[ruleEOL]() {
							goto l361
						}
						goto l360
					l361:
						position, tokenIndex = position361, tokenIndex361
					}
					if !matchDot() {
						goto l360
					}
					goto l359
				l360:
					position, tokenIndex = position360, tokenIndex360
				}
				{
					position362, tokenIndex362 := position, tokenIndex
					if !_rules[ruleEOL]() {
						goto l363
					}
					goto l362
				l363:
					position, tokenIndex = position362, tokenIndex362
					if !_rules[ruleEOF]() {
						goto l357
					}
				}
			l362:
				add(ruleComment, position358)
			}
			return true
		l357:
			position, tokenIndex = position357, tokenIndex357
			return false
		},
		/* 65 EOL <- <(('\r' '\n') / '\n' / '\r')> */
		func() bool {
			position364, tokenIndex364 := position, tokenIndex
			{
				position365 := position
				{
					position366, tokenIndex366 := position, tokenIndex
					if buffer[position] != rune('\r') {
						goto l367
					}
					position++
					if buffer[position] != rune('\n') {
						goto l367
					}
					position++
					goto l366
				l367:
					position, tokenIndex = position366, tokenIndex366
					if buffer[position] != rune('\n') {
						goto l368
					}
					position++
					goto l366
				l368:
					position, tokenIndex = position366, tokenIndex366
					if buffer[position] != rune('\r') {
						goto l364
					}
					position++
				}
			l366:
				add(ruleEOL, position365)
			}
			return true
		l364:
			position, tokenIndex = position364, tokenIndex364
			return false
		},
		/* 66 EOF <- <!.> */
		func() bool {
			position369, tokenIndex369 := position, tokenIndex
			{
				position370 := position
				{
					position371, tokenIndex371 := position, tokenIndex
					if !matchDot() {
						goto l371
					}
					goto l369
				l371:
					position, tokenIndex = position371, tokenIndex371
				}
				add(ruleEOF, position370)
			}
			return true
		l369:
			position, tokenIndex = position369, tokenIndex369
			return false
		},
		/* 68 Action0 <- <{ p.init() }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 69 Action1 <- <{ p.beginStatement(text) }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		/* 70 Action2 <- <{ p.closeStatement() }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
		/* 71 Action3 <- <{ p.beginSection(text) }> */
		func() bool {
			{
				add(ruleAction3, position)
			}
			return true
		},
		/* 72 Action4 <- <{ p.closeSection() }> */
		func() bool {
			{
				add(ruleAction4, position)
			}
			return true
		},
		/* 73 Action5 <- <{ p.sign = 1 }> */
		func() bool {
			{
				add(ruleAction5, position)
			}
			return true
		},
		/* 74 Action6 <- <{ p.beginArray() }> */
		func() bool {
			{
				add(ruleAction6, position)
			}
			return true
		},
		/* 75 Action7 <- <{ p.closeArray() }> */
		func() bool {
			{
				add(ruleAction7, position)
			}
			return true
		},
		/* 76 Action8 <- <{ p.beginMap() }> */
		func() bool {
			{
				add(ruleAction8, position)
			}
			return true
		},
		/* 77 Action9 <- <{ p.closeMap() }> */
		func() bool {
			{
				add(ruleAction9, position)
			}
			return true
		},
		/* 78 Action10 <- <{ p.consume(Symbol(text)) }> */
		func() bool {
			{
				add(ruleAction10, position)
			}
			return true
		},
		/* 79 Action11 <- <{ p.consume(Symbol(unquote(typeSymbol, text))) }> */
		func() bool {
			{
				add(ruleAction11, position)
			}
			return true
		},
		nil,
		/* 81 Action12 <- <{ p.beginRegexp() }> */
		func() bool {
			{
				add(ruleAction12, position)
			}
			return true
		},
		/* 82 Action13 <- <{ p.closeRegexp() }> */
		func() bool {
			{
				add(ruleAction13, position)
			}
			return true
		},
		/* 83 Action14 <- <{ p.tip().(*regexpBuilder).add(text) }> */
		func() bool {
			{
				add(ruleAction14, position)
			}
			return true
		},
		/* 84 Action15 <- <{ p.tip().(*regexpBuilder).add("/") }> */
		func() bool {
			{
				add(ruleAction15, position)
			}
			return true
		},
		/* 85 Action16 <- <{ p.consume(String(unquote(typeString, text))) }> */
		func() bool {
			{
				add(ruleAction16, position)
			}
			return true
		},
		/* 86 Action17 <- <{ p.consumeRational(text) }> */
		func() bool {
			{
				add(ruleAction17, position)
			}
			return true
		},
		/* 87 Action18 <- <{ p.consumeFloat(text) }> */
		func() bool {
			{
				add(ruleAction18, position)
			}
			return true
		},
		/* 88 Action19 <- <{ p.consumeFloat(text) }> */
		func() bool {
			{
				add(ruleAction19, position)
			}
			return true
		},
		/* 89 Action20 <- <{ p.consumeInteger(text, 10) }> */
		func() bool {
			{
				add(ruleAction20, position)
			}
			return true
		},
		/* 90 Action21 <- <{ p.parseBase(text) }> */
		func() bool {
			{
				add(ruleAction21, position)
			}
			return true
		},
		/* 91 Action22 <- <{ p.consumeInteger(text, p.baseIntBase) }> */
		func() bool {
			{
				add(ruleAction22, position)
			}
			return true
		},
		/* 92 Action23 <- <{ p.consumeInteger(text, 16) }> */
		func() bool {
			{
				add(ruleAction23, position)
			}
			return true
		},
		/* 93 Action24 <- <{ p.consumeInteger(text, 8) }> */
		func() bool {
			{
				add(ruleAction24, position)
			}
			return true
		},
		/* 94 Action25 <- <{ p.consumeInteger(text, 2) }> */
		func() bool {
			{
				add(ruleAction25, position)
			}
			return true
		},
		/* 95 Action26 <- <{ p.sign = -1 }> */
		func() bool {
			{
				add(ruleAction26, position)
			}
			return true
		},
		/* 96 Action27 <- <{ p.sign = 1 }> */
		func() bool {
			{
				add(ruleAction27, position)
			}
			return true
		},
		/* 97 Action28 <- <{ p.consume(Bool(true)) }> */
		func() bool {
			{
				add(ruleAction28, position)
			}
			return true
		},
		/* 98 Action29 <- <{ p.consume(Bool(false)) }> */
		func() bool {
			{
				add(ruleAction29, position)
			}
			return true
		},
		/* 99 Action30 <- <{ p.consumeDuration(text) }> */
		func() bool {
			{
				add(ruleAction30, position)
			}
			return true
		},
	}
	p.rules = _rules
}
