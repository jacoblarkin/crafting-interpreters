import error
import sequtils
import scanner
import sugar

type
  Expr* = ref object of RootObj
  BinaryExpr* = ref object of Expr
    left: Expr
    right: Expr
    operator: Token
  UnaryExpr* = ref object of Expr
    operator: Token
    right: Expr
  LiteralExpr*[T] = ref object of Expr
    value: T
  NilExpr* = ref object of Expr
  Grouping* = ref object of Expr
    expression: Expr

  ParseError = object of CatchableError

  Parser = ref object
    current: int
    tokens: seq[Token]

var spaces: int = 0

method toString*(expression: Expr): string =
  return "Expr\n"

method toString*(expression: BinaryExpr): string =
  result = "BinaryExpr (op: " & expression.operator.lexeme & ")\n"
  spaces += 2
  var s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= expression.left.toString & "\n"
  s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= expression.right.toString
  spaces -= 2

method toString*(expression: UnaryExpr): string =
  result = "UnaryExpr (op: " & expression.operator.lexeme & ")\n"
  spaces += 2
  var s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= expression.right.toString
  spaces -= 2

method toString*(expression: NilExpr): string =
  return "NIL"

method toString*[T: string | float | bool](expression: LiteralExpr[T]): string =
  return "LiteralExpr (value: " & $expression.value & ")"

method toString*(expression: Grouping): string =
  result = "Grouping\n"
  spaces += 2
  var s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= expression.expression.toString
  spaces -= 2

func `$`*(expression: Expr): string =
  return toString expression

func previous(parser: Parser): Token =
  return parser.tokens[parser.current - 1]

func peek(parser: Parser): Token =
  return parser.tokens[parser.current]

func isAtEnd(parser: Parser): bool =
  return parser.peek.ttype == TokenType.EOF

proc advance(parser: Parser): Token =
  if not isAtEnd parser:
    inc parser.current
  return previous parser

func check(parser: Parser, ttype: TokenType): bool =
  return if isAtEnd parser: false else: parser.peek.ttype == ttype

proc match(parser: Parser, ttypes: varargs[TokenType]): bool =
  let matched = ttypes.toSeq.any(x => parser.check(x))
  if matched: discard advance parser
  return matched

proc synchronize(parser: Parser) =
  discard advance parser
  while not isAtEnd parser:
    if parser.previous.ttype == TokenType.SEMICOLON: return
    case parser.peek.ttype:
      of TokenType.CLASS,
         TokenType.FUN,
         TokenType.VAR,
         TokenType.FOR,
         TokenType.IF,
         TokenType.WHILE,
         TokenType.PRINT,
         TokenType.RETURN:
        return
      else:
        discard advance parser

proc parseError(token: Token, message: string): ref ParseError =
  if token.ttype == TokenType.EOF:
    report token.line, " at end", message
  else:
    report token.line, " at '" & token.lexeme & "'", message
  var e: ref ParseError
  new e
  e.msg = ""
  return e

proc consume(parser: Parser, ttype: TokenType, message: string): Token =
  if parser.check ttype: return advance parser
  raise parseError(parser.peek, message)

proc expression(parser: Parser): Expr

proc primary(parser: Parser): Expr =
  if parser.match(TokenType.FALSE, TokenType.TRUE):
    return LiteralExpr[bool](value: parser.previous.bool_literal)
  if parser.match(TokenType.NIL): return NilExpr()
  if parser.match(TokenType.NUMBER):
    return LiteralExpr[float](value: parser.previous.number_literal)
  if parser.match(TokenType.STRING):
    return LiteralExpr[string](value: parser.previous.string_literal)
  if parser.match(TokenType.LEFT_PAREN):
    let expression = expression(parser)
    discard consume(parser, TokenType.RIGHT_PAREN, "Expect ')' after expression.")
    return Grouping(expression: expression)
  raise parseError(parser.peek, "Expect expression.")

proc unary(parser: Parser): Expr =
  if parser.match(TokenType.BANG, TokenType.MINUS):
    let operator = previous parser
    let right = unary parser
    return UnaryExpr(operator: operator, right: right)
  return primary parser

template parseBinaryExpr(name, next: untyped, ttypes: varargs[TokenType]) =
  proc name(parser: Parser): Expr =
    result = next parser
    while parser.match(ttypes):
      let operator = previous parser
      let right = next parser
      result = BinaryExpr(left: result, right: right, operator: operator)

parseBinaryExpr(factor, unary, TokenType.STAR, TokenType.SLASH)
parseBinaryExpr(term, factor, TokenType.PLUS, TokenType.MINUS)
parseBinaryExpr(comparison, term, TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)
parseBinaryExpr(equality, comparison, TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)

proc expression(parser: Parser): Expr =
  return equality parser

proc parse*(tokens: seq[Token]): Expr =
  var parser = Parser(current: 0, tokens: tokens)
  try:
    return expression parser
  except ParseError:
    return nil
