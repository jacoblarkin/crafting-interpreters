import error
import scanner

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
  Grouping* = ref object of Expr
    expression: Expr

  ParseError = object of CatchableError

  Parser = ref object
    current: int
    tokens: seq[Token]

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
  for ttype in ttypes:
    if parser.check ttype:
      discard advance parser
      return true
  return false

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
  if parser.match(TokenType.NIL): return LiteralExpr[nil](value: nil)
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

proc parse(tokens: seq[Token]): Expr =
  var parser = Parser(current: 0, tokens: tokens)
  return expression parser
