import error
import hashes
import sequtils
import scanner
import sugar

type
  Expr* = ref object of RootObj
  BinaryExpr* = ref object of Expr
    left*: Expr
    right*: Expr
    operator*: Token
  UnaryExpr* = ref object of Expr
    operator*: Token
    right*: Expr
  LiteralType* = enum
    NumberLiteralExpr,
    StringLiteralExpr,
    BoolLiteralExpr,
    NilLiteralExpr
  LiteralExpr* = ref object of Expr
    case ltype*: LiteralType:
      of NumberLiteralExpr: num*: float
      of StringLiteralExpr: str*: string
      of BoolLiteralExpr: bl*: bool
      of NilLiteralExpr: discard
  Grouping* = ref object of Expr
    expression*: Expr
  VariableExpr* = ref object of Expr
    name*: Token
  AssignmentExpr* = ref object of Expr
    name*: Token
    value*: Expr
  CallExpr* = ref object of Expr
    callee*: Expr
    paren*: Token
    arguments*: seq[Expr]
  AnonFnExpr* = ref object of Expr
    params*: seq[Token]
    body*: seq[Stmt]
  GetExpr* = ref object of Expr
    obj*: Expr
    name*: Token
  SetExpr* = ref object of Expr
    obj*: Expr
    name*: Token
    value*: Expr
  ThisExpr* = ref object of Expr
    keyword*: Token
  SuperExpr* = ref object of Expr
    keyword*: Token
    calledMethod*: Token

  Stmt* = ref object of RootObj
  ExpressionStmt* = ref object of Stmt
    expression*: Expr
  PrintStmt* = ref object of Stmt
    expression*: Expr
  VarDecl* = ref object of Stmt
    name*: Token
    initializer*: Expr
  BlockStmt* = ref object of Stmt
    statements*: seq[Stmt]
  IfStmt* = ref object of Stmt
    condition*: Expr
    thenBranch*: Stmt
    elseBranch*: Stmt
  WhileStmt* = ref object of Stmt
    condition*: Expr
    body*: Stmt
  Function* = ref object of Stmt
    name*: Token
    params*: seq[Token]
    body*: seq[Stmt]
  ReturnStmt* = ref object of Stmt
    keyword*: Token
    value*: Expr
  ClassDecl* = ref object of Stmt
    name*: Token
    superclass*: VariableExpr
    methods*: seq[Function]
    classMethods*: seq[Function]
    attrs*: seq[Function]

  ParseError = object of CatchableError

  Parser = ref object
    current: int
    tokens: seq[Token]

var spaces: int = 0

method hash*(expression: Expr): Hash {.base.} =
  raise newException(Defect, "Cannot has this expression type: " & $Expr)

method hash*(expression: VariableExpr): Hash =
  return hash(expression.name)

method hash*(expression: AssignmentExpr): Hash =
  return hash(expression.name)

method hash*(expression: ThisExpr): Hash =
  return hash(expression.keyword)

method hash*(expression: SuperExpr): Hash =
  return hash(expression.keyword)

method toString*(expression: Expr): string {.base.} =
  return "Expr\n"

method toString*(statement: Stmt): string {.base.} =
  return "Stmt\n"

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

method toString*(expression: LiteralExpr): string =
  let val: string = case expression.ltype:
    of NumberLiteralExpr: $expression.num
    of StringLiteralExpr: expression.str
    of BoolLiteralExpr: $expression.bl
    of NilLiteralExpr: "nil"
  return "LiteralExpr (type: " & $expression.ltype & " value: " & val & ")"

method toString*(expression: Grouping): string =
  result = "Grouping\n"
  spaces += 2
  var s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= expression.expression.toString
  spaces -= 2

method toString*(expression: VariableExpr): string =
  result = "Variable (name: " & expression.name.lexeme & ")"

method toString*(expression: AssignmentExpr): string =
  result = "Assign (var: " & expression.name.lexeme & ")\n"
  spaces += 2
  var s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= expression.value.toString
  spaces -= 2

method toString*(expression: CallExpr): string =
  result = "Call:\n"
  spaces += 2
  var s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= "Callee: " & expression.callee.toString
  var counter = 0
  for arg in expression.arguments:
    result &= "\n"
    counter += 1
    s = 0
    while s < spaces:
      result &= " "
      inc s
    result &= "Argument #" & $counter & ": "
    result &= arg.toString
  spaces -= 2

method toString*(expression: AnonFnExpr): string =
  result = "Anon Function Declaration"
  if expression.params.len > 0:
    result &= ", params:"
    for param in expression.params:
      result &= " " & param.lexeme
  result &= "\n"
  spaces += 2
  var counter = 0
  for thisStmt in expression.body:
    counter += 1
    var s = 0
    while s < spaces:
      result &= " "
      inc s
    result &= thisStmt.toString & (if counter <
        expression.body.len: "\n" else: "")
  spaces -= 2

method toString*(expression: GetExpr): string =
  result = "Get " & expression.name.lexeme & "\n"
  spaces += 2
  var s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= "from: " & expression.obj.toString
  spaces -= 2

method toString*(expression: SetExpr): string =
  result = "Set " & expression.name.lexeme & " from \n"
  spaces += 2
  var s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= "from: " & expression.obj.toString & "\n"
  s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= "with: " & expression.value.toString
  spaces -= 2

method toString*(expression: ThisExpr): string =
  result = "This"

method toString*(expression: SuperExpr): string =
  result = "Superclass call " & expression.calledMethod.lexeme

func `$`*(expression: Expr): string =
  return toString expression

method toString*(statement: ExpressionStmt): string =
  result = "Expression:\n"
  spaces += 2
  var s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= $statement.expression
  spaces -= 2

method toString*(statement: PrintStmt): string =
  result = "Print:\n"
  spaces += 2
  var s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= $statement.expression
  spaces -= 2

method toString*(statement: VarDecl): string =
  result = "Variable Declaration (name: " & statement.name.lexeme & ")"
  if statement.initializer != nil:
    result &= "\n  Initialize:\n"
    spaces += 4
    var s = 0
    while s < spaces:
      result &= " "
      inc s
    result &= $statement.initializer
    spaces -= 4

method toString*(statement: BlockStmt): string =
  result = "Block:\n"
  spaces += 2
  var counter = 0
  for thisStmt in statement.statements:
    counter += 1
    var s = 0
    while s < spaces:
      result &= " "
      inc s
    result &= thisStmt.toString & (if counter <
        statement.statements.len: "\n" else: "")
  spaces -= 2

method toString*(statement: IfStmt): string =
  let hasElse: bool = statement.elseBranch != nil
  result = "If (has else: " & $hasElse & ")\n"
  spaces += 2
  var s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= "condition: " & statement.condition.toString & "\n"
  s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= "then:\n"
  s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= statement.thenBranch.toString & (if hasElse: "\n" else: "")
  if hasElse:
    s = 0
    while s < spaces:
      result &= " "
      inc s
    result &= "else:\n"
    s = 0
    while s < spaces:
      result &= " "
      inc s
    result &= statement.elseBranch.toString
  spaces -= 2

method toString*(statement: WhileStmt): string =
  result = "While"
  var s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= "condition: " & statement.condition.toString & "\n"
  s = 0
  while s < spaces:
    result &= " "
    inc s
  result &= statement.body.toString

method toString*(statement: Function, ftype: string = "Function"): string =
  result = ftype & " Declaration (name: " & statement.name.lexeme
  if statement.params.len > 0:
    result &= ", params:"
    for param in statement.params:
      result &= " " & param.lexeme
  result &= ")\n"
  spaces += 2
  var counter = 0
  for thisStmt in statement.body:
    counter += 1
    var s = 0
    while s < spaces:
      result &= " "
      inc s
    result &= thisStmt.toString & (if counter <
        statement.body.len: "\n" else: "")
  spaces -= 2

method toString*(statement: ReturnStmt): string =
  result = "Return"
  if statement.value != nil:
    result &= "\n"
    spaces += 2
    var s = 0
    while s < spaces:
      result &= " "
      inc s
    result &= statement.value.toString
    spaces -= 2

method toString*(statement: ClassDecl): string =
  result = "Class (name: " & statement.name.lexeme
  if statement.superclass != nil:
    result &= " superclass: " & statement.superclass.name.lexeme
  result &= ")"
  spaces += 2
  for thisMethod in statement.methods:
    result &= "\n"
    var s = 0
    while s < spaces:
      result &= " "
      inc s
    result &= thisMethod.toString "Method"
  for thisMethod in statement.classMethods:
    result &= "\n"
    var s = 0
    while s < spaces:
      result &= " "
      inc s
    result &= thisMethod.toString "Class Method"
  for thisMethod in statement.attrs:
    result &= "\n"
    var s = 0
    while s < spaces:
      result &= " "
      inc s
    result &= thisMethod.toString "Attribute"
  spaces -= 2

func `$`*(statement: Stmt): string =
  return toString statement

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

func checkNext(parser: Parser, ttype: TokenType): bool =
  return if isAtEnd parser: false else: parser.tokens[parser.current+1].ttype == ttype

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

proc parseBlock(parser: Parser): seq[Stmt]

proc anonFun(parser: Parser): Expr =
  discard parser.consume(TokenType.LEFT_PAREN, "Expect '(' before params in anonymous function.")
  var params: seq[Token] = @[]
  if not parser.check(TokenType.RIGHT_PAREN):
    params &= parser.consume(TokenType.IDENTIFIER, "Expect parameter name.")
    while parser.match(TokenType.COMMA):
      params &= parser.consume(TokenType.IDENTIFIER, "Expect parameter name.")
  discard parser.consume(TokenType.RIGHT_PAREN, "Expect ')' after params")
  discard parser.consume(TokenType.LEFT_BRACE, "Expect '{' before body")
  let body = parseBlock parser
  return AnonFnExpr(params: params, body: body)

proc primary(parser: Parser): Expr =
  if parser.match(TokenType.FALSE, TokenType.TRUE):
    return LiteralExpr(ltype: BoolLiteralExpr, bl: parser.previous.bool_literal)
  if parser.match(TokenType.NIL):
    return LiteralExpr(ltype: NilLiteralExpr)
  if parser.match(TokenType.NUMBER):
    return LiteralExpr(ltype: NumberLiteralExpr,
        num: parser.previous.number_literal)
  if parser.match(TokenType.STRING):
    return LiteralExpr(ltype: StringLiteralExpr,
        str: parser.previous.string_literal)
  if parser.match(TokenType.LEFT_PAREN):
    let expression = expression(parser)
    discard consume(parser, TokenType.RIGHT_PAREN, "Expect ')' after expression.")
    return Grouping(expression: expression)
  if parser.match(TokenType.IDENTIFIER):
    return VariableExpr(name: parser.previous)
  if parser.match(TokenType.FUN):
    return anonFun(parser)
  if parser.match(TokenType.THIS):
    return ThisExpr(keyword: parser.previous)
  if parser.match(TokenType.SUPER):
    let keyword = parser.previous
    discard parser.consume(TokenType.DOT, "Expect '.' after 'super'.")
    let calledMethod = parser.consume(TokenType.IDENTIFIER, "Expect superclass method name.")
    return SuperExpr(keyword: keyword, calledMethod: calledMethod)
  raise parseError(parser.peek, "Expect expression.")

proc finishCall(parser: Parser, callee: Expr): Expr =
  var arguments: seq[Expr] = @[]
  if not parser.check(TokenType.RIGHT_PAREN):
    arguments &= parser.expression
    while parser.match(TokenType.COMMA):
      arguments &= parser.expression
  let paren = parser.consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.")
  if arguments.len > 255:
    raise parseError(paren, "Maximum of 255 arguments allowed. Found " &
        $arguments.len & ".")
  return CallExpr(callee: callee, paren: paren, arguments: arguments)

proc call(parser: Parser): Expr =
  result = primary parser
  while parser.match(TokenType.LEFT_PAREN, TokenType.DOT):
    case parser.previous.ttype:
      of TokenType.LEFT_PAREN:
        result = parser.finishCall(result)
      of TokenType.DOT:
        let name = parser.consume(TokenType.IDENTIFIER, "Expect property name after '.'.")
        result = GetExpr(obj: result, name: name)
      else:
        raise parseError(parser.previous, "Should never reach this point!!!")

proc unary(parser: Parser): Expr =
  if parser.match(TokenType.BANG, TokenType.MINUS):
    let operator = previous parser
    let right = unary parser
    return UnaryExpr(operator: operator, right: right)
  return call parser

template parseBinaryExpr(name, next: untyped, ttypes: varargs[TokenType]) =
  proc name(parser: Parser): Expr =
    result = next parser
    while parser.match(ttypes):
      let operator = previous parser
      let right = next parser
      result = BinaryExpr(left: result, right: right, operator: operator)

parseBinaryExpr(factor, unary, TokenType.STAR, TokenType.SLASH)
parseBinaryExpr(term, factor, TokenType.PLUS, TokenType.MINUS)
parseBinaryExpr(comparison, term, TokenType.GREATER, TokenType.GREATER_EQUAL,
    TokenType.LESS, TokenType.LESS_EQUAL)
parseBinaryExpr(equality, comparison, TokenType.BANG_EQUAL,
    TokenType.EQUAL_EQUAL)
parseBinaryExpr(parseAnd, equality, TokenType.AND)
parseBinaryExpr(parseOr, parseAnd, TokenType.OR)

proc assignment(parser: Parser): Expr =
  let expression = parseOr parser
  if parser.match(TokenType.EQUAL):
    let equals = parser.previous
    let value = assignment parser
    if expression of VariableExpr:
      return AssignmentExpr(name: VariableExpr(expression).name, value: value)
    elif expression of GetExpr:
      let tmp = GetExpr(expression)
      return SetExpr(obj: tmp.obj, name: tmp.name, value: value)
    raise parseError(equals, "Invalid lvalue.")
  return expression

proc expression(parser: Parser): Expr =
  return assignment parser

proc printStatement(parser: Parser): Stmt =
  let value = expression parser
  discard parser.consume(TokenType.SEMICOLON, "Expect ';' after value.")
  return PrintStmt(expression: value)

proc expressionStatement(parser: Parser): Stmt =
  let expression = expression parser
  discard parser.consume(TokenType.SEMICOLON, "Expect ';' after expression.")
  return ExpressionStmt(expression: expression)

proc statement(parser: Parser): Stmt

proc ifStatement(parser: Parser): Stmt =
  discard parser.consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
  let condition = expression parser
  discard parser.consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.")
  let thenBranch = statement parser
  let elseBranch = if parser.match(TokenType.ELSE): statement parser else: nil
  return IfStmt(condition: condition, thenBranch: thenBranch,
      elseBranch: elseBranch)

proc whileStatement(parser: Parser): Stmt =
  discard parser.consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.")
  let condition = expression parser
  discard parser.consume(TokenType.RIGHT_PAREN, "Expect ')' after while condition.")
  let body = statement parser
  return WhileStmt(condition: condition, body: body)

proc varDeclaration(parser: Parser): Stmt

proc forStatement(parser: Parser): Stmt =
  discard parser.consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.")
  let initializer = if parser.match(TokenType.SEMICOLON): nil elif parser.match(
      TokenType.VAR): varDeclaration(parser) else: expressionStatement(parser)
  let condition = if not parser.check(TokenType.SEMICOLON): expression(
      parser) else: LiteralExpr(ltype: BoolLiteralExpr, bl: true)
  discard parser.consume(TokenType.SEMICOLON, "Expect ';' after loop condition.")
  let increment = if not parser.check(TokenType.RIGHT_PAREN): expression(
      parser) else: nil
  discard parser.consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.")
  result = statement parser
  if increment != nil:
    result = BlockStmt(statements: @[result, ExpressionStmt(
        expression: increment)])
  result = WhileStmt(condition: condition, body: result)
  if initializer != nil:
    result = BlockStmt(statements: @[initializer, result])

proc function(parser: Parser, kind: string): Stmt =
  let name = parser.consume(TokenType.IDENTIFIER, "Expect " & kind & " name.")
  if kind != "getAttr":
    discard parser.consume(TokenType.LEFT_PAREN, "Expect '(' after " & kind & " name.")
  var parameters: seq[Token] = @[]
  if not parser.check(TokenType.RIGHT_PAREN) and kind != "getAttr":
    parameters &= parser.consume(TokenType.IDENTIFIER, "Expect parameter name.")
    while parser.match(TokenType.COMMA):
      parameters &= parser.consume(TokenType.IDENTIFIER, "Expect parameter name.")
      if parameters.len > 255:
        raise parseError(parser.peek, "Maximum of 255 arguments allowed. Found " &
            $parameters.len & ".")
  if kind != "getAttr":
    discard parser.consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.")
  discard parser.consume(TokenType.LEFT_BRACE, "Expect '{' before " & kind & " body.")
  let body = parseBlock parser
  return Function(name: name, params: parameters, body: body)

proc parseReturn(parser: Parser): Stmt =
  let keyword = previous parser
  let value = if not parser.check(TokenType.SEMICOLON): parser.expression else: nil
  discard parser.consume(TokenType.SEMICOLON, "Expect ';' after return value.")
  return ReturnStmt(keyword: keyword, value: value)

proc statement(parser: Parser): Stmt =
  if parser.match(TokenType.PRINT):
    return printStatement parser
  elif parser.match(TokenType.LEFT_BRACE):
    return BlockStmt(statements: parser.parseBlock)
  elif parser.match(TokenType.IF):
    return ifStatement parser
  elif parser.match(TokenType.WHILE):
    return whileStatement parser
  elif parser.match(TokenType.FOR):
    return forStatement parser
  elif parser.match(TokenType.RETURN):
    return parseReturn parser
  return expressionStatement parser

proc varDeclaration(parser: Parser): Stmt =
  let name = parser.consume(TokenType.IDENTIFIER, "Expect variable name.")
  var initializer: Expr = nil
  if parser.match(TokenType.EQUAL):
    initializer = expression parser
  discard parser.consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.")
  return VarDecl(name: name, initializer: initializer)

proc classDeclaration(parser: Parser): Stmt =
  let name = parser.consume(TokenType.IDENTIFIER, "Expect class name.")
  var superclass: VariableExpr = nil
  if parser.match(TokenType.LESS):
    discard parser.consume(TokenType.IDENTIFIER, "Expect superclass name.")
    superclass = VariableExpr(name: parser.previous)
  discard parser.consume(TokenType.LEFT_BRACE, "Expect '{' before class body.")
  var methods: seq[Function] = @[]
  var classMethods: seq[Function] = @[]
  var attrs: seq[Function] = @[]
  while not parser.check(TokenType.RIGHT_BRACE) and not parser.isAtEnd:
    if parser.match(TokenType.CLASS):
      classMethods.add(Function(function(parser, "method")))
    elif parser.checkNext(TokenType.LEFT_PAREN):
      methods.add(Function(function(parser, "method")))
    else:
      attrs.add(Function(function(parser, "getAttr")))
  discard parser.consume(TokenType.RIGHT_BRACE, "Expect '}' after class body.")
  return ClassDecl(name: name, superclass: superclass, methods: methods,
      classMethods: classMethods, attrs: attrs)

proc declaration(parser: Parser): Stmt =
  try:
    if parser.match(TokenType.VAR):
      return varDeclaration parser
    elif parser.check(TokenType.FUN) and parser.checkNext(TokenType.IDENTIFIER):
      discard advance parser
      return function(parser, "function")
    elif parser.match(TokenType.CLASS):
      return classDeclaration parser
    return statement parser
  except ParseError:
    parser.synchronize

proc parseBlock(parser: Parser): seq[Stmt] =
  while not parser.check(TokenType.RIGHT_BRACE) and not parser.isAtEnd:
    result &= declaration(parser)
  discard parser.consume(TokenType.RIGHT_BRACE, "Expect '}' after block.")

proc parse*(tokens: seq[Token]): seq[Stmt] =
  var parser = Parser(current: 0, tokens: tokens)
  while not isAtEnd parser:
    result &= declaration parser
