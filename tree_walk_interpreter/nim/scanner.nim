import error
import hashes
import parseutils
import strutils
import tables

type
  TokenType* = enum
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, QMARK, COLON, SEMICOLON,
    SLASH, STAR, BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,
    IDENTIFIER, STRING, NUMBER,
    AND, BREAK, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL,
    OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,
    EOF

  Token* = object
    lexeme*: string
    line*: int
    case ttype*: TokenType
    of STRING: string_literal*: string
    of NUMBER: number_literal*: float
    of TRUE, FALSE: bool_literal*: bool
    else: discard

  Scanner = ref object
    current: int
    start: int
    line: int
    source: string

const
  single_operators = {'(': TokenType.LEFT_PAREN,
                      ')': TokenType.RIGHT_PAREN,
                      '{': TokenType.LEFT_BRACE,
                      '}': TokenType.RIGHT_BRACE,
                      ',': TokenType.COMMA,
                      '.': TokenType.DOT,
                      '-': TokenType.MINUS,
                      '+': TokenType.PLUS,
                      '?': TokenType.QMARK,
                      ':': TokenType.COLON,
                      ';': TokenType.SEMICOLON,
                      '/': TokenType.SLASH,
                      '*': TokenType.STAR,
                      '!': TokenType.BANG,
                      '=': TokenType.EQUAL,
                      '>': TokenType.GREATER,
                      '<': TokenType.LESS}.toTable

  double_operators = {"!=": TokenType.BANG_EQUAL,
                      "==": TokenType.EQUAL_EQUAL,
                      ">=": TokenType.GREATER_EQUAL,
                      "<=": TokenType.LESS_EQUAL}.toTable

  keywords = {"and": TokenType.AND,
              "break": TokenType.BREAK,
              "class": TokenType.CLASS,
              "else": TokenType.ELSE,
              "false": TokenType.FALSE,
              "fun": TokenType.FUN,
              "for": TokenType.FOR,
              "if": TokenType.IF,
              "nil": TokenType.NIL,
              "or": TokenType.OR,
              "print": TokenType.PRINT,
              "return": TokenType.RETURN,
              "super": TokenType.SUPER,
              "this": TokenType.THIS,
              "true": TokenType.TRUE,
              "var": TokenType.VAR,
              "while": TokenType.WHILE}.toTable

proc hash*(token: Token): Hash =
  return !$(hash(token.lexeme) !& hash(token.line))

func `$`*(t: Token): string =
  let literal: string = case t.ttype:
    of TokenType.STRING: t.string_literal
    of TokenType.NUMBER: $t.number_literal
    of TokenType.TRUE, TokenType.FALSE: $t.bool_literal
    else: ""
  return $t.ttype & " " & t.lexeme & " " & literal

func isAtEnd(scanner: Scanner): bool =
  return scanner.current >= len scanner.source

proc advance(scanner: Scanner): char =
  defer: inc scanner.current
  return scanner.source[scanner.current]

proc match(scanner: Scanner, expected: char): bool =
  if isAtEnd scanner: return false
  if scanner.source[scanner.current] != expected: return false
  inc scanner.current
  return true

func peek(scanner: Scanner): char =
  return if isAtEnd scanner: '\0' else: scanner.source[scanner.current]

func peekNext(scanner: Scanner): char =
  return if scanner.current+1 >= len scanner.source: '\0'
         else: scanner.source[scanner.current+1]

func makeToken[T](scanner: Scanner, ttype: TokenType, literal: T): Token =
  let lexeme = scanner.source[scanner.start ..< scanner.current]
  let line = scanner.line
  when T is float:
    if ttype == TokenType.NUMBER:
      return Token(lexeme: lexeme, line: line, ttype: TokenType.NUMBER,
          number_literal: literal)
  elif T is string:
    if ttype == TokenType.STRING:
      return Token(lexeme: lexeme, line: line, ttype: TokenType.STRING,
          string_literal: literal)
  elif T is bool:
    if ttype == TokenType.TRUE:
      return Token(lexeme: lexeme, line: line, ttype: TokenType.TRUE,
          bool_literal: literal)
    elif ttype == TokenType.FALSE:
      return Token(lexeme: lexeme, line: line, ttype: TokenType.FALSE,
          bool_literal: literal)
  return Token(lexeme: lexeme, line: line, ttype: ttype)

func makeToken(scanner: Scanner, ttype: TokenType): Token =
  return makeToken(scanner, ttype, nil)

proc scanString(scanner: Scanner): Token =
  while scanner.peek != '"' and not isAtEnd scanner:
    if scanner.peek == '\n': inc scanner.line
    discard advance scanner
  if not isAtEnd scanner:
    discard advance scanner
    return makeToken(scanner, TokenType.STRING, scanner.source[
        scanner.start+1 ..< scanner.current-1])
  error scanner.line, "Unterminated string"
  return Token(lexeme: "", line: scanner.line, ttype: TokenType.EOF)

proc scanNumber(scanner: Scanner, foundDecPt: bool = false): Token =
  while not foundDecPt and scanner.peek in Digits:
    discard advance scanner
  if foundDecPt or (scanner.peek == '.' and scanner.peekNext in Digits):
    discard advance scanner
    while scanner.peek in Digits:
      discard advance scanner
  var val: float
  discard parseFloat(scanner.source[scanner.start ..< scanner.current], val)
  return makeToken(scanner, TokenType.NUMBER, val)


proc scanToken(scanner: Scanner): Token =
  var c: char = advance scanner
  var dc: string = if not isAtEnd scanner:
                     scanner.source[scanner.start .. scanner.current]
                   else:
                     ""
  if dc in double_operators:
    discard advance scanner
    return makeToken(scanner, double_operators[dc])
  elif dc == "//":
    while scanner.peek != '\n' and not isAtEnd scanner:
      discard advance scanner
  elif c == '.' and len(dc) == 2 and dc[1] in Digits:
    return scanner.scanNumber true
  elif c in single_operators:
    return makeToken(scanner, single_operators[c])
  elif c == '\n':
    inc scanner.line
  elif c in Whitespace:
    discard
  elif c == '"':
    return scanner.scanString
  elif c in Digits:
    return scanner.scanNumber
  elif c in IdentStartChars:
    while scanner.peek in IdentChars:
      discard advance scanner
    let text: string = scanner.source[scanner.start ..< scanner.current]
    if text == "true":
      return makeToken(scanner, TokenType.TRUE, true)
    elif text == "false":
      return makeToken(scanner, TokenType.FALSE, false)
    elif text in keywords:
      return makeToken(scanner, keywords[text])
    else:
      return makeToken(scanner, TokenType.IDENTIFIER)
  else: error scanner.line, "Unexpected character, " & c
  return Token(lexeme: "", line: scanner.line, ttype: TokenType.EOF)

proc scanTokens*(source: string): seq[Token] =
  var tokens: seq[Token] = @[]
  var scanner = Scanner(current: 0, start: 0, line: 1, source: source)
  while not isAtEnd scanner:
    let tok = scanToken scanner
    if tok.ttype != TokenType.EOF:
      tokens.add tok
    scanner.start = scanner.current
  tokens.add Token(lexeme: "", line: scanner.line, ttype: TokenType.EOF)
  return tokens

