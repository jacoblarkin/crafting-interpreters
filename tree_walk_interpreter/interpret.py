import argparse
import sys

from abc import ABC, abstractmethod
from typing import Any
from dataclasses import dataclass
from enum import Enum, auto
from time import time_ns

hadError = False
hadRuntimeError = False

class TokenType(Enum):
  LEFT_PAREN = auto()
  RIGHT_PAREN = auto()
  LEFT_BRACE = auto()
  RIGHT_BRACE = auto()
  COMMA = auto()
  DOT = auto()
  MINUS = auto()
  PLUS = auto()
  QMARK = auto()
  COLON = auto()
  SEMICOLON = auto()
  SLASH = auto()
  STAR = auto()
  BANG = auto()
  BANG_EQUAL = auto()
  EQUAL = auto()
  EQUAL_EQUAL = auto()
  GREATER = auto()
  GREATER_EQUAL = auto()
  LESS = auto()
  LESS_EQUAL = auto()
  IDENTIFIER = auto()
  STRING = auto()
  NUMBER = auto()
  AND = auto()
  BREAK = auto()
  CLASS = auto()
  ELSE = auto()
  FALSE = auto()
  FUN = auto()
  FOR = auto()
  IF = auto()
  NIL = auto()
  OR = auto()
  PRINT = auto()
  RETURN = auto()
  SUPER = auto()
  THIS = auto()
  TRUE = auto()
  VAR = auto()
  WHILE = auto()
  EOF = auto()

@dataclass(frozen=True)
class Token:
    ttype: TokenType
    lexeme: str
    literal: Any
    line: int
    pos: int

class Scanner:
    single_token_map = {'(': TokenType.LEFT_PAREN, 
                        ')': TokenType.RIGHT_PAREN,
                        '{': TokenType.LEFT_BRACE,
                        '}': TokenType.RIGHT_BRACE,
                        ',': TokenType.COMMA,
                        '.': TokenType.DOT,
                        '-': TokenType.MINUS,
                        '+': TokenType.PLUS,
                        ';': TokenType.SEMICOLON,
                        '?': TokenType.QMARK,
                        ':': TokenType.COLON,
                        '*': TokenType.STAR}
    maybe_two_token_map = {'!': ('=', TokenType.BANG_EQUAL, TokenType.BANG),
                           '=': ('=', TokenType.EQUAL_EQUAL, TokenType.EQUAL),
                           '<': ('=', TokenType.LESS_EQUAL, TokenType.LESS),
                           '>': ('=', TokenType.GREATER_EQUAL, TokenType.GREATER)}
    keywords_token_map = {'and': TokenType.AND,
                          'break': TokenType.BREAK,
                          'class': TokenType.CLASS,
                          'else': TokenType.ELSE,
                          'false': TokenType.FALSE,
                          'for': TokenType.FOR,
                          'fun': TokenType.FUN,
                          'if': TokenType.IF,
                          'nil': TokenType.NIL,
                          'or': TokenType.OR,
                          'print': TokenType.PRINT,
                          'return': TokenType.RETURN,
                          'super': TokenType.SUPER,
                          'this': TokenType.THIS,
                          'true': TokenType.TRUE,
                          'var': TokenType.VAR,
                          'while': TokenType.WHILE}

    def __init__(self, source):
        self.source = source
        self.tokens = []
        self.current = 0
        self.start = 0
        self.line = 1

    def scanToken(self):
        c = self.source[self.current]
        self.current += 1
        if c in Scanner.single_token_map:
            self.tokens.append(Token(Scanner.single_token_map[c], c, None, self.line, self.current))
        elif c in Scanner.maybe_two_token_map:
            ttype = Scanner.maybe_two_token_map[c][2]
            if self.current < len(self.source) and \
               self.source[self.current] == Scanner.maybe_two_token_map[c][0]:
                ttype = Scanner.maybe_two_token_map[c][1]
                c += self.source[self.current]
                self.current += 1
            self.tokens.append(Token(ttype, c, None, self.line, self.current))
        elif c == '/':
            if self.current < len(self.source) and \
               self.source[self.current] == '/':
                while c != '\n' and self.current < len(self.source):
                    c = self.source[self.current]
                    self.current += 1
                self.line += 1
            elif self.current < len(self.source) and \
                    self.source[self.current] == '*':
                self.current += 1
                while self.current < len(self.source):
                    c = self.source[self.current]
                    self.current += 1
                    if c == '*' and self.source[self.current] == '/':
                        self.current += 1
                        break
                    if c == '\n': self.line += 1
            else:
                self.tokens.append(Token(TokenType.SLASH, '/', None, self.line, self.current))
        elif c in ' \r\t':
            pass
        elif c == '\n':
            self.line += 1
        elif c == '"':
            if self.current >= len(self.source): error(self.line, 'No closing "')
            c = self.source[self.current]
            self.current += 1
            while c != '"' and self.current < len(self.source):
                if c == '\n': self.line += 1
                c = self.source[self.current]
                self.current += 1
            val = None
            val = self.source[self.start:self.current]
            self.tokens.append(Token(TokenType.STRING, val, val[1:-1], self.line, self.current))
        elif c.isdecimal():
            while c.isdecimal() and self.current < len(self.source):
                c = self.source[self.current]
                self.current += 1
            if c == '.' and self.current < len(self.source) and \
                    self.source[self.current].isdecimal():
                c = self.source[self.current]
                self.current += 1
                while c.isdecimal() and self.current < len(self.source):
                    c = self.source[self.current]
                    self.current += 1
            if self.current < len(self.source) or not c.isdecimal(): self.current -= 1
            val = self.source[self.start:self.current]
            self.tokens.append(Token(TokenType.NUMBER, val, float(val), self.line, self.current))
        elif c.isalpha() or c == '_':
            while (c.isalpha() or c.isdecimal() or c == '_') and self.current < len(self.source):
                c = self.source[self.current]
                self.current += 1
            if self.current < len(self.source) or not (c.isalpha() or c.isdecimal() or c == '_'): 
                self.current -= 1
            val = self.source[self.start:self.current]
            if val in Scanner.keywords_token_map:
                if val == 'false':
                    self.tokens.append(Token(Scanner.keywords_token_map[val], val, False, self.line, self.current))
                elif val == 'true':
                    self.tokens.append(Token(Scanner.keywords_token_map[val], val, True, self.line, self.current))
                else:
                    self.tokens.append(Token(Scanner.keywords_token_map[val], val, None, self.line, self.current))
            else:
                self.tokens.append(Token(TokenType.IDENTIFIER, val, None, self.line, self.current))
        else:
            error(self.line, f'Unexpected character: {c}')

    def scanTokens(self):
        while self.current < len(self.source):
            self.start = self.current
            self.scanToken()
        self.tokens.append(Token(TokenType.EOF, "", None, self.line, self.current))
        return self.tokens

class Stmt(ABC):
    @abstractmethod
    def accept(self, visitor):
        pass

class Expr(ABC):
    @abstractmethod
    def accept(self, visitor):
        pass

@dataclass(frozen=True)
class BlockStmt(Stmt):
    statements: [Stmt]

    def __str__(self):
        out = 'begin block\n'
        for s in self.statements:
            out += f'\t{s}'
        out += 'end block'
        return out

    def accept(self, visitor):
        return visitor.visitBlock(self)

@dataclass(frozen=True)
class BreakStmt(Stmt):
    def __str__(self):
        return 'break\n'

    def accept(self, visitor):
        return visitor.visitBreak(self)

@dataclass(frozen=True)
class FunctionDecl(Stmt):
    name: Token
    params: [Token]
    body: [Stmt]

    def __str__(self):
        out = f'({self.name} '
        for p in self.params:
            out += f'{p.lexeme}\n'
        for s in self.body:
            out += f'{s}'
        return out + ')'

    def accept(self, visitor):
        return visitor.visitFunDecl(self)

@dataclass(frozen=True)
class IfStmt(Stmt):
    condition: Expr
    thenBranch: Stmt
    elseBranch: Stmt

    def __str__(self):
        if self.elseBranch is None:
            return f'(if {self.condition})\nthen {self.thenBranch}'
        else:
            return f'(if {self.condition})\nthen {self.thenBranch}else {self.elseBranch}'

    def accept(self, visitor):
        return visitor.visitIf(self)

@dataclass(frozen=True)
class PrintStmt(Stmt):
    value: Expr

    def __str__(self):
        return f'print {self.value}\n'

    def accept(self, visitor):
        return visitor.visitPrint(self)

@dataclass(frozen=True)
class ExpressionStmt(Stmt):
    expression: Expr

    def accept(self, visitor):
        return visitor.visitExpression(self)

    def __str__(self):
        return f'{self.expression}\n'

@dataclass(frozen=True)
class ReturnStmt(Stmt):
    keyword: Token
    value: Expr

    def __str__(self):
        return f'(return {self.value})\n'

    def accept(self, visitor):
        return visitor.visitReturn(self)

@dataclass(frozen=True)
class WhileStmt(Stmt):
    condition: Expr
    body: Stmt

    def __str__(self):
        return f'while {self.condition}\n  {self.body}'

    def accept(self, visitor):
        return visitor.visitWhile(self)

@dataclass(frozen=True)
class VarDecl(Stmt):
    name: Token
    initialization: Expr

    def __str__(self):
        if self.initialization is None:
            return f'(decl {self.name.lexeme})\n'
        else:
            return f'(decl {self.name.lexeme} {self.initialization})\n'

    def accept(self, visitor):
        return visitor.visitVarDecl(self)

@dataclass(frozen=True)
class AnonFun(Expr):
    params: [Token]
    body: [Stmt]

    def accept(self, visitor):
        return visitor.visitAnonFun(self)

    def __str__(self):
        out = f'( '
        for p in self.params:
            out += f'{p.lexeme} '
        for s in self.body:
            out += f'{s}'
        return out + ')'

@dataclass(frozen=True)
class Assign(Expr):
    name: Token
    value: Expr

    def __str__(self):
        return f'(assign {self.name.lexeme} {self.value})'

    def accept(self, visitor):
        return visitor.visitAssign(self)

@dataclass(frozen=True)
class Binary(Expr):
    left: Expr
    operator: Token
    right: Expr

    def __str__(self):
        return f'({self.left} {self.operator.lexeme} {self.right})'

    def accept(self, visitor):
        return visitor.visitBinary(self)

@dataclass(frozen=True)
class Call(Expr):
    callee: Expr
    paren: Token
    arguments: [Expr]

    def __str__(self):
        out = f'({self.callee}'
        for arg in self.arguments:
            out += f'{arg}'
        out += ')'
        return out

    def accept(self, visitor):
        return visitor.visitCall(self)

@dataclass(frozen=True)
class Get(Expr):
    obj: Expr
    name: Token

    def accept(self, visitor):
        return visitor.visitGet(self)

    def __str__(self):
        return f'(get {self.obj}.{self.name.lexeme})'

@dataclass(frozen=True)
class Grouping(Expr):
    expression: Expr

    def __str__(self):
        return f'({self.expression})'

    def accept(self, visitor):
        return visitor.visitGrouping(self)

@dataclass(frozen=True)
class Literal(Expr):
    value: Any
    
    def __str__(self):
        if self.value is None: return 'nil'
        return f'{self.value}'

    def accept(self, visitor):
        return visitor.visitLiteral(self)

@dataclass(frozen=True)
class Logical(Expr):
    left: Expr
    operator: Token
    right: Expr

    def __str__(self):
        return f'({self.left} {self.operator.lexeme} {self.right})'

    def accept(self, visitor):
        return visitor.visitLogical(self)

@dataclass(frozen=True)
class Set(Expr):
    obj: Expr
    name: Token
    value: Expr

    def __str__(self):
        return f'(set {self.obj}.{self.name.lexeme} {self.value})'

    def accept(self, visitor):
        return visitor.visitSet(self)

@dataclass(frozen=True)
class Super(Expr):
    keyword: Token
    method: Token

    def accept(self, visitor):
        return visitor.visitSuper(self)

    def __str__(self):
        return f'(super {self.method.lexeme})'

@dataclass(frozen=True)
class This:
    keyword: Token

    def __str__(self):
        return 'this'

    def accept(self, visitor):
        return visitor.visitThis(self)

@dataclass(frozen=True)
class Unary(Expr):
    operator: Token
    right: Expr

    def __str__(self):
        return f'({self.operator.lexeme} {self.right})'

    def accept(self, visitor):
        return visitor.visitUnary(self)

@dataclass(frozen=True)
class Ternary(Expr):
    operator: Token
    begin: Expr
    middle: Expr
    end: Expr

    def __str__(self):
        return f'({self.operator.lexeme} {self.begin} {self.middle} {self.end})'

    def accept(self, visitor):
        return visitor.visitTernary(self)

@dataclass(frozen=True)
class Variable(Expr):
    name: Token

    def __str__(self):
        return f'{self.name.lexeme}'

    def accept(self, visitor):
        return visitor.visitVariable(self)

@dataclass(frozen=True)
class ClassDecl(Stmt):
    name: Token
    superclass: Variable
    methods: [FunctionDecl]
    classMethods: [FunctionDecl]
    attrs: [FunctionDecl]

    def __str__(self):
        out = f'Class {self.name.lexeme} < {self.superclass} [\n'
        for m in self.methods:
            out += f'{m}'
        for m in self.classMethods:
            out += f'{m}'
        for m in self.attrs:
            out += f'{m}'
        out += ']'
        return out

    def accept(self, visitor):
        return visitor.visitClassDecl(self)

class LoxCallable(ABC):
    @abstractmethod
    def arity(self):
        pass

    @abstractmethod
    def call(self, interpreter, arguments):
        pass

class StmtVisitor(ABC):
    @abstractmethod
    def visitBlock(self, stmt):
        pass

    @abstractmethod
    def visitBreak(self, stmt):
        pass

    @abstractmethod
    def visitIf(self, stmt):
        pass

    @abstractmethod
    def visitPrint(self, stmt):
        pass

    @abstractmethod
    def visitExpression(self, stmt):
        pass

    @abstractmethod
    def visitReturn(self, stmt):
        pass

    @abstractmethod
    def visitWhile(self, stmt):
        pass

    @abstractmethod
    def visitClassDecl(self, stmt):
        pass

    @abstractmethod
    def visitFunDecl(self, stmt):
        pass

    @abstractmethod
    def visitVarDecl(self, stmt):
        pass

class ExprVisitor(ABC):
    @abstractmethod
    def visitAnonFun(self, expr):
        pass

    @abstractmethod
    def visitAssign(self, expr):
        pass

    @abstractmethod
    def visitBinary(self, expr):
        pass

    @abstractmethod
    def visitCall(self, expr):
        pass

    @abstractmethod
    def visitGet(self, expr):
        pass

    @abstractmethod
    def visitGrouping(self, expr):
        pass

    @abstractmethod
    def visitLogical(self, expr):
        pass

    @abstractmethod
    def visitLiteral(self, expr):
        pass

    @abstractmethod
    def visitSet(self, expr):
        pass

    @abstractmethod
    def visitThis(self, expr):
        pass

    @abstractmethod
    def visitUnary(self, expr):
        pass

    @abstractmethod
    def visitTernary(self, expr):
        pass

    @abstractmethod
    def visitVariable(self, expr):
        pass

class ParseError(Exception):
    def __init__(self, token, message):
        global hadRuntimeError
        self.token = token
        self.message = message
        hadRuntimeError = True
        super().__init__(self.message)

    def report(self):
        if self.token.ttype == TokenType.EOF:
            report(self.token.line, ' at end', self.message)
        else:
            report(self.token.line, f' at "{self.token.lexeme}"', self.message)


class Parser:
    def __init__(self, tokens, repl = False):
        self.tokens = tokens
        self.current = 0
        self.repl = repl

    def parse(self):
        statements = []
        while not self.isAtEnd():
            try:
                statements.append(self.declaration())
            except ParseError as err:
                err.report()
                self.synchronize()
        return statements

    def match(self, *args):
        matched = any(self.check(ttype) for ttype in args)
        if matched: self.advance()
        return matched

    def isAtEnd(self):
        return self.peek().ttype == TokenType.EOF

    def check(self, ttype):
        return not self.isAtEnd() and self.peek().ttype == ttype

    def advance(self):
        if not self.isAtEnd():
            self.current += 1
        return self.previous()

    def peek(self):
        return self.tokens[self.current]

    def previous(self):
        return self.tokens[self.current - 1]

    def next(self):
        return self.tokens[self.current + 1]

    def declaration(self):
        if self.match(TokenType.CLASS): return self.classDeclaration()
        elif self.match(TokenType.FUN):
            if self.check(TokenType.IDENTIFIER):
                return self.funDeclaration('function')
            else:
                self.current -= 1
        elif self.match(TokenType.VAR): return self.varDeclaration()
        return self.statement()

    def classDeclaration(self):
        name = self.consume(TokenType.IDENTIFIER, "Expect class name.")
        superclass = Variable(self.consume(TokenType.IDENTIFIER, "Expect superclass name.")) \
                if self.match(TokenType.LESS) else None
        self.consume(TokenType.LEFT_BRACE, "Expect '{' before class body.")
        methods = []
        classMethods = []
        attrs = []
        while not self.check(TokenType.RIGHT_BRACE) and not self.isAtEnd():
            if self.match(TokenType.CLASS):
                classMethods.append(self.funDeclaration('method'))
            elif self.next().ttype == TokenType.LEFT_PAREN:
                methods.append(self.funDeclaration('method'))
            else:
                attrs.append(self.funDeclaration('getAttr'))
        self.consume(TokenType.RIGHT_BRACE, "Expect '}' after class body")
        return ClassDecl(name, superclass, methods, classMethods, attrs)

    def funDeclaration(self, kind):
        name = self.consume(TokenType.IDENTIFIER, 'Expect ' + kind + 'name.')
        if not kind == 'getAttr':
            self.consume(TokenType.LEFT_PAREN, "Expect '(' after " + kind + " name.")
        params = []
        if not self.check(TokenType.RIGHT_PAREN) and not kind == 'getAttr':
            params.append(self.consume(TokenType.IDENTIFIER, 'Expect parameter name.'))
            while self.match(TokenType.COMMA):
                params.append(self.consume(TokenType.IDENTIFIER, 'Expect parameter name.'))
        if not kind == 'getAttr':
            self.consume(TokenType.RIGHT_PAREN, "Expect ')' after " + kind + " name.")
        self.consume(TokenType.LEFT_BRACE, "Expect '{' before " + kind + " body.")
        body = self.block()
        return FunctionDecl(name, params, body.statements)

    def varDeclaration(self):
        name = self.consume(TokenType.IDENTIFIER, 'Expected variable name.')
        initialization = self.expression() if self.match(TokenType.EQUAL) else None
        if not self.repl or not self.isAtEnd():
            self.consume(TokenType.SEMICOLON, "Expected ';' at end of declaration.")
        return VarDecl(name, initialization)

    def statement(self):
        if self.match(TokenType.IF):
            return self.if_stmt()
        elif self.match(TokenType.RETURN):
            return self.return_stmt()
        elif self.match(TokenType.PRINT):
            return self.print_stmt()
        elif self.match(TokenType.WHILE):
            return self.while_stmt()
        elif self.match(TokenType.FOR):
            return self.for_stmt()
        elif self.match(TokenType.BREAK):
            return self.break_stmt()
        elif self.match(TokenType.LEFT_BRACE):
            return self.block()
        return self.expression_stmt()

    def block(self):
        statements = []
        while not self.check(TokenType.RIGHT_BRACE) and not self.isAtEnd():
            statements.append(self.declaration())
        self.consume(TokenType.RIGHT_BRACE, "Expect '}' at end of block.")
        return BlockStmt(statements)

    def break_stmt(self):
        self.consume(TokenType.SEMICOLON, "Expected ';' at end of statement.")
        return BreakStmt()

    def if_stmt(self):
        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.")
        expr = self.expression()
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.")
        thenBranch = self.statement()
        elseBranch = None
        if self.match(TokenType.ELSE):
            elseBranch = self.statement()
        return IfStmt(expr, thenBranch, elseBranch)

    def print_stmt(self):
        value = self.expression()
        if not self.repl or not self.isAtEnd():
            self.consume(TokenType.SEMICOLON, "Expected ';' at end of statement.")
        return PrintStmt(value)

    def expression_stmt(self):
        expr = self.expression()
        if not self.repl or not self.isAtEnd():
            self.consume(TokenType.SEMICOLON, "Expected ';' at end of statement.")
        return ExpressionStmt(expr)

    def return_stmt(self):
        keyword = self.previous()
        value = self.expression() if not self.check(TokenType.SEMICOLON) else None
        self.consume(TokenType.SEMICOLON, "Expected ';' after return statement.")
        return ReturnStmt(keyword, value)

    def while_stmt(self):
        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.")
        condition = self.expression()
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after condition in while.")
        body = self.statement()
        return WhileStmt(condition, body)

    def for_stmt(self):
        self.consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.")
        initializer = None
        if self.match(TokenType.SEMICOLON):
            initializer = None
        elif self.match(TokenType.VAR):
            initializer = self.varDeclaration()
        else:
            initializer = self.expression_stmt()
        condition = self.expression() if not self.check(TokenType.SEMICOLON) else Literal(True)
        self.consume(TokenType.SEMICOLON, "Expec ';' after for loop condition.")
        increment = self.expression() if not self.check(TokenType.RIGHT_PAREN) else None
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.")
        body = self.statement()
        if not increment is None:
            body = BlockStmt([body, ExpressionStmt(increment)])
        body = WhileStmt(condition, body)
        if not initializer is None:
            body = BlockStmt([initializer, body])
        return body

    def expression(self):
        return self.comma_expr()

    def comma_expr(self):
        expr = self.assignment()
        while self.match(TokenType.COMMA):
            operator = self.previous()
            right = self.assignment()
            expr = Binary(expr, operator, right)
        return expr

    def assignment(self):
        expr = self.ternary()
        if self.match(TokenType.EQUAL):
            equals = self.previous()
            value = self.assignment()
            if type(expr) is Variable:
                name = expr.name
                return Assign(name, value)
            elif isinstance(expr, Get):
                return Set(expr.obj, expr.name, value)
            else:
                raise ParseError(equals, 'Invalid lvalue.')
        return expr

    def ternary(self):
        expr = self.orExpr()
        if self.match(TokenType.QMARK):
            operator = self.previous()
            middle = self.expression()
            self.consume(TokenType.COLON, "Expected ':' for ternary expression.")
            end = self.expression()
            expr = Ternary(operator, expr, middle, end)
        return expr

    def orExpr(self):
        expr = self.andExpr()
        while self.match(TokenType.OR):
            operator = self.previous()
            right = self.andExpr()
            expr = Logical(expr, operator, right)
        return expr

    def andExpr(self):
        expr = self.equality()
        while self.match(TokenType.AND):
            operator = self.previous()
            right = self.equality()
            expr = Logical(expr, operator, right)
        return expr

    def equality(self):
        expr = self.comparison()
        while self.match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL):
            operator = self.previous()
            right = self.comparison()
            expr = Binary(expr, operator, right)
        return expr

    def comparison(self):
        expr = self.term()
        while self.match(TokenType.GREATER, TokenType.GREATER_EQUAL, \
                TokenType.LESS, TokenType.LESS_EQUAL):
            operator = self.previous()
            right = self.term()
            expr = Binary(expr, operator, right)
        return expr

    def term(self):
        expr = self.factor()
        while self.match(TokenType.PLUS, TokenType.MINUS):
            operator = self.previous()
            right = self.factor()
            expr = Binary(expr, operator, right)
        return expr

    def factor(self):
        expr = self.unary()
        while self.match(TokenType.STAR, TokenType.SLASH):
            operator = self.previous()
            right = self.unary()
            expr = Binary(expr, operator, right)
        return expr

    def unary(self):
        if self.match(TokenType.MINUS, TokenType.BANG):
            operator = self.previous()
            expr = self.unary()
            return Unary(operator, expr)
        return self.call()

    def call(self):
        expr = self.primary()
        while True:
            if self.match(TokenType.LEFT_PAREN):
                arguments = []
                if not self.check(TokenType.RIGHT_PAREN):
                    arguments.append(self.assignment()) # Use assignment instead of expression to
                    while self.match(TokenType.COMMA):  # diasambiguate comma parsing. Comma operator
                        arguments.append(self.assignment()) # in arg list must be parenthesized.
                paren = self.consume(TokenType.RIGHT_PAREN, "Expected ')' after arguments.")
                expr = Call(expr, paren, arguments)
            elif self.match(TokenType.DOT):
                name = self.consume(TokenType.IDENTIFIER, "Expected property name after '.'.")
                expr = Get(expr, name)
            else:
                break
        return expr

    def primary(self):
        if self.match(TokenType.FALSE): return Literal(False)
        elif self.match(TokenType.TRUE): return Literal(True)
        elif self.match(TokenType.NIL): return Literal(None)
        elif self.match(TokenType.NUMBER, TokenType.STRING):
            return Literal(self.previous().literal)
        elif self.match(TokenType.IDENTIFIER):
            return Variable(self.previous())
        elif self.match(TokenType.FUN):
            return self.anon_fun()
        elif self.match(TokenType.THIS):
            return This(self.previous())
        elif self.match(TokenType.SUPER):
            keyword = self.previous()
            self.consume(TokenType.DOT, "Expect '.' after 'super'.")
            method = self.consume(TokenType.IDENTIFIER, "Expect superclass method name.")
            return Super(keyword, method)
        elif self.match(TokenType.LEFT_PAREN):
            expr = self.expression()
            self.consume(TokenType.RIGHT_PAREN, "Expected ')' after expression.")
            return Grouping(expr)
        elif self.match(TokenType.EQUAL_EQUAL, TokenType.BANG_EQUAL):
            op = self.previous()
            right = self.comparison()
            raise ParseError(op, "Missing left side of binary operator.")
        elif self.match(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL):
            op = self.previous()
            right = self.term()
            raise ParseError(op, "Missing left side of binary operator.")
        elif self.match(TokenType.PLUS):
            op = self.previous()
            right = self.factor()
            raise ParseError(op, "Missing left side of binary operator.")
        elif self.match(TokenType.STAR, TokenType.SLASH):
            op = self.previous()
            right = self.unary()
            raise ParseError(op, "Missing left side of binary operator.")
        else:
            raise ParseError(self.peek(), "Expected expression.")

    def anon_fun(self):
        self.consume(TokenType.LEFT_PAREN, "Expect '(' before params in anonymous function.")
        params = []
        if not self.check(TokenType.RIGHT_PAREN):
            params.append(self.consume(TokenType.IDENTIFIER, 'Expect parameter name.'))
            while self.match(TokenType.COMMA):
                params.append(self.consume(TokenType.IDENTIFIER, 'Expect parameter name.'))
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after params.")
        self.consume(TokenType.LEFT_BRACE, "Expect '{' before body.")
        body = self.block()
        return AnonFun(params, body.statements)

    def consume(self, ttype, msg):
        if self.check(ttype):
            return self.advance()
        else:
            raise ParseError(self.peek(), msg)

    def synchronize(self):
        self.advance()
        ttype = self.peek().ttype
        match_types = [TokenType.CLASS, TokenType.FUN, TokenType.VAR, \
                TokenType.FOR, TokenType.IF, TokenType.WHILE, TokenType.PRINT, \
                TokenType.RETURN]
        while not ttype == TokenType.EOF:
            if ttype in match_types:
                return
            self.advance()
            ttype = self.peek().ttype

class RuntimeError(Exception):
    def __init__(self, token, message):
        self.token = token
        self.message = message
        super().__init__(self.message)

    def report(self):
        reportRuntime(self.token.line, self.message)

class Environment:
    def __init__(self, enclosing = None):
        self.values = {}
        self.enclosing = enclosing
    
    def define(self, name, value, initialized = False):
        self.values[name] = (value, initialized)

    def get(self, name):
        if name.lexeme in self.values:
            if not self.values[name.lexeme][1]:
                raise RuntimeError(name, f"Using uninitialized variable '{name.lexeme}'")
            return self.values[name.lexeme][0]
        if not self.enclosing is None:
            return self.enclosing.get(name)
        raise RuntimeError(name, f"Undefined varaible '{name.lexeme}'.")
    
    def getAt(self, dist, name):
        return self.ancestor(dist).values[name][0]

    def ancestor(self, dist):
        environment = self
        for i in range(dist):
            environment = environment.enclosing
        return environment

    def assign(self, name, value):
        if name.lexeme in self.values:
            self.values[name.lexeme] = (value, True)
        elif not self.enclosing is None:
            self.enclosing.assign(name, value)
        else:
            raise RuntimeError(name, f"Undefined varaible '{name.lexeme}'.")

    def assignAt(self, dist, name, value):
        self.ancestor(dist).assign(name, value)

class Return(Exception):
    def __init__(self, value):
        self.value = value
        super().__init__(None)


class LoxFunction(LoxCallable):
    def __init__(self, declaration, closure, isInitializer = False):
        self.declaration = declaration
        self.closure = closure
        self.isInitializer = isInitializer

    def call(self, interpreter, arguments):
        environment = Environment(self.closure)
        for param, arg in zip(self.declaration.params, arguments):
            environment.define(param.lexeme, arg, True)
        try:
            interpreter.executeBlock(self.declaration.body, environment)
        except Return as ret:
            if self.isInitializer: return self.closure.getAt(0, "this")
            return ret.value
        if self.isInitializer: return self.closure.getAt(0, 'this')

    def bind(self, instance):
        environment = Environment(self.closure)
        environment.define('this', instance)
        return LoxFunction(self.declaration, environment, self.isInitializer)

    def arity(self):
        return len(self.declaration.params)
    
    def __str__(self):
        return "<fn " + self.declaration.name.lexeme + ">"

class LoxInstance:
    def __init__(self, klass):
        self.klass = klass
        self.fields = {}

    def isAttr(self, name):
        return name.lexeme in self.klass.attrs

    def get(self, name):
        if name.lexeme in self.fields:
            return self.fields[name.lexeme]
        method = self.klass.findMethod(name.lexeme)
        if method is None:
            raise RuntimeError(name, f"Undefined property '{name.lexeme}'.")
        return method.bind(self)

    def set(self, name, value):
        self.fields[name.lexeme] = value
    
    def __str__(self):
        return self.klass.name + " instance"

class LoxClass(LoxCallable, LoxInstance):
    def __init__(self, name, superclass, methods, classMethods, attrs):
        self.name = name
        self.superclass = superclass
        self.methods = methods
        self.attrs = attrs
        LoxInstance.__init__(self, self.MetaClass(classMethods))

    class MetaClass:
        def __init__(self, methods):
            self.name = 'class'
            self.methods = methods
            self.attrs = {}

        def findMethod(self, name):
            if name in self.methods:
                return self.methods[name]

    def call(self, interpreter, arguments):
        instance = LoxInstance(self)
        initializer = self.findMethod('init')
        if not initializer is None:
            initializer.bind(instance).call(interpreter, arguments)
        return instance

    def arity(self):
        initializer = self.findMethod('init')
        if initializer is None: return 0
        return initializer.arity()

    def findMethod(self, name):
        if name in self.methods:
            return self.methods[name]
        if name in self.attrs:
            return self.attrs[name]
        if not self.superclass is None:
            return self.superclass.findMethod(name)

    def __str__(self):
        return self.name

class Interpreter(ExprVisitor, StmtVisitor):
    def __init__(self, repl = False):
        self.globals = Environment()
        self.environment = self.globals
        self.locals = {}
        self.repl = repl
        self.to_break = False

        self.globals.define('clock', self.Clock(), True)

    class Clock(LoxCallable):
        def arity(self):
            return 0

        def call(self, interpreter, arguments):
            return time_ns() / 1000000000

        def __str__(self):
            return '<native fn>'


    def interpret(self, stmts):
        end = ''
        try:
            for stmt in stmts:
                end = self.evaluate(stmt)
            if self.repl and end: print(end)
        except RuntimeError as err:
            err.report()

    def resolve(self, expr, depth):
        self.locals[expr] = depth

    def lookupVariable(self, name, expr):
        return self.environment.getAt(self.locals[expr], name.lexeme) \
                if expr in self.locals else self.globals.get(name)

    def evaluate(self, stmt):
        return stmt.accept(self)

    def checkNumberOperand(self, operator, value):
        if not type(value) is float:
            raise RuntimeError(operator, "Expected number operand.")

    def visitBlock(self, stmt):
        self.executeBlock(stmt.statements, Environment(self.environment))

    def executeBlock(self, stmts, environment):
        previous = self.environment
        try:
            self.environment = environment
            for s in stmts:
                self.evaluate(s)
        finally:
            self.environment = previous

    def visitBreak(self, stmt):
        self.to_break = True

    def visitIf(self, stmt):
        if self.evaluate(stmt.condition):
            self.evaluate(stmt.thenBranch)
        elif not stmt.elseBranch is None:
            self.evaluate(stmt.elseBranch)

    def visitPrint(self, stmt):
        print(self.evaluate(stmt.value))

    def visitExpression(self, stmt):
        return self.evaluate(stmt.expression)

    def visitReturn(self, stmt):
        raise Return(self.evaluate(stmt.value) if not stmt.value is None else None)

    def visitWhile(self, stmt):
        while self.evaluate(stmt.condition):
            self.evaluate(stmt.body)
            if self.to_break: break
        self.to_break = False

    def visitClassDecl(self, stmt):
        superclass = None
        if not stmt.superclass is None:
            superclass = self.evaluate(stmt.superclass)
            if not isinstance(superclass, LoxClass):
                raise RuntimeError(stmt.superclass.name, "Superclass must be a class.")
        self.environment.define(stmt.name.lexeme, None, True)
        if not stmt.superclass is None:
            self.environment = Environment(self.environment)
            self.environment.define("super", superclass, True)
        methods = {method.name.lexeme: LoxFunction(method, self.environment, method.name.lexeme == 'init')
                for method in stmt.methods}
        classMethods = {method.name.lexeme: LoxFunction(method, self.environment, method.name.lexeme == 'init')
                for method in stmt.classMethods}
        attrs = {method.name.lexeme: LoxFunction(method, self.environment, method.name.lexeme == 'init')
                for method in stmt.attrs}
        klass = LoxClass(stmt.name.lexeme, superclass, methods, classMethods, attrs)
        if not stmt.superclass is None:
            self.environment = self.environment.enclosing
        self.environment.assign(stmt.name, klass)

    def visitFunDecl(self, stmt):
        self.environment.define(stmt.name.lexeme, LoxFunction(stmt, self.environment), True)

    def visitVarDecl(self, stmt):
        value = self.evaluate(stmt.initialization) if not stmt.initialization is None else None
        inited = not stmt.initialization is None
        self.environment.define(stmt.name.lexeme, value, inited)
        return value

    def visitAnonFun(self, stmt):
        return LoxFunction(stmt, self.environment)

    def visitAssign(self, expr):
        value = self.evaluate(expr.value)
        if expr in self.locals:
            self.environment.assignAt(self.locals[expr], expr.name, value)
        else:
            self.globals.assign(expr.name, value)
        return value

    def visitBinary(self, expr):
        left = self.evaluate(expr.left)
        right = self.evaluate(expr.right)
        match expr.operator.ttype:
            case TokenType.MINUS:
                self.checkNumberOperand(expr.operator, left)
                self.checkNumberOperand(expr.operator, right)
                return left - right
            case TokenType.PLUS:
                if not ((type(left) is float or type(left) is str) and \
                        (type(right) is float or type(right) is str)):
                    raise RuntimeError(expr.operator, f'Expected number or string operand, but got {type(left)} and {type(right)}.')
                return left + right
            case TokenType.STAR:
                self.checkNumberOperand(expr.operator, left)
                self.checkNumberOperand(expr.operator, right)
                return left * right
            case TokenType.SLASH:
                self.checkNumberOperand(expr.operator, left)
                self.checkNumberOperand(expr.operator, right)
                if right == 0:
                    raise RuntimeError(expr.operator, "Division by zero.")
                return left / right
            case TokenType.GREATER:
                self.checkNumberOperand(expr.operator, left)
                self.checkNumberOperand(expr.operator, right)
                return left > right
            case TokenType.GREATER_EQUAL:
                self.checkNumberOperand(expr.operator, left)
                self.checkNumberOperand(expr.operator, right)
                return left >= right
            case TokenType.LESS:
                self.checkNumberOperand(expr.operator, left)
                self.checkNumberOperand(expr.operator, right)
                return left < right
            case TokenType.LESS_EQUAL:
                self.checkNumberOperand(expr.operator, left)
                self.checkNumberOperand(expr.operator, right)
                return left <= right
            case TokenType.EQUAL_EQUAL:
                return left == right
            case TokenType.BANG_EQUAL:
                return left != right

    def visitCall(self, expr):
        callee = self.evaluate(expr.callee)
        arguments = [self.evaluate(arg) for arg in expr.arguments]
        if not isinstance(callee, LoxCallable):
            raise RuntimeError(expr.paren, 'Can only call functions and classes.')
        if not len(arguments) == callee.arity():
            raise RuntimeError(expr.paren, f'Expexted {callee.arity()} arguments, but got {len(arguments)}')
        return callee.call(self, arguments)

    def visitGet(self, expr):
        obj = self.evaluate(expr.obj)
        if isinstance(obj, LoxInstance):
            if obj.isAttr(expr.name):
                return obj.get(expr.name).call(self, [])
            return obj.get(expr.name)
        raise RuntimeError(expr.name, "Only instances have properties.")

    def visitGrouping(self, expr):
        return self.evaluate(expr.expression)

    def visitLogical(self, expr):
        match expr.operator.ttype:
            case TokenType.OR:
                return True if self.evaluate(expr.left) else self.evaluate(expr.right)
            case TokenType.AND:
                return False if not self.evaluate(expr.left) else self.evaluate(expr.right)

    def visitLiteral(self, expr):
        return expr.value

    def visitSet(self, expr):
        obj = self.evaluate(expr.obj)
        if not isinstance(obj, LoxInstance):
            raise RuntimeError(expr.name, "Only instances have fields")
        value = self.evaluate(expr.value)
        obj.set(expr.name, value)
        return value

    def visitSuper(self, expr):
        dist = self.locals[expr]
        superclass = self.environment.getAt(dist, 'super')
        obj = self.environment.getAt(dist - 1, 'this')
        method = superclass.findMethod(expr.method.lexeme)
        if method is None:
            raise RuntimeError(expr.method, f"Undefined property '{expr.method.lexeme}'.")
        return method.bind(obj)

    def visitThis(self, expr):
        return self.lookupVariable(expr.keyword, expr)

    def visitUnary(self, expr):
        right = self.evaluate(expr.right)
        if expr.operator.ttype == TokenType.MINUS:
            self.checkNumberOperand(expr.operator, right)
            return -1*right
        elif expr.operator.ttype == TokenType.BANG:
            return not right

    def visitTernary(self, expr):
        if self.evaluate(expr.begin):
            return self.evaluate(expr.middle)
        else:
            return self.evaluate(expr.end)

    def visitVariable(self, expr):
        return self.lookupVariable(expr.name, expr)

class Resolver(ExprVisitor, StmtVisitor):
    def __init__(self, interpreter):
        self.interpreter = interpreter
        self.scopes = []
        self.used = []
        self.currentFunction = self.FunctionType.NONE
        self.currentClass = self.ClassType.NONE

    class FunctionType(Enum):
        NONE = auto()
        FUNCTION = auto()
        INITIALIZER = auto()
        METHOD= auto()

    class ClassType(Enum):
        NONE = auto()
        CLASS = auto()
        SUBCLASS = auto()

    def resolve_stmts(self, stmts):
        for stmt in stmts:
            self.resolve(stmt)

    def resolve(self, stmt):
        stmt.accept(self)

    def resolveLocal(self, expr, name):
        for i, scope in enumerate(reversed(self.scopes)):
            if name.lexeme in scope:
                self.interpreter.resolve(expr, i)
                if name.lexeme != 'this':
                    self.used[-1*(i+1)][name.lexeme] = (self.used[-1*(i+1)][name.lexeme][0], True)
                break

    def resolveFunction(self, fun, funcType):
        enclosingFunction = self.currentFunction
        self.currentFunction = funcType
        self.beginScope()
        for param in fun.params:
            self.declare(param)
            self.define(param)
        self.resolve_stmts(fun.body)
        self.endScope()
        self.currentFunction = enclosingFunction

    def beginScope(self):
        self.scopes.append({})
        self.used.append({})

    def endScope(self):
        self.scopes.pop()
        used = self.used.pop()
        for key, value in used.items():
            if not value[1]:
                error(value[0], 'Unused variable.')

    def declare(self, name):
        top = len(self.scopes)
        if not top == 0:
            if name.lexeme in self.scopes[top - 1]:
                error(name, 'Already a variable with this name in this scope.')
            self.scopes[top - 1][name.lexeme] = False
            self.used[top - 1][name.lexeme] = (name, False)

    def define(self, name):
        top = len(self.scopes)
        if not top == 0:
            self.scopes[top - 1][name.lexeme] = True

    def visitBlock(self, stmt):
        self.beginScope()
        self.resolve_stmts(stmt.statements)
        self.endScope()

    def visitBreak(self, stmt):
        pass

    def visitIf(self, stmt):
        self.resolve(stmt.condition)
        self.resolve(stmt.thenBranch)
        if not stmt.elseBranch is None:
            self.resolve(stmt.elseBranch)

    def visitPrint(self, stmt):
        self.resolve(stmt.value)

    def visitExpression(self, stmt):
        self.resolve(stmt.expression)

    def visitReturn(self, stmt):
        if self.currentFunction == self.FunctionType.NONE:
            error(stmt.keyword, "Can't return from top-level code.")
        if not stmt.value is None:
            if self.currentFunction == self.FunctionType.INITIALIZER:
                error(stmt.keyword, "Can't return value from an initializer.")
            self.resolve(stmt.value)

    def visitWhile(self, stmt):
        self.resolve(stmt.condition)
        self.resolve(stmt.body)

    def visitClassDecl(self, stmt):
        enclosingClass = self.currentClass
        self.currentClass = self.ClassType.CLASS
        self.declare(stmt.name)
        self.define(stmt.name)
        if not stmt.superclass is None:
            self.currentClass = self.ClassType.SUBCLASS
            if stmt.name.lexeme == stmt.superclass.name.lexeme:
                error(stmt.superclass.name, "A class can't inherit from itself.")
            else:
                self.resolve(stmt.superclass)
        if not stmt.superclass is None:
            self.beginScope();
            self.scopes[-1]["super"] = True
        self.beginScope()
        self.scopes[-1]['this'] = True
        for method in stmt.methods:
            if method.name.lexeme == 'init':
                self.resolveFunction(method, self.FunctionType.INITIALIZER)
            else:
                self.resolveFunction(method, self.FunctionType.METHOD)
        for method in stmt.classMethods:
            self.resolveFunction(method, self.FunctionType.METHOD)
        for method in stmt.attrs:
            self.resolveFunction(method, self.FunctionType.METHOD)
        self.endScope()
        if not stmt.superclass is None: self.endScope()
        self.currentClass = enclosingClass

    def visitFunDecl(self, stmt):
        self.declare(stmt.name)
        self.define(stmt.name)
        self.resolveFunction(stmt, self.FunctionType.FUNCTION)

    def visitVarDecl(self, stmt):
        self.declare(stmt.name)
        if not stmt.initialization is None:
            self.resolve(stmt.initialization)
        self.define(stmt.name)
        
    def visitAnonFun(self, expr):
        self.resolveFunction(expr, self.FunctionType.FUNCTION)

    def visitAssign(self, expr):
        self.resolve(expr.value)
        self.resolveLocal(expr, expr.name)

    def visitBinary(self, expr):
        self.resolve(expr.left)
        self.resolve(expr.right)

    def visitCall(self, expr):
        self.resolve(expr.callee)
        for arg in expr.arguments:
            self.resolve(arg)

    def visitGet(self, expr):
        self.resolve(expr.obj)

    def visitGrouping(self, expr):
        self.resolve(expr.expression)

    def visitLogical(self, expr):
        self.resolve(expr.left)
        self.resolve(expr.right)

    def visitLiteral(self, expr):
        pass

    def visitSet(self, expr):
        self.resolve(expr.obj)
        self.resolve(expr.value)

    def visitSuper(self, expr):
        if self.currentClass == self.ClassType.NONE:
            error(expr.keyword, "Can't use 'super' outside of a class.")
        elif not self.currentClass == self.ClassType.SUBCLASS:
            error(expr.keyword, "Can't use 'super' in a class with no superclass.")
        self.resolveLocal(expr, expr.keyword)

    def visitThis(self, expr):
        if self.currentClass == self.ClassType.NONE:
            error(expr.keyword, "Can't use 'this' keyword outside a class.")
        else:
            self.resolveLocal(expr, expr.keyword)

    def visitUnary(self, expr):
        self.resolve(expr.right)

    def visitTernary(self, expr):
        self.resolve(expr.begin)
        self.resolve(expr.middle)
        self.resolve(expr.end)

    def visitVariable(self, expr):
        if not len(self.scopes) == 0 and \
                expr.name.lexeme in self.scopes[len(self.scopes) - 1] and \
                self.scopes[len(self.scopes) - 1][expr.name.lexeme] == False:
            error(expr.name, "Can't read local variable in its own initializer.")
        self.resolveLocal(expr, expr.name)

def report(line, where, message):
    global hadError
    print(f'[line {line}] Error{where}: {message}')
    hadError = True

def reportRuntime(line, message):
    global hadRuntimeError
    print(f'[line {line}] Runtime Error: {message}')
    hadRuntimeError = True

def error(line, message):
    report(line, '', message)

def run(source, interpreter, repl = False):
    global hadError
    global hadRuntimeError
    scanner = Scanner(source)
    tokens = scanner.scanTokens()
    for token in tokens:
        print(token)

    parser = Parser(tokens, repl)
    stmts = parser.parse()
    if not hadError:
        for stmt in stmts:
            print(stmt, end='')
        resolver = Resolver(interpreter)
        resolver.resolve_stmts(stmts)
        if not hadError:
            interpreter.interpret(stmts)

def runFile(name):
    global hadError
    global hadRuntimeError
    with open(name) as f:
        contents = f.read()
    interpreter = Interpreter()
    run(contents, interpreter)
    if hadError: sys.exit(65)
    if hadRuntimeError: sys.exit(70)

def runPrompt():
    global hadError
    global hadRuntimeError
    interpreter = Interpreter(True)
    while True:
        try:
            line = input('> ')
        except EOFError:
            break
        if not line:
            break
        run(line, interpreter, True)
        hadError = False
        hadRuntimeError = False

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("name", help="Name of file to interpret",
                    type=str, nargs='?', default='')
    args = parser.parse_args()
    if(args.name):
        runFile(args.name)
    else:
        runPrompt()

if __name__ == '__main__':
    main()
