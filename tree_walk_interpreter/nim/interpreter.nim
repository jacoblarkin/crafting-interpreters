import error
import parser
import scanner

import std/enumerate
import sequtils
import sugar
import tables
import times

type
  FunctionType = enum
    NONE,
    FUNCTION,
    INITIALIZER,
    METHOD

  ClassType = enum
    NONEClass,
    CLASS,
    SUBCLASS

  ValueType* = enum
    LoxCallable,
    LoxNumber,
    LoxString,
    LoxBool,
    LoxClass,
    LoxInstance,
    LoxNil

  LoxCallableImpl = ref object
    call: (seq[LoxValue]) -> LoxValue
    arity: int
    closure: Environment
    declaration: Function
    isInitializer: bool
    name: string

  LoxInstanceImpl = ref object of RootObj
    klass: LoxClassImpl
    fields: Table[string, LoxValue]

  # Inherit instance b/c instance of MetaClass
  LoxClassImpl = ref object of LoxInstanceImpl
    name: string
    superclass: LoxClassImpl
    call: (seq[LoxValue]) -> LoxValue
    arity: int
    methods: Table[string, LoxCallableImpl]
    attrs: Table[string, LoxCallableImpl]

  LoxValue* = object
    case valType: ValueType
      of LoxCallable: callable: LoxCallableImpl
      of LoxNumber: lnumber: float
      of LoxString: lstring: string
      of LoxBool: lbool: bool
      of LoxClass: lclass: LoxClassImpl
      of LoxInstance: linstance: LoxInstanceImpl
      of LoxNil: discard

  RuntimeError* = object of CatchableError
  Return = object of CatchableError
    value: LoxValue
  ReturnRef = ref Return

  Environment = ref object
    values: Table[string, LoxValue]
    enclosing: Environment

let
  falseValue = LoxValue(valType: LoxBool, lbool: false)
  trueValue = LoxValue(valType: LoxBool, lbool: true)
  nilValue = LoxValue(valType: LoxNil)
  clockFn = LoxCallableImpl(call: ((x: seq[LoxValue])=>LoxValue(
      valType: LoxNumber, lnumber: cpuTime())), arity: 0, closure: nil,
          isInitializer: false, name: "clock")

var
  scopes: seq[Table[string, bool]] = @[]
  locals = initTable[Expr, int]()
  globals: Environment = Environment(values: initTable[string, LoxValue](),
      enclosing: nil)
  currentEnvironment: Environment = globals
  currentFunction: FunctionType = FunctionType.NONE
  currentClass = ClassType.NONEClass

proc `$`*(value: LoxValue): string =
  case value.valType:
    of LoxCallable:
      return "<fn " & value.callable.name & ">"
    of LoxNumber:
      return $value.lnumber
    of LoxString:
      return value.lstring
    of LoxBool:
      return $value.lbool
    of LoxClass:
      return "<class " & value.lclass.name & ">"
    of LoxInstance:
      return "<instance of class " & value.linstance.klass.name & ">"
    of LoxNil:
      return "nil"

proc newEnvironment(enclosing: Environment): Environment =
  return Environment(values: initTable[string, LoxValue](),
      enclosing: enclosing)

proc assign(environment: Environment, name: Token, value: LoxValue) =
  if name.lexeme in environment.values:
    environment.values[name.lexeme] = value
  elif environment.enclosing != nil:
    environment.enclosing.assign(name, value)
  else:
    raise newException(RuntimeError, "Undefined variable '" & name.lexeme & "'.")

proc define(environment: Environment, name: string, value: LoxValue) =
  environment.values[name] = value

proc get(environment: Environment, name: Token): LoxValue =
  if name.lexeme in environment.values:
    return environment.values[name.lexeme]
  if environment.enclosing != nil:
    return environment.enclosing.get(name)
  raise newException(RuntimeError, "Undefined variable '" & name.lexeme & "'.")

proc ancestor(environment: Environment, distance: int): Environment =
  var tmpEnv = environment
  var i = distance
  while i > 0:
    tmpEnv = tmpEnv.enclosing
    dec i
  return tmpEnv

proc getAt(environment: Environment, distance: int, name: string): LoxValue =
  return environment.ancestor(distance).values[name]

proc assignAt(environment: Environment, distance: int, name: Token,
    value: LoxValue) =
  environment.ancestor(distance).values[name.lexeme] = value

proc lookupVariable(name: Token, expression: Expr): LoxValue =
  if expression in locals:
    return currentEnvironment.getAt(locals[expression], name.lexeme)
  return globals.get(name)

proc isTruthy(value: LoxValue): bool =
  case value.valType:
    of LoxNumber:
      return value.lnumber != 0
    of LoxString:
      return value.lstring != ""
    of LoxBool:
      return value.lbool
    of LoxNil:
      return false
    of LoxCallable:
      return true
    of LoxClass:
      return true
    of LoxInstance:
      return true

template LoxValueFunctorImpl(checkedType, returnedType, valName, returnedName) =
  if value.valType == checkedType:
    return LoxValue(valType: returnedType, returnedName: fn(value.valName))

template LoxValueApplicativePartial(checkedType, returnedType, valName,
    returnedName) =
  if value.valType == checkedType:
    let actualFn = (x: seq[LoxValue]) => LoxValue(valType: returnedType,
      returnedName: fn(value.valName, x[0].valName))
    let callable = LoxCallableImpl(call: actualFn, arity: 1)
    return LoxValue(valType: LoxCallable, callable: callable)

func `<$>`[T: float | string | bool, R: float | string | bool](fn: (T) -> R,
    value: LoxValue): LoxValue =
  when T is float and R is float:
    LoxValueFunctorImpl(LoxNumber, LoxNumber, lnumber, lnumber)
  elif T is float and R is bool:
    LoxValueFunctorImpl(LoxNumber, LoxBool, lnumber, lbool)
  elif T is string and R is string:
    LoxValueFunctorImpl(LoxString, LoxString, lstring, lstring)
  elif T is bool and R is bool:
    LoxValueFunctorImpl(LoxBool, LoxBool, lbool, lbool)
  raise newException(RuntimeError, "(single <$>) Expected value of type " & $T &
      ", but got " & $value.valType)

func `<$>`[T: float | string | bool, R: float | string | bool](fn: (T, T) -> R,
    value: LoxValue): LoxValue =
  when T is float and R is float:
    LoxValueApplicativePartial(LoxNumber, LoxNumber, lnumber, lnumber)
  elif T is float and R is bool:
    LoxValueApplicativePartial(LoxNumber, LoxBool, lnumber, lbool)
  elif T is string and R is string:
    LoxValueApplicativePartial(LoxString, LoxString, lstring, lstring)
  elif T is bool and R is bool:
    LoxValueApplicativePartial(LoxBool, LoxBool, lbool, lbool)
  raise newException(RuntimeError, "(double <$>) Expected value of type " & $T &
      ", but got " & $value.valType)

proc `<*>`(fn: LoxValue, value: LoxValue): LoxValue =
  if fn.valType != LoxCallable:
    raise newException(RuntimeError, "Using `<*>` on a non-callable LoxValue.")
  return fn.callable.call(@[value])

method evaluate*(expression: Expr): LoxValue {.base, locks: "unknown".} =
  raise newException(RuntimeError, "Unrecognized expression type\n" & $expression)

method evaluate*(expression: LiteralExpr): LoxValue =
  case expression.ltype:
    of NumberLiteralExpr:
      result = LoxValue(valType: LoxNumber, lnumber: expression.num)
    of StringLiteralExpr:
      result = LoxValue(valType: LoxString, lstring: expression.str)
    of BoolLiteralExpr:
      result = LoxValue(valType: LoxBool, lbool: expression.bl)
    of NilLiteralExpr:
      result = LoxValue(valType: LoxNil)

method evaluate*(expression: UnaryExpr): LoxValue =
  let right = expression.right.evaluate
  case expression.operator.ttype:
    of TokenType.MINUS:
      return ((x: float) => -1*x) <$> right
    of TokenType.BANG:
      if right.valType == LoxNil:
        return trueValue
      return ((x: bool) => not x) <$> right
    else:
      raise newException(RuntimeError, "Not a unary expression")

method evaluate*(expression: BinaryExpr): LoxValue =
  let left = evaluate expression.left
  case expression.operator.ttype:
    of TokenType.AND:
      if not isTruthy left: return falseValue
      if not expression.right.evaluate.isTruthy: return falseValue
      return trueValue
    of TokenType.OR:
      if isTruthy left: return trueValue
      if expression.right.evaluate.isTruthy: return trueValue
      return falseValue
    else:
      discard
  let right = evaluate expression.right
  case expression.operator.ttype:
    of TokenType.PLUS:
      if left.valType == LoxString:
        return ((x, y: string) => x & y) <$> left <*> right
      return ((x, y: float) => x + y) <$> left <*> right
    of TokenType.MINUS:
      return ((x, y: float) => x - y) <$> left <*> right
    of TokenType.STAR:
      return ((x, y: float) => x * y) <$> left <*> right
    of TokenType.SLASH:
      return ((x, y: float) => x / y) <$> left <*> right
    of TokenType.GREATER:
      return ((x, y: float) => x > y) <$> left <*> right
    of TokenType.GREATER_EQUAL:
      return ((x, y: float) => x >= y) <$> left <*> right
    of TokenType.LESS:
      return ((x, y: float) => x < y) <$> left <*> right
    of TokenType.LESS_EQUAL:
      return ((x, y: float) => x <= y) <$> left <*> right
    of TokenType.EQUAL_EQUAL:
      if left.valType == LoxNil and right.valType == LoxNil:
        return trueValue
      elif left.valType == LoxNil or right.valType == LoxNil:
        return falseValue
      elif left.valType == LoxBool:
        return ((x, y: bool) => x == y) <$> left <*> right
      return ((x, y: float) => x == y) <$> left <*> right
    of TokenType.BANG_EQUAL:
      if left.valType == LoxNil and right.valType == LoxNil:
        return falseValue
      elif left.valType == LoxNil or right.valType == LoxNil:
        return trueValue
      elif left.valType == LoxBool:
        return ((x, y: bool) => x != y) <$> left <*> right
      return ((x, y: float) => x != y) <$> left <*> right
    else:
      raise newException(RuntimeError, "Not a binary operator.")

method evaluate*(expression: Grouping): LoxValue =
  return expression.expression.evaluate

method evaluate*(expression: VariableExpr): LoxValue =
  return lookupVariable(expression.name, expression)

method evaluate*(expression: AssignmentExpr): LoxValue =
  result = evaluate expression.value
  if expression in locals:
    currentEnvironment.assignAt(locals[expression], expression.name, result)
  else:
    globals.assign(expression.name, result)

method evaluate*(expression: CallExpr): LoxValue =
  let callee = evaluate expression.callee
  var arguments: seq[LoxValue] = @[]
  for arg in expression.arguments:
    arguments &= evaluate arg
  case callee.valType:
    of LoxCallable:
      if arguments.len != callee.callable.arity:
        raise newException(RuntimeError, "Expected " & $callee.callable.arity &
            " arguments, but got " & $arguments.len & ".")
      return callee.callable.call(arguments)
    of LoxClass:
      if arguments.len != callee.lclass.arity:
        raise newException(RuntimeError, "Expected " & $callee.lclass.arity &
            " arguments, but got " & $arguments.len & ".")
      return callee.lclass.call(arguments)
    else:
      raise newException(RuntimeError, "Can only call functions and classes.")

method execute*(statement: Stmt) {.base.} =
  raise newException(RuntimeError, "Unrecognized statement type.")

proc executeBlock(statements: seq[Stmt], environment: Environment) =
  let previous = currentEnvironment
  defer: currentEnvironment = previous
  currentEnvironment = environment
  for statement in statements:
    execute statement

proc callAnon(fn: AnonFnExpr, arguments: seq[LoxValue],
    closure: Environment): LoxValue =
  var environment = newEnvironment closure
  for (param, arg) in zip(fn.params, arguments):
    environment.define(param.lexeme, arg)
  try:
    executeBlock(fn.body, environment)
    return nilValue
  except Return:
    return ReturnRef(getCurrentException()).value

method evaluate*(expression: AnonFnExpr): LoxValue =
  let closure = currentEnvironment
  let function = LoxCallableImpl(
    call: (args: seq[LoxValue])=>callAnon(expression, args, closure),
    arity: expression.params.len,
    closure: closure,
    isInitializer: false,
    name: "anonymous")
  return LoxValue(valType: LoxCallable, callable: function)

proc callFn(fn: Function, arguments: seq[LoxValue],
    closure: Environment, isInitializer: bool = false): LoxValue

proc bindTo(thisMethod: LoxCallableImpl,
    instance: LoxInstanceImpl): LoxCallableImpl =
  let environment = newEnvironment thisMethod.closure
  environment.define("this", LoxValue(valType: LoxInstance,
      linstance: instance))
  return LoxCallableImpl(
    call: (args: seq[LoxValue])=>callFn(thisMethod.declaration, args,
        environment, thisMethod.isInitializer),
    arity: thisMethod.arity,
    name: thisMethod.name,
    closure: environment,
    isInitializer: thisMethod.isInitializer,
    declaration: thisMethod.declaration)

proc findMethod(klass: LoxClassImpl, name: string): LoxCallableImpl =
  if name in klass.methods:
    return klass.methods[name]
  elif name in klass.attrs:
    return klass.attrs[name]
  elif klass.superclass != nil:
    return klass.superclass.findMethod(name)
  else:
    return nil

proc get(instance: LoxInstanceImpl, name: Token): LoxValue =
  if name.lexeme in instance.fields:
    return instance.fields[name.lexeme]
  let callable = instance.klass.findMethod(name.lexeme)
  if callable != nil:
    return LoxValue(valType: LoxCallable, callable: callable.bindTo(instance))
  raise newException(RuntimeError, "Undefined property '" & name.lexeme & "'.")

proc isAttr(klass: LoxClassImpl, name: string): bool =
  if name in klass.attrs: return true
  elif klass.superclass != nil: return klass.superclass.isAttr(name)
  else: return false

method evaluate*(expression: GetExpr): LoxValue =
  let obj = evaluate expression.obj
  if obj.valType == LoxInstance:
    if obj.linstance.klass.isAttr(expression.name.lexeme):
      return get(obj.linstance, expression.name).callable.call(@[])
    return get(obj.linstance, expression.name)
  elif obj.valType == LoxClass:
    return get(obj.lclass, expression.name)
  raise newException(RuntimeError, "Only instances have properties.")

proc set(instance: LoxInstanceImpl, name: Token, value: LoxValue) =
  instance.fields[name.lexeme] = value

method evaluate*(expression: SetExpr): LoxValue =
  let obj = evaluate expression.obj
  if obj.valType != LoxInstance:
    raise newException(RuntimeError, "Only instances have fields.")
  result = evaluate expression.value
  set(obj.linstance, expression.name, result)

method evaluate*(expression: ThisExpr): LoxValue =
  return lookUpVariable(expression.keyword, expression)

method evaluate*(expression: SuperExpr): LoxValue =
  let distance = locals[expression]
  let superclass = currentEnvironment.getAt(distance, "super")
  if superclass.valType != LoxClass:
    raise newException(RuntimeError, "Super is not a class???")
  let instance = currentEnvironment.getAt(distance - 1, "this")
  if instance.valType != LoxInstance:
    raise newException(RuntimeError, "This is not an instance???")
  let calledMethod = superclass.lclass.findMethod(expression.calledMethod.lexeme)
  if calledMethod == nil:
    raise newException(RuntimeError, "Undefined property '" & expression.calledMethod.lexeme & "'.")
  if superclass.lclass.isAttr(calledMethod.name):
    return calledMethod.bindTo(instance.linstance).call(@[])
  return LoxValue(valType: LoxCallable, callable: calledMethod.bindTo(instance.linstance))

method execute*(statement: PrintStmt) =
  echo statement.expression.evaluate

method execute*(statement: ExpressionStmt) =
  discard evaluate statement.expression

method execute*(statement: VarDecl) =
  let value: LoxValue = if statement.initializer != nil:
      evaluate statement.initializer
    else:
      LoxValue(valType: LoxNil)
  currentEnvironment.define(statement.name.lexeme, value)

method execute*(statement: IfStmt) =
  if statement.condition.evaluate.isTruthy:
    execute statement.thenBranch
  elif statement.elseBranch != nil:
    execute statement.elseBranch

method execute*(statement: WhileStmt) =
  while statement.condition.evaluate.isTruthy:
    statement.body.execute

method execute*(statement: BlockStmt) =
  executeBlock statement.statements, newEnvironment(currentEnvironment)

proc callFn(fn: Function, arguments: seq[LoxValue],
    closure: Environment, isInitializer: bool = false): LoxValue =
  var environment = newEnvironment closure
  for (param, arg) in zip(fn.params, arguments):
    environment.define(param.lexeme, arg)
  try:
    executeBlock(fn.body, environment)
    if isInitializer: return closure.getAt(0, "this")
    return nilValue
  except Return:
    if isInitializer: return closure.getAt(0, "this")
    return ReturnRef(getCurrentException()).value

method execute*(statement: Function) =
  let closure = currentEnvironment
  let function = LoxCallableImpl(
    call: (args: seq[LoxValue])=>callFn(statement, args, closure, false),
    arity: statement.params.len,
    closure: closure,
    declaration: statement,
    isInitializer: false,
    name: statement.name.lexeme)
  currentEnvironment.define(statement.name.lexeme, LoxValue(
      valType: LoxCallable, callable: function))

method execute*(statement: ReturnStmt) =
  let value = if statement.value == nil: nilValue else: evaluate statement.value
  var e: ref Return
  new e
  e.msg = ""
  e.value = value
  raise e

proc callClass*(klass: LoxClassImpl, arguments: seq[LoxValue]): LoxValue =
  let instance = LoxInstanceImpl(klass: klass, fields: initTable[string,
      LoxValue]())
  let initializer = klass.findMethod("init")
  if initializer != nil:
    discard initializer.bindTo(instance).call(arguments)
  return LoxValue(valType: LoxInstance, linstance: instance)

method execute*(statement: ClassDecl) =
  let superclass = if statement.superclass == nil: nilValue else: evaluate(statement.superclass)
  if superclass.valType != LoxNil and superclass.valType != LoxClass:
    raise newException(RuntimeError, "Superclass must be a class.")
  currentEnvironment.define(statement.name.lexeme, nilValue)
  if statement.superclass != nil:
    currentEnvironment = newEnvironment(currentEnvironment)
    currentEnvironment.define("super", superclass)
  let closure = currentEnvironment
  var methods = initTable[string, LoxCallableImpl]()
  var classMethods = initTable[string, LoxCallableImpl]()
  var attrs = initTable[string, LoxCallableImpl]()
  for thisMethod in statement.methods:
    let newMethod = thisMethod
    let isInitializer = thisMethod.name.lexeme == "init"
    let callable = LoxCallableImpl(
      call: (args: seq[LoxValue])=>callFn(newMethod, args, closure,
          isInitializer), arity: thisMethod.params.len, name: thisMethod.name.lexeme,
          closure: closure, declaration: newMethod,
              isInitializer: newMethod.name.lexeme == "init")
    methods[thisMethod.name.lexeme] = callable
  for thisMethod in statement.classMethods:
    let newMethod = thisMethod
    let callable = LoxCallableImpl(
      call: (args: seq[LoxValue])=>callFn(newMethod, args, closure, false),
          arity: thisMethod.params.len, name: thisMethod.name.lexeme,
          closure: closure, declaration: newMethod, isInitializer: false)
    classMethods[thisMethod.name.lexeme] = callable
  for thisMethod in statement.attrs:
    let newMethod = thisMethod
    let isInitializer = thisMethod.name.lexeme == "init"
    let callable = LoxCallableImpl(
      call: (args: seq[LoxValue])=>callFn(newMethod, args, closure,
          isInitializer), arity: thisMethod.params.len, name: thisMethod.name.lexeme,
          closure: closure, declaration: newMethod,
              isInitializer: newMethod.name.lexeme == "init")
    attrs[thisMethod.name.lexeme] = callable
  let superclassImpl = if superclass.valType == LoxClass: superclass.lclass else: nil
  let supermetaclass = if superclassImpl == nil: nil else: superclassImpl.klass
  let metaClass = LoxClassImpl(name: "MetaClass[" & statement.name.lexeme & "]",
    superclass: supermetaclass,
    call: nil, arity: 0,
    fields: initTable[string, LoxValue](),
    attrs: initTable[string, LoxCallableImpl](),
    methods: classMethods)
  let arity = if "init" in methods: methods["init"].arity else: 0
  var thisClass = LoxClassImpl(name: statement.name.lexeme, klass: metaClass,
      call: nil, arity: arity, fields: initTable[string, LoxValue](),
      methods: methods, attrs: attrs, superclass: superclassImpl)
  let constructor = (x: seq[LoxValue]) => callClass(thisClass, x)
  thisClass.call = constructor
  if statement.superclass != nil:
    currentEnvironment = currentEnvironment.enclosing
  currentEnvironment.assign(statement.name, LoxValue(valType: LoxClass,
      lclass: thisClass))

proc beginScope() =
  scopes.add(initTable[string, bool]())

proc endScope() =
  discard scopes.pop

proc declare(name: Token) =
  if scopes.len == 0:
    return
  if name.lexeme in scopes[^1]:
    error(name.line, "Already a variable with this name in this scope.")
  scopes[^1][name.lexeme] = false

proc define(name: Token) =
  if scopes.len == 0:
    return
  scopes[^1][name.lexeme] = true

method resolve(statement: Stmt) {.base.} =
  return

method resolve(expression: Expr) {.base.} =
  return

proc resolve*(statements: seq[Stmt]) =
  for statement in statements:
    resolve statement

proc resolve(expression: Expr, depth: int) =
  locals[expression] = depth

proc resolveLocal(expression: Expr, name: Token) =
  for i, scope in enumerate(scopes):
    if name.lexeme in scope:
      expression.resolve (scopes.len - 1 - i)

proc resolveFunction[T: Function | AnonFnExpr](function: T,
    ftype: FunctionType) =
  let enclosingFunction = currentFunction
  defer: currentFunction = enclosingFunction
  currentFunction = ftype
  beginScope()
  for param in function.params:
    declare param
    define param
  resolve function.body
  endScope()

method resolve(statement: BlockStmt) =
  beginScope()
  resolve statement.statements
  endScope()

method resolve(statement: VarDecl) =
  declare statement.name
  if statement.initializer != nil:
    resolve statement.initializer
  define statement.name

method resolve(statement: Function) =
  declare statement.name
  define statement.name
  resolveFunction statement, FunctionType.FUNCTION

method resolve(statement: ExpressionStmt) =
  resolve statement.expression

method resolve(statement: IfStmt) =
  resolve statement.condition
  resolve statement.thenBranch
  if statement.elseBranch != nil:
    resolve statement.elseBranch

method resolve(statement: PrintStmt) =
  resolve statement.expression

method resolve(statement: ReturnStmt) =
  if currentFunction == FunctionType.NONE:
    error(statement.keyword.line, "Can't return from top-level code.")
  if statement.value != nil:
    if currentFunction == INITIALIZER:
      error(statement.keyword.line, "Can't return a value from an initializer.")
    resolve statement.value

method resolve(statement: WhileStmt) =
  resolve statement.condition
  resolve statement.body

method resolve(statement: ClassDecl) =
  var enclosingClass = currentClass
  defer: currentClass = enclosingClass
  currentClass = ClassType.CLASS
  declare statement.name
  define statement.name
  if statement.superclass != nil:
    if statement.superclass.name.lexeme == statement.name.lexeme:
      error(statement.superclass.name.line, "A class can't inherit from itself.")
    currentClass = ClassType.SUBCLASS
    resolve statement.superclass
    beginScope()
#    defer: endScope()
    scopes[^1]["super"] = true
  for thisMethod in statement.classMethods:
    resolveFunction(thisMethod, METHOD)
  beginScope()
  scopes[^1]["this"] = true
  for thisMethod in statement.methods:
    let declaration = if thisMethod.name.lexeme ==
        "init": INITIALIZER else: METHOD
    resolveFunction(thisMethod, declaration)
  for thisMethod in statement.attrs:
    resolveFunction(thisMethod, METHOD)
  endScope()
  if statement.superclass != nil:
    endScope()

method resolve(expression: AnonFnExpr) =
  resolveFunction expression, FunctionType.FUNCTION

method resolve(expression: AssignmentExpr) =
  resolve expression.value
  expression.resolveLocal expression.name

method resolve(expression: CallExpr) =
  resolve expression.callee
  for arg in expression.arguments:
    resolve arg

method resolve(expression: VariableExpr) =
  if scopes.len != 0 and expression.name.lexeme in scopes[^1] and not scopes[
      ^1][expression.name.lexeme]:
    raise newException(RuntimeError, "Can't read local variable in its own initializer")
  expression.resolveLocal expression.name

method resolve(expression: BinaryExpr) =
  resolve expression.left
  resolve expression.right

method resolve(expression: Grouping) =
  resolve expression.expression

method resolve(expression: LiteralExpr) =
  return

method resolve(expression: UnaryExpr) =
  resolve expression.right

method resolve(expression: GetExpr) =
  resolve expression.obj

method resolve(expression: SetExpr) =
  resolve expression.value
  resolve expression.obj

method resolve(expression: ThisExpr) =
  if currentClass == ClassType.NONEClass:
    error(expression.keyword.line, "Can't use 'this' outside of a class.")
  else:
    resolveLocal(expression, expression.keyword)

method resolve(expression: SuperExpr) =
  if currentClass == ClassType.NONEClass:
    error(expression.keyword.line, "Can't use 'super' outside of a class.")
  elif currentClass == ClassType.CLASS:
    error(expression.keyword.line, "Can't use 'super' in a class with no superclass.")
  expression.resolveLocal(expression.keyword)

proc initInterpreter*() =
  globals.define("clock", LoxValue(valType: LoxCallable, callable: clockFn))

