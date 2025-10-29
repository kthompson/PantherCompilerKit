import org.scalatest.Inside
import panther.{assert as panthAssert, *}
import org.scalatest.matchers.should.Matchers

trait TestHelpers extends Matchers with Inside {

  def mkTokens(text: string): Array[SyntaxToken] = {
    val sourceFile = new SourceFile(text, "test.pn")
    val diagnostics = new DiagnosticBag(CompilerSettingsFactory.default)
    val lexer = new Lexer(sourceFile, diagnostics)
    MakeTokenList.create(lexer)
  }

  def mkCompilation(text: string): Compilation = {
    val tree =
      MakeSyntaxTree.parseContent(text, CompilerSettingsFactory.default)
    val comp = MakeCompilation.create(
      ListModule.one(tree),
      CompilerSettingsFactory.default
    )
    if (comp.diagnostics.count() > 0) {
      comp.diagnostics.printDiagnostics(20)
      comp.diagnostics.count() should be(0)
    }
    comp
  }

  def mkSyntaxTree(text: string): SyntaxTree =
    MakeSyntaxTree.parseContent(text, CompilerSettingsFactory.default)

  def mkBinaryExpr(text: string): Expression.Binary = {
    val expression = mkSyntaxTreeExpr(text)
    assertBinaryExpr(expression)
  }

  def mkMemberAccessExpr(text: string): Expression.MemberAccess = {
    val expression = mkSyntaxTreeExpr(text)
    assertMemberAccess(expression)
  }

  def mkMatchExpr(text: string): Expression.Match = {
    val expression = mkSyntaxTreeExpr(text)
    assertMatch(expression)
  }

  def mkAssignmentExpr(text: string): Expression.Assignment = {
    val expression = mkSyntaxTreeExpr(text)
    assertAssignmentExpr(expression)
  }

  def mkCallExpr(text: string): Expression.Call = {
    val expression = mkSyntaxTreeExpr(text)
    assertCallExpr(expression)
  }

  def mkIfExpr(text: string): Expression.If = {
    val expression = mkSyntaxTreeExpr(text)
    assertIfExpr(expression)
  }

  def mkWhileExpr(text: string): Expression.While = {
    val expression = mkSyntaxTreeExpr(text)
    assertWhileExpr(expression)
  }

  def mkUnaryExpr(text: string): Expression.Unary = {
    val expr = mkSyntaxTreeExpr(text)
    assertUnaryExpr(expr)
  }

  def mkGroupExpr(text: string): Expression.Group = {
    val expr = mkSyntaxTreeExpr(text)
    assertGroupExpr(expr)
  }

  def mkBlockExpr(text: string): Expression.Block = {
    val expr = mkSyntaxTreeExpr(text)
    assertBlockExpr(expr)
  }

  def mkIsExpr(text: string): Expression.Is = {
    val expression = mkSyntaxTreeExpr(text)
    assertIsExpr(expression)
  }

  def mkSyntaxTreeExpr(text: string): Expression = {
    inside(mkSyntaxTreeStatement(text)) {
      case StatementSyntax.ExpressionStatement(expr) => expr
    }
  }

  def mkSyntaxTreeStatement(text: string): StatementSyntax = {
    inside(mkMember(text)) {
      case MemberSyntax.GlobalStatementSyntax(statement) => statement
    }
  }

  def mkFunctionMember(text: string): MemberSyntax.FunctionDeclarationSyntax = {
    inside(mkMember(text)) {
      case member: MemberSyntax.FunctionDeclarationSyntax => member
    }
  }

  def mkMember(text: string): MemberSyntax = {
    val tree = mkSyntaxTree(text)
    assertSingle(tree.root.members)
  }

  def assertSingle[T](items: List[T]): T = {
    inside(items) { case List.Cons(head, List.Nil) => head }
  }

  def assertSingle[T](items: Array[T]): T = {
    inside(items) { case Array(head) => head }
  }

  def assertIndex[T](i: int, list: List[T]): T = {
    if (i < 0 || i >= list.length)
      fail("index out of bounds: " + i)
    list.getUnsafe(i)
  }

  def assertBinaryExpr(expression: Expression): Expression.Binary = {
    inside(expression) { case expr: Expression.Binary => expr }
  }

  def assertMemberAccess(expression: Expression): Expression.MemberAccess =
    inside(expression) { case expr: Expression.MemberAccess => expr }

  def assertMatch(expression: Expression): Expression.Match =
    inside(expression) { case expr: Expression.Match => expr }

  def assertLiteralPattern(expression: PatternSyntax): PatternSyntax.Literal =
    inside(expression) { case expr: PatternSyntax.Literal => expr }

  def assertAssignmentExpr(expression: Expression): Expression.Assignment =
    inside(expression) { case expr: Expression.Assignment => expr }

  def assertIfExpr(
      expression: Expression
  ): Expression.If = {
    inside(expression) { case expr: Expression.If => expr }
  }

  def assertCallExpr(
      expression: Expression
  ): Expression.Call = {
    inside(expression) { case expr: Expression.Call => expr }
  }

  def assertWhileExpr(
      expression: Expression
  ): Expression.While = {
    inside(expression) { case expr: Expression.While => expr }
  }

  def assertGroupExpr(expr: Expression): Expression.Group = {
    inside(expr) { case expr: Expression.Group => expr }
  }

  def assertBlockExpr(expr: Expression): Expression.Block = {
    inside(expr) { case expr: Expression.Block => expr }
  }

  def assertUnaryExpr(expr: Expression): Expression.Unary = {
    inside(expr) { case expr: Expression.Unary => expr }
  }

  def assertIsExpr(expr: Expression): Expression.Is = {
    inside(expr) { case expr: Expression.Is => expr }
  }

  def assertVariableDeclaration(
      statement: StatementSyntax
  ): StatementSyntax.VariableDeclarationStatement = {
    inside(statement) {
      case stmt: StatementSyntax.VariableDeclarationStatement => stmt
    }
  }

  def assertNumberToken(
      expected: int,
      token: SyntaxToken
  ): Unit = {
    token.kind should be(SyntaxKind.NumberToken)
    inside(token.value) { case SyntaxTokenValue.Number(value) =>
      value should be(expected)
    }
  }

  def assertNumberExpr(expected: int, expression: Expression): Unit = {
    inside(expression) {
      case Expression.Literal(_, SyntaxTokenValue.Number(n)) =>
        n should be(expected)
    }
  }

  def assertBoolExpr(expected: bool, expression: Expression): Unit = {
    inside(expression) {
      case Expression.Literal(_, SyntaxTokenValue.Boolean(b)) =>
        b should be(expected)

    }
  }

  def assertTrueExpr(expression: Expression): Unit = {
    inside(expression) {
      case Expression.Literal(_, SyntaxTokenValue.Boolean(true)) =>
    }
  }

  def assertFalseExpr(expression: Expression): Unit = {
    inside(expression) {
      case Expression.Literal(_, SyntaxTokenValue.Boolean(false)) =>
    }
  }

  def assertIdentifierExpr(expected: string, expression: Expression): Unit = {
    inside(expression) {
      case Expression.IdentifierName(
            SimpleNameSyntax.IdentifierNameSyntax(token)
          ) =>
        token.text should be(expected)
    }
  }

  def assertSimpleNameIdentifierExpr(
      expected: string,
      name: SimpleNameSyntax
  ): Unit = {
    inside(name) { case SimpleNameSyntax.IdentifierNameSyntax(token) =>
      token.text should be(expected)
    }
  }

  def assertGenericIdentifierExpr(
      expression: Expression
  ): SimpleNameSyntax.GenericNameSyntax = {
    inside(expression) {
      case Expression.IdentifierName(a: SimpleNameSyntax.GenericNameSyntax) =>
        a
    }
  }

  def assertTokenKind(expected: int, token: SyntaxToken): Unit =
    if (expected != token.kind) {
      fail(
        "expected " + SyntaxFacts.getKindName(expected) + ", got " + SyntaxFacts
          .getKindName(token.kind)
      )
    }

  def assertTokenText(expected: string, token: SyntaxToken): Unit =
    token.text should be(expected)

  def assertName(expected: string, actual: NameSyntax): Unit = {
    val sb = IndentedStringBuilder(false)
    val printer = new AstPrinter(false, sb)
    printer.printName(actual)
    Trim.both(printer.toString()) should be(expected)
  }

  def assertSome[T](option: Option[T]): T = option match {
    case Option.Some(value) => value
    case Option.None =>
      fail("expected Some, found None")
  }

  def assertNone[T](option: Option[T]): Unit = {
    if (option.isDefined())
      fail("expected None, found Some")
  }

  def assertEmpty[T](list: List[T]): Unit = {
    if (!list.isEmpty)
      fail(
        "expected empty list, found " + list.length + " items"
      )
  }

  def enumerateSymbols(compilation: Compilation): ChainEnumerator[Symbol] =
    new ChainEnumerator(compilation.getSymbols())

  def assertProgramSymbol(
      enumerator: ChainEnumerator[Symbol]
  ): Symbol = {
    assert(enumerator.moveNext())
    val program = enumerator.current()
    assert(program.kind == SymbolKind.Object)

    assert(
      program.name == "$Program" || program.name == "Program"
    )

    // $runtimeInit
    assert(enumerator.moveNext())
    val runtimeInit = enumerator.current()
    assert(
      runtimeInit.kind == SymbolKind.Method
    )
    assert(runtimeInit.name == "$runtimeInit")

    program
  }

  def enumerateSymbolsSkipBuiltin(
      compilation: Compilation
  ): ChainEnumerator[Symbol] = {
    val enumerator = new ChainEnumerator(compilation.getSymbols())

    assertSymbol(enumerator, SymbolKind.Class, "any")
    assertSymbol(enumerator, SymbolKind.Class, "int")
    assertConversionMethod(enumerator)
    assertSymbol(enumerator, SymbolKind.Class, "string")
    assertSymbol(enumerator, SymbolKind.Field, "length")
    assertConversionMethod(enumerator)
    assertSymbol(enumerator, SymbolKind.Class, "bool")
    assertConversionMethod(enumerator)
    assertSymbol(enumerator, SymbolKind.Class, "char")
    assertConversionMethod(enumerator)
    assertSymbol(enumerator, SymbolKind.Class, "unit")
    assertSymbol(enumerator, SymbolKind.Class, "Array")
    assertSymbol(enumerator, SymbolKind.TypeParameter(Variance.Invariant), "T")
    assertSymbol(enumerator, SymbolKind.Constructor, ".ctor")
    assertSymbol(enumerator, SymbolKind.Parameter, "size")
    assertSymbol(enumerator, SymbolKind.Field, "length")
    assertSymbol(enumerator, SymbolKind.Method, "apply")
    assertSymbol(enumerator, SymbolKind.Parameter, "index")
    //    assertSymbol(enumerator, SymbolKind.Object, "predef")
    assertSymbol(enumerator, SymbolKind.Method, "println")
    assertSymbol(enumerator, SymbolKind.Parameter, "message")
    assertSymbol(enumerator, SymbolKind.Method, "print")
    assertSymbol(enumerator, SymbolKind.Parameter, "message")
    assertSymbol(enumerator, SymbolKind.Method, "panic")
    assertSymbol(enumerator, SymbolKind.Parameter, "message")
    assertSymbol(enumerator, SymbolKind.Method, "assert")
    assertSymbol(enumerator, SymbolKind.Parameter, "condition")
    assertSymbol(enumerator, SymbolKind.Parameter, "message")
    assertSymbol(enumerator, SymbolKind.Method, "mod")
    assertSymbol(enumerator, SymbolKind.Parameter, "a")
    assertSymbol(enumerator, SymbolKind.Parameter, "b")

    enumerator
  }

  def assertConversionMethod(enumerator: ChainEnumerator[Symbol]) = {
    assertSymbol(enumerator, SymbolKind.Method, "apply")
    assertSymbol(enumerator, SymbolKind.Parameter, "value")
  }

  def assertSymbol(
      enumerator: ChainEnumerator[Symbol],
      kind: SymbolKind,
      name: string
  ): Symbol = {
    assert(enumerator.moveNext())
    val symbol = enumerator.current()
    assert(symbol.name == name)
    assert(symbol.kind == kind)
    symbol
  }

  def assertMainSymbol(enumerator: ChainEnumerator[Symbol]): Symbol =
    assertSymbol(enumerator, SymbolKind.Method, "main")

  def assertNoSymbols(
      enumerator: ChainEnumerator[Symbol]
  ): Unit = {
    if (enumerator.moveNext()) {
      val symbol = enumerator.current()
      fail(
        "Unexpected symbol: " + symbol.kind + " " + symbol.name
      )
    }
  }

  def assertExprTypeWithSetup(
      setup: string,
      expression: string,
      expectedType: string
  ): Unit = {
    val comp = mkCompilation(setup + "\n\nval typeTestSymbol = " + expression)
    val program = assertSome(comp.root.lookup("$Program"))
    val symbol = assertSome(program.lookup("typeTestSymbol"))
    assertSymbolType(comp, symbol, expectedType)
  }

  def assertAssignableToWithSetup(
      setup: string,
      expression: string,
      expectedType: string
  ): Unit = {
    mkCompilation(
      setup + "\n\nval typeTestSymbol: " + expectedType + " = " + expression
    )
  }

  def assertExprTypeTest(expression: string, expectedType: string): Unit = {
    assertExprType(expression, expectedType)
  }

  def assertExprType(expression: string, expectedType: string): Unit = {
    val comp = mkCompilation("val x = " + expression)
    val symbols = enumerateSymbolsSkipBuiltin(comp)

    assertProgramSymbol(symbols)

    val x = assertSymbol(symbols, SymbolKind.Field, "x")
    assertSymbolType(comp, x, expectedType)

    assertMainSymbol(symbols)
    //    assertNoSymbols(symbols)
  }

  def assertSymbolType(comp: Compilation, symbol: Symbol, typ: string): unit = {
    val symbolType = comp.binder.tryGetSymbolType(symbol)
    symbolType match {
      case Option.Some(value) =>
        assert(value.toString() == typ)
      case Option.None =>
        fail(
          "Expected symbol " + symbol.name + " to have a type " + typ
            + " but got none"
        )
    }
  }

  def assertAssignableTo(expression: string, assignableTo: string): unit =
    mkCompilation("val typeTestSymbol: " + assignableTo + " = " + expression)

  def execValue(program: string): Value = {
    val compilation = mkCompilation(program)
    compilation.exec() match {
      case InterpretResult.OkValue(value) =>
        value
      case result =>
        fail("Expected exec result, got: " + result)
    }
  }

  def assertValueInt(value: Value, expected: int): Unit = {
    value match {
      case Value.Int(v) =>
        assert(v == expected)
      case _ =>
        fail(
          "Expected " + string(expected) + ", got: " + value
        )
    }
  }

  def assertValueBool(value: Value, expected: bool): Unit = {
    value match {
      case Value.Bool(v) =>
        assert(v == expected)
      case _ =>
        fail(
          "Expected " + string(expected) + ", got: " + value
        )
    }
  }

  def assertValueString(value: Value, expected: string): Unit = {
    value match {
      case Value.String(v) =>
        assert(v == expected)
      case _ =>
        fail("Expected string value, got: " + value)
    }
  }

  def assertExecValueIntWithSetup(
      setup: string,
      program: string,
      expected: int
  ): Unit = {
    val value = execValue(setup + "\n\n" + program)
    assertValueInt(value, expected)
  }

  def assertExecValueInt(
      program: string,
      expected: int
  ): Unit = {
    val value = execValue(program)
    assertValueInt(value, expected)
  }

  def assertExecValueBool(
      program: string,
      expected: bool
  ): Unit = {
    val value = execValue(program)
    assertValueBool(value, expected)
  }

  def assertExecValueString(
      program: string,
      expected: string
  ): Unit = {
    val value = execValue(program)
    assertValueString(value, expected)
  }

  def assertExecValueBoolWithSetup(
      setup: string,
      program: string,
      expected: bool
  ): Unit = {
    val value = execValue(setup + "\n\n" + program)
    assertValueBool(value, expected)
  }

  def assertExecValueStringWithSetup(
      setup: string,
      program: string,
      expected: string
  ): Unit = {
    val value = execValue(setup + "\n\n" + program)
    assertValueString(value, expected)
  }
}
