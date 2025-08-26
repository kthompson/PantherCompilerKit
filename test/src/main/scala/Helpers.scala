import panther._
import TestFramework._

object Helpers {

  def mkTokens(text: string): Array[SyntaxToken] = {
    val sourceFile = new SourceFile(text, "test.pn")
    val diagnostics = new DiagnosticBag(CompilerSettingsFactory.default)
    val lexer = new Lexer(sourceFile, diagnostics)
    MakeTokenList.create(lexer)
  }

  def mkCompilation(text: string): Compilation = {
    val tree = MakeSyntaxTree.parseContent(
      text,
      CompilerSettingsFactory.default
    )
    val comp = MakeCompilation.create(
      ListModule.one(tree),
      CompilerSettingsFactory.default
    )
    if (comp.diagnostics.count() > 0) {
      comp.diagnostics.printDiagnostics(20)
      failed("Compilation failed")
    }
    comp
  }

  def assertExprTypeWithSetup(
      setup: string,
      expression: string,
      expectedType: string
  ): unit = {
    test("typeof " + expression + " is " + expectedType)
    val comp = mkCompilation(setup + "\n\nval typeTestSymbol = " + expression)
    val program = Assert.some(comp.root.lookup("$Program"))
    val symbol = Assert.some(program.lookup("typeTestSymbol"))
    assertSymbolType(comp, symbol, expectedType)
  }

  def assertAssignableToWithSetup(
      setup: string,
      expression: string,
      expectedType: string
  ): unit = {
    mkCompilation(
      setup + "\n\nval typeTestSymbol: " + expectedType + " = " + expression
    )
  }

  def execValue(program: string): Value = {
    val compilation = mkCompilation(program)
    compilation.exec() match {
      case InterpretResult.OkValue(value) =>
        value
      case result =>
        failed("Expected exec result, got: " + result)
    }
  }

  def assertValueInt(value: Value, expected: int): unit = {
    value match {
      case Value.Int(v) =>
        Assert.intEqual(expected, v)
      case _ =>
        failed("Expected " + string(expected) + ", got: " + value)
    }
  }

  def assertValueBool(value: Value, expected: bool): unit = {
    assertValueInt(value, if (expected) 1 else 0)
  }

  def assertValueString(value: Value, expected: string): unit = {
    value match {
      case Value.String(v) =>
        Assert.stringEqual(expected, v)
      case _ =>
        failed("Expected string value, got: " + value)
    }
  }

  def assertExecValueIntWithSetup(
      setup: string,
      program: string,
      expected: int
  ): unit = {
    test(program)
    val value = execValue(setup + "\n\n" + program)
    assertValueInt(value, expected)
  }

  def assertExecValueInt(
      program: string,
      expected: int
  ): unit = {
    test(program)
    val value = execValue(program)
    assertValueInt(value, expected)
  }

  def assertExecValueBool(
      program: string,
      expected: bool
  ): unit = {
    test(program)
    val value = execValue(program)
    assertValueBool(value, expected)
  }

  def assertExecValueString(
      program: string,
      expected: string
  ): unit = {
    test(program)
    val value = execValue(program)
    assertValueString(value, expected)
  }

  def assertAssignableTo(expression: string, assignableTo: string): unit =
    mkCompilation("val typeTestSymbol: " + assignableTo + " = " + expression)

  def assertExprTypeTest(expression: string, expectedType: string): unit = {
    test(expression.replaceAll("\n", " "))
    assertExprType(expression, expectedType)
  }

  def assertExprType(expression: string, expectedType: string): unit = {
    val comp = mkCompilation("val x = " + expression)
    val symbols = enumNonBuiltinSymbols(comp)

    assertProgramSymbol(symbols)

    val x = assertSymbol(symbols, SymbolKind.Field, "x")
    assertSymbolType(comp, x, expectedType)

    assertMainSymbol(symbols)
//    assertNoSymbols(symbols)
  }

  def enumSymbols(compilation: Compilation): ChainEnumerator[Symbol] =
    new ChainEnumerator(compilation.getSymbols())

  def assertProgramSymbol(
      enumerator: ChainEnumerator[Symbol]
  ): Symbol = {
    Assert.isTrue(enumerator.moveNext())
    val program = enumerator.current()
    Assert.equal(SymbolKind.Object, program.kind)

    Assert.assert(
      program.name == "$Program" || program.name == "Program",
      "expected 'Program' or '$Program', got: " + program.name
    )

    // $runtimeInit
    Assert.isTrue(enumerator.moveNext())
    val runtimeInit = enumerator.current()
    Assert.equal(SymbolKind.Method, runtimeInit.kind)
    Assert.stringEqual("$runtimeInit", runtimeInit.name)

    program
  }

  def enumNonBuiltinSymbols(
      compilation: Compilation
  ): ChainEnumerator[Symbol] = {
    val enumerator = new ChainEnumerator(compilation.getSymbols())

    assertSymbol(enumerator, SymbolKind.Class, "any")
    assertSymbol(enumerator, SymbolKind.Class, "int")
    assertSymbol(enumerator, SymbolKind.Class, "string")
    assertSymbol(enumerator, SymbolKind.Field, "length")
    assertSymbol(enumerator, SymbolKind.Class, "bool")
    assertSymbol(enumerator, SymbolKind.Class, "char")
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

    enumerator
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

  def assertBinaryExpr(expression: Expression): Expression.Binary = {
    expression match {
      case expr: Expression.Binary => expr
      case _                       => failed("expected binary expression")
    }
  }

  def assertMemberAccess(
      expression: Expression
  ): Expression.MemberAccess = {
    expression match {
      case expr: Expression.MemberAccess => expr
      case _ => failed("expected member access expression")
    }
  }

  def assertMatch(
      expression: Expression
  ): Expression.Match = {
    expression match {
      case expr: Expression.Match => expr
      case _                      => failed("expected match expression")
    }
  }

  def assertLiteralPattern(
      expression: PatternSyntax
  ): PatternSyntax.Literal = {
    expression match {
      case expr: PatternSyntax.Literal => expr
      case _                           => failed("expected literal pattern")
    }
  }

  def assertAssignmentExpr(
      expression: Expression
  ): Expression.Assignment = {
    expression match {
      case expr: Expression.Assignment => expr
      case _ => failed("expected assignment expression")
    }
  }

  def assertIfExpr(
      expression: Expression
  ): Expression.If = {
    expression match {
      case expr: Expression.If => expr
      case _                   => failed("expected assignment expression")
    }
  }

  def assertCallExpr(
      expression: Expression
  ): Expression.Call = {
    expression match {
      case expr: Expression.Call => expr
      case _                     => failed("expected call expression")
    }
  }

  def assertWhileExpr(
      expression: Expression
  ): Expression.While = {
    expression match {
      case expr: Expression.While => expr
      case _                      => failed("expected while expression")
    }
  }

  def assertGroupExpr(expr: Expression): Expression.Group = {
    expr match {
      case expr: Expression.Group => expr
      case _                      => failed("expected group expression")
    }
  }

  def assertBlockExpr(expr: Expression): Expression.Block = {
    expr match {
      case expr: Expression.Block => expr
      case _                      => failed("expected block expression")
    }
  }

  def assertUnaryExpr(expr: Expression): Expression.Unary = {
    expr match {
      case expr: Expression.Unary => expr
      case _                      => failed("expected unary expression")
    }
  }

  def assertSymbol(
      enumerator: ChainEnumerator[Symbol],
      kind: SymbolKind,
      name: string
  ): Symbol = {
    Assert.isTrue(enumerator.moveNext())
    val symbol = enumerator.current()
    Assert.equal(name, symbol.name)
    Assert.equal(kind, symbol.kind)
    symbol
  }

  def assertSymbolType(comp: Compilation, symbol: Symbol, typ: string): unit = {
    val symbolType = comp.binder.tryGetSymbolType(symbol)
    symbolType match {
      case Option.Some(value) =>
        Assert.stringEqual(typ, value.toString())
      case Option.None =>
        failed(
          "Expected symbol " + symbol.name + " to have a type " + typ
            .toString() + " but got none"
        )
    }
  }

  def assertMainSymbol(enumerator: ChainEnumerator[Symbol]): Symbol =
    assertSymbol(enumerator, SymbolKind.Method, "main")

  def assertNoSymbols(
      enumerator: ChainEnumerator[Symbol]
  ): unit = {
    if (enumerator.moveNext()) {
      val symbol = enumerator.current()
      failed("Unexpected symbol: " + symbol.kind + " " + symbol.name)
      Assert.isFalse(true)
    }
  }

  def mkGroupExpr(text: string): Expression.Group = {
    val expr = mkSyntaxTreeExpr(text)
    assertGroupExpr(expr)
  }

  def mkSyntaxTreeExpr(text: string): Expression = {
    mkSyntaxTreeStatement(text) match {
      case StatementSyntax.ExpressionStatement(expr) => expr
      case _ => failed("Expected expression")
    }
  }

  def mkBlockExpr(text: string): Expression.Block = {
    val expr = mkSyntaxTreeExpr(text)
    assertBlockExpr(expr)
  }

  def mkSyntaxTreeStatement(text: string): StatementSyntax = {
    mkMember(text) match {
      case MemberSyntax.GlobalStatementSyntax(statement) => statement
      case _ => failed("Expected statement")
    }
  }

  def mkFunctionMember(text: string): MemberSyntax.FunctionDeclarationSyntax = {
    mkMember(text) match {
      case member: MemberSyntax.FunctionDeclarationSyntax => member
      case _ => failed("Expected function declaration")
    }
  }

  def mkMember(text: string): MemberSyntax = {
    val tree = mkSyntaxTree(text)
    Assert.single(tree.root.members)
  }

  def assertNumberToken(
      expected: int,
      token: SyntaxToken
  ): unit = {
    Assert.equal(SyntaxKind.NumberToken, token.kind)
    token.value match {
      case SyntaxTokenValue.Number(value) =>
        Assert.intEqual(expected, value)
      case _ =>
        failed("Expected number token, got: " + token.value)
    }
  }

  def assertNumberExpr(expected: int, expression: Expression): unit = {
    expression match {
      case Expression.Literal(_, SyntaxTokenValue.Number(n)) =>
        Assert.intEqual(expected, n)
      case _ => failed("Expected number expression")
    }
  }

  def assertBoolExpr(expected: bool, expression: Expression): unit = {
    expression match {
      case Expression.Literal(_, SyntaxTokenValue.Boolean(b)) =>
        Assert.boolEqual(expected, b)
      case _ => failed("Expected boolean expression")
    }
  }

  def assertTrueExpr(expression: Expression): unit = {
    expression match {
      case Expression.Literal(_, SyntaxTokenValue.Boolean(true)) =>
      case _ => failed("Expected true expression")
    }
  }

  def assertFalseExpr(expression: Expression): unit = {
    expression match {
      case Expression.Literal(_, SyntaxTokenValue.Boolean(false)) =>
      case _ => failed("Expected false expression")
    }
  }

  def assertIdentifierExpr(expected: string, expression: Expression): unit = {
    expression match {
      case Expression.IdentifierName(
            SimpleNameSyntax.IdentifierNameSyntax(token)
          ) =>
        Assert.stringEqual(expected, token.text)
      case _ => failed("Expected identifier expression")
    }
  }

  def assertSimpleNameIdentifierExpr(
      expected: string,
      name: SimpleNameSyntax
  ): unit = {
    name match {
      case SimpleNameSyntax.IdentifierNameSyntax(token) =>
        Assert.stringEqual(expected, token.text)
      case _ => failed("Expected identifier expression")
    }
  }

  def assertGenericIdentifierExpr(
      expression: Expression
  ): SimpleNameSyntax.GenericNameSyntax = {
    expression match {
      case Expression.IdentifierName(
            a: SimpleNameSyntax.GenericNameSyntax
          ) =>
        a
      case _ => failed("Expected generic identifier expression")
    }
  }

  def assertTokenKind(expected: int, token: SyntaxToken): unit =
    if (expected == token.kind) ()
    else
      failed(
        "expected " + SyntaxFacts.getKindName(expected) + ", got " + SyntaxFacts
          .getKindName(token.kind)
      )

  def assertTokenText(expected: string, token: SyntaxToken): unit =
    Assert.stringEqual(expected, token.text)

  def assertVariableDeclaration(
      statement: StatementSyntax
  ): StatementSyntax.VariableDeclarationStatement = {
    statement match {
      case stmt: StatementSyntax.VariableDeclarationStatement => stmt
      case _ => failed("Expected variable declaration")
    }
  }

  def assertName(expected: string, actual: NameSyntax): unit = {
    val sb = IndentedStringBuilder(false)
    val printer = new AstPrinter(false, sb)
    printer.printName(actual)
    Assert.stringEqual(expected, Trim.both(printer.toString()))
  }
}
