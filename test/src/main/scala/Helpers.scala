import panther._
import TestFramework._

object Helpers {

  def mkTokens(text: string): Array[SyntaxToken] = {
    val sourceFile = new SourceFile(text, "test.pn")
    val diagnostics = new DiagnosticBag()
    val lexer = new Lexer(sourceFile, diagnostics)
    MakeTokenList.create(lexer)
  }

  def mkCompilation(text: string): Compilation = {
    val tree = MakeSyntaxTree.parseContent(text)
    val comp = MakeCompilation.create(ListModule.one(tree))
    if (comp.diagnostics.count() > 0) {
      comp.diagnostics.printDiagnostics()
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
    assertNoSymbols(symbols)
  }

  def enumSymbols(compilation: Compilation): ChainEnumerator[Symbol] =
    new ChainEnumerator(compilation.getSymbols())

  def assertProgramSymbol(
      enumerator: ChainEnumerator[Symbol]
  ): Symbol = {
    Assert.isTrue(enumerator.moveNext())
    val symbol = enumerator.current()
    Assert.equal(SymbolKind.Object, symbol.kind)

    Assert.assert(
      symbol.name == "$Program" || symbol.name == "Program",
      "expected 'Program' or '$Program', got: " + symbol.name
    )
    symbol
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
    assertSymbol(enumerator, SymbolKind.Constructor, ".ctor")
    assertSymbol(enumerator, SymbolKind.Parameter, "size")
    //    assertSymbol(enumerator, SymbolKind.Object, "predef")
    assertSymbol(enumerator, SymbolKind.Method, "println")
    assertSymbol(enumerator, SymbolKind.Parameter, "message")
    assertSymbol(enumerator, SymbolKind.Method, "print")
    assertSymbol(enumerator, SymbolKind.Parameter, "message")

    enumerator
  }

  def mkSyntaxTree(text: string): SyntaxTree =
    MakeSyntaxTree.parseContent(text)

  def mkBinaryExpr(text: string): Expression.BinaryExpression = {
    val expression = mkSyntaxTreeExpr(text)
    assertBinaryExpr(expression)
  }

  def mkAssignmentExpr(text: string): Expression.AssignmentExpression = {
    val expression = mkSyntaxTreeExpr(text)
    assertAssignmentExpr(expression)
  }

  def mkCallExpr(text: string): Expression.CallExpression = {
    val expression = mkSyntaxTreeExpr(text)
    assertCallExpr(expression)
  }

  def mkIfExpr(text: string): Expression.If = {
    val expression = mkSyntaxTreeExpr(text)
    assertIfExpr(expression)
  }

  def mkWhileExpr(text: string): Expression.WhileExpression = {
    val expression = mkSyntaxTreeExpr(text)
    assertWhileExpr(expression)
  }

  def mkUnaryExpr(text: string): Expression.UnaryExpression = {
    val expr = mkSyntaxTreeExpr(text)
    assertUnaryExpr(expr)
  }

  def assertBinaryExpr(expression: Expression): Expression.BinaryExpression = {
    expression match {
      case expr: Expression.BinaryExpression => expr
      case _ => failed("expected binary expression")
    }
  }

  def assertAssignmentExpr(
      expression: Expression
  ): Expression.AssignmentExpression = {
    expression match {
      case expr: Expression.AssignmentExpression => expr
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
  ): Expression.CallExpression = {
    expression match {
      case expr: Expression.CallExpression => expr
      case _                               => failed("expected call expression")
    }
  }

  def assertWhileExpr(
      expression: Expression
  ): Expression.WhileExpression = {
    expression match {
      case expr: Expression.WhileExpression => expr
      case _ => failed("expected while expression")
    }
  }

  def assertGroupExpr(expr: Expression): Expression.GroupExpression = {
    expr match {
      case expr: Expression.GroupExpression => expr
      case _ => failed("expected group expression")
    }
  }

  def assertBlockExpr(expr: Expression): Expression.BlockExpression = {
    expr match {
      case expr: Expression.BlockExpression => expr
      case _ => failed("expected block expression")
    }
  }

  def assertUnaryExpr(expr: Expression): Expression.UnaryExpression = {
    expr match {
      case expr: Expression.UnaryExpression => expr
      case _ => failed("expected unary expression")
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
    val symbolType = comp.binder.getSymbolType(symbol)
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

  def mkGroupExpr(text: string): Expression.GroupExpression = {
    val expr = mkSyntaxTreeExpr(text)
    assertGroupExpr(expr)
  }

  def mkSyntaxTreeExpr(text: string): Expression = {
    mkSyntaxTreeStatement(text) match {
      case StatementSyntax.ExpressionStatement(expr) => expr
      case _ => failed("Expected expression")
    }
  }

  def mkBlockExpr(text: string): Expression.BlockExpression = {
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

  def assertNumberExpr(expected: int, expression: Expression): unit = {
    expression match {
      case Expression.LiteralExpression(_, SyntaxTokenValue.Number(n)) =>
        Assert.intEqual(expected, n)
      case _ => failed("Expected number expression")
    }
  }

  def assertBoolExpr(expected: bool, expression: Expression): unit = {
    expression match {
      case Expression.LiteralExpression(_, SyntaxTokenValue.Boolean(b)) =>
        Assert.boolEqual(expected, b)
      case _ => failed("Expected boolean expression")
    }
  }

  def assertTrueExpr(expression: Expression): unit = {
    expression match {
      case Expression.LiteralExpression(_, SyntaxTokenValue.Boolean(true)) =>
      case _ => failed("Expected true expression")
    }
  }

  def assertFalseExpr(expression: Expression): unit = {
    expression match {
      case Expression.LiteralExpression(_, SyntaxTokenValue.Boolean(false)) =>
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
    val printer = new AstPrinter(false, false)
    printer.printName(actual)
    Assert.stringEqual(expected, Trim.both(printer.toString()))
  }
}
