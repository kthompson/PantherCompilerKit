import panther.{assert => panthAssert, *}
import utest._

object TestHelpers {

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
      throw new AssertionError("Compilation failed", Seq())
    }
    comp
  }

  def mkSyntaxTree(text: string): SyntaxTree =
    MakeSyntaxTree.parseContent(text)

  def mkBinaryExpr(text: string): Expression.BinaryExpression = {
    val expression = mkSyntaxTreeExpr(text)
    assertBinaryExpr(expression)
  }

  def mkMemberAccessExpr(text: string): Expression.MemberAccessExpression = {
    val expression = mkSyntaxTreeExpr(text)
    assertMemberAccess(expression)
  }

  def mkMatchExpr(text: string): Expression.MatchExpression = {
    val expression = mkSyntaxTreeExpr(text)
    assertMatch(expression)
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

  def mkGroupExpr(text: string): Expression.GroupExpression = {
    val expr = mkSyntaxTreeExpr(text)
    assertGroupExpr(expr)
  }

  def mkBlockExpr(text: string): Expression.BlockExpression = {
    val expr = mkSyntaxTreeExpr(text)
    assertBlockExpr(expr)
  }

  def mkIsExpr(text: string): Expression.IsExpression = {
    val expression = mkSyntaxTreeExpr(text)
    assertIsExpr(expression)
  }

  def mkSyntaxTreeExpr(text: string): Expression = {
    mkSyntaxTreeStatement(text) match {
      case StatementSyntax.ExpressionStatement(expr) => expr
      case _ => throw new AssertionError("Expected expression", Seq())
    }
  }

  def mkSyntaxTreeStatement(text: string): StatementSyntax = {
    mkMember(text) match {
      case MemberSyntax.GlobalStatementSyntax(statement) => statement
      case _ => throw new AssertionError("Expected statement", Seq())
    }
  }

  def mkFunctionMember(text: string): MemberSyntax.FunctionDeclarationSyntax = {
    mkMember(text) match {
      case member: MemberSyntax.FunctionDeclarationSyntax => member
      case _ => throw new AssertionError("Expected function declaration", Seq())
    }
  }

  def mkMember(text: string): MemberSyntax = {
    val tree = mkSyntaxTree(text)
    assertSingle(tree.root.members)
  }

  def assertSingle[T](items: List[T]): T = {
    items match {
      case List.Nil =>
        throw new AssertionError("expected one item, found zero", Seq())
      case List.Cons(head, tail) =>
        if (tail.isEmpty) head
        else
          throw new AssertionError(
            "expected one item, found " + items.length,
            Seq()
          )
    }
  }

  def assertSingle[T](items: Array[T]): T = {
    if (items.length == 0)
      throw new AssertionError("expected one item, found zero", Seq())
    else if (items.length > 1)
      throw new AssertionError(
        "expected one item, found " + items.length,
        Seq()
      )
    else items(0)
  }

  def assertIndex[T](i: int, list: List[T]): T = {
    if (i < 0 || i >= list.length)
      throw new AssertionError("index out of bounds: " + i, Seq())
    list.getUnsafe(i)
  }

  def assertBinaryExpr(expression: Expression): Expression.BinaryExpression = {
    expression match {
      case expr: Expression.BinaryExpression => expr
      case _ => throw new AssertionError("expected binary expression", Seq())
    }
  }

  def assertMemberAccess(
      expression: Expression
  ): Expression.MemberAccessExpression = {
    expression match {
      case expr: Expression.MemberAccessExpression => expr
      case _ =>
        throw new AssertionError("expected member access expression", Seq())
    }
  }

  def assertMatch(
      expression: Expression
  ): Expression.MatchExpression = {
    expression match {
      case expr: Expression.MatchExpression => expr
      case _ => throw new AssertionError("expected match expression", Seq())
    }
  }

  def assertLiteralPattern(
      expression: PatternSyntax
  ): PatternSyntax.Literal = {
    expression match {
      case expr: PatternSyntax.Literal => expr
      case _ => throw new AssertionError("expected literal pattern", Seq())
    }
  }

  def assertAssignmentExpr(
      expression: Expression
  ): Expression.AssignmentExpression = {
    expression match {
      case expr: Expression.AssignmentExpression => expr
      case _ =>
        throw new AssertionError("expected assignment expression", Seq())
    }
  }

  def assertIfExpr(
      expression: Expression
  ): Expression.If = {
    expression match {
      case expr: Expression.If => expr
      case _ => throw new AssertionError("expected if expression", Seq())
    }
  }

  def assertCallExpr(
      expression: Expression
  ): Expression.CallExpression = {
    expression match {
      case expr: Expression.CallExpression => expr
      case _ => throw new AssertionError("expected call expression", Seq())
    }
  }

  def assertWhileExpr(
      expression: Expression
  ): Expression.WhileExpression = {
    expression match {
      case expr: Expression.WhileExpression => expr
      case _ => throw new AssertionError("expected while expression", Seq())
    }
  }

  def assertGroupExpr(expr: Expression): Expression.GroupExpression = {
    expr match {
      case expr: Expression.GroupExpression => expr
      case _ => throw new AssertionError("expected group expression", Seq())
    }
  }

  def assertBlockExpr(expr: Expression): Expression.BlockExpression = {
    expr match {
      case expr: Expression.BlockExpression => expr
      case _ => throw new AssertionError("expected block expression", Seq())
    }
  }

  def assertUnaryExpr(expr: Expression): Expression.UnaryExpression = {
    expr match {
      case expr: Expression.UnaryExpression => expr
      case _ => throw new AssertionError("expected unary expression", Seq())
    }
  }

  def assertIsExpr(expr: Expression): Expression.IsExpression = {
    expr match {
      case expr: Expression.IsExpression => expr
      case _ => throw new AssertionError("expected is expression", Seq())
    }
  }

  def assertVariableDeclaration(
      statement: StatementSyntax
  ): StatementSyntax.VariableDeclarationStatement = {
    statement match {
      case stmt: StatementSyntax.VariableDeclarationStatement => stmt
      case _ => throw new AssertionError("Expected variable declaration", Seq())
    }
  }

  def assertNumberToken(
      expected: int,
      token: SyntaxToken
  ): Unit = {
    assert(token.kind == SyntaxKind.NumberToken)
    token.value match {
      case SyntaxTokenValue.Number(value) =>
        assert(value == expected)
      case _ =>
        throw new AssertionError(
          "Expected number token, got: " + token.value,
          Seq()
        )
    }
  }

  def assertNumberExpr(expected: int, expression: Expression): Unit = {
    expression match {
      case Expression.LiteralExpression(_, SyntaxTokenValue.Number(n)) =>
        assert(n == expected)
      case _ => throw new AssertionError("Expected number expression", Seq())
    }
  }

  def assertBoolExpr(expected: bool, expression: Expression): Unit = {
    expression match {
      case Expression.LiteralExpression(_, SyntaxTokenValue.Boolean(b)) =>
        assert(b == expected)
      case _ => throw new AssertionError("Expected boolean expression", Seq())
    }
  }

  def assertTrueExpr(expression: Expression): Unit = {
    expression match {
      case Expression.LiteralExpression(_, SyntaxTokenValue.Boolean(true)) =>
      case _ => throw new AssertionError("Expected true expression", Seq())
    }
  }

  def assertFalseExpr(expression: Expression): Unit = {
    expression match {
      case Expression.LiteralExpression(_, SyntaxTokenValue.Boolean(false)) =>
      case _ => throw new AssertionError("Expected false expression", Seq())
    }
  }

  def assertIdentifierExpr(expected: string, expression: Expression): Unit = {
    expression match {
      case Expression.IdentifierName(
            SimpleNameSyntax.IdentifierNameSyntax(token)
          ) =>
        assert(token.text == expected)
      case _ =>
        throw new AssertionError("Expected identifier expression", Seq())
    }
  }

  def assertSimpleNameIdentifierExpr(
      expected: string,
      name: SimpleNameSyntax
  ): Unit = {
    name match {
      case SimpleNameSyntax.IdentifierNameSyntax(token) =>
        assert(token.text == expected)
      case _ =>
        throw new AssertionError("Expected identifier expression", Seq())
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
      case _ =>
        throw new AssertionError(
          "Expected generic identifier expression",
          Seq()
        )
    }
  }

  def assertTokenKind(expected: int, token: SyntaxToken): Unit =
    if (expected != token.kind)
      throw new AssertionError(
        "expected " + SyntaxFacts.getKindName(expected) + ", got " + SyntaxFacts
          .getKindName(token.kind),
        Seq()
      )

  def assertTokenText(expected: string, token: SyntaxToken): Unit =
    assert(token.text == expected)

  def assertName(expected: string, actual: NameSyntax): Unit = {
    val sb = IndentedStringBuilder(false)
    val printer = new AstPrinter(false, sb)
    printer.printName(actual)
    assert(Trim.both(printer.toString()) == expected)
  }

  def assertSome[T](option: Option[T]): T = option match {
    case Option.Some(value) => value
    case Option.None =>
      throw new AssertionError("expected Some, found None", Seq())
  }

  def assertNone[T](option: Option[T]): Unit = {
    if (option.isDefined())
      throw new AssertionError("expected None, found Some", Seq())
  }

  def assertEmpty[T](list: List[T]): Unit = {
    if (!list.isEmpty)
      throw new AssertionError(
        "expected empty list, found " + list.length + " items",
        Seq()
      )
  }

  def enumSymbols(compilation: Compilation): ChainEnumerator[Symbol] =
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
      throw new AssertionError(
        "Unexpected symbol: " + symbol.kind + " " + symbol.name,
        Seq()
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
    val symbols = enumNonBuiltinSymbols(comp)

    assertProgramSymbol(symbols)

    val x = assertSymbol(symbols, SymbolKind.Field, "x")
    assertSymbolType(comp, x, expectedType)

    assertMainSymbol(symbols)
    //    assertNoSymbols(symbols)
  }

  def assertSymbolType(comp: Compilation, symbol: Symbol, typ: string): Unit = {
    val symbolType = comp.binder.tryGetSymbolType(symbol)
    symbolType match {
      case Option.Some(value) =>
        assert(value.toString() == typ)
      case Option.None =>
        throw new AssertionError(
          "Expected symbol " + symbol.name + " to have a type " + typ
            + " but got none",
          Seq()
        )
    }
  }

  def assertAssignableTo(expression: string, assignableTo: string): Unit =
    mkCompilation("val typeTestSymbol: " + assignableTo + " = " + expression)

  def execValue(program: string): Value = {
    val compilation = mkCompilation(program)
    compilation.exec() match {
      case InterpretResult.OkValue(value) =>
        value
      case result =>
        throw new AssertionError("Expected exec result, got: " + result, Seq())
    }
  }

  def assertValueInt(value: Value, expected: int): Unit = {
    value match {
      case Value.Int(v) =>
        assert(v == expected)
      case _ =>
        throw new AssertionError(
          "Expected " + string(expected) + ", got: " + value,
          Seq()
        )
    }
  }

  def assertValueBool(value: Value, expected: bool): Unit = {
    value match {
      case Value.Bool(v) =>
        assert(v == expected)
      case _ =>
        throw new AssertionError(
          "Expected " + string(expected) + ", got: " + value,
          Seq()
        )
    }
  }

  def assertValueString(value: Value, expected: string): Unit = {
    value match {
      case Value.String(v) =>
        assert(v == expected)
      case _ =>
        throw new AssertionError("Expected string value, got: " + value, Seq())
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
