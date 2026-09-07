import panther.{assert => panthAssert, *}

object TestHelpers {

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
      throw new AssertionError("Compilation failed")
    }
    comp
  }

  /** Compile text that is expected to report diagnostics. Unlike mkCompilation
    * this does not throw, so a caller can inspect what came back — and a
    * compiler that panics instead of reporting fails the test.
    */
  def mkFailingCompilation(text: string): Compilation =
    MakeCompilation.create(
      ListModule.one(
        MakeSyntaxTree.parseContent(text, CompilerSettingsFactory.default)
      ),
      CompilerSettingsFactory.default
    )

  def diagnosticMessages(comp: Compilation): Seq[String] = {
    def walk(diagnostics: Diagnostics): Seq[String] =
      diagnostics match {
        case Diagnostics.Empty => Seq.empty
        case Diagnostics.Node(left, head, right) =>
          walk(left) ++ Seq(head.message) ++ walk(right)
      }
    walk(comp.diagnostics)
  }

  def mkSyntaxTree(text: string): SyntaxTree =
    MakeSyntaxTree.parseContent(text, CompilerSettingsFactory.default)

  /** Transpile Scala source and hand back the Panther text.
    *
    * The `.scala` file name is what puts the parser and the transpiler in Scala
    * mode, and the output is built in memory rather than written, so a test
    * needs no directory.
    */
  def mkTranspiled(text: string): String = {
    val tree = MakeSyntaxTree.parseSourceFile(
      new SourceFile(text, "test.scala"),
      CompilerSettingsFactory.default
    )
    val context = new TranspilerContext(new StringBuilder())
    new Transpiler(List.Nil, "").transpileRoot(tree.root, context)
    context.sb.toString()
  }

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
    mkSyntaxTreeStatement(text) match {
      case StatementSyntax.ExpressionStatement(expr) => expr
      case _ => throw new AssertionError("Expected expression")
    }
  }

  def mkSyntaxTreeStatement(text: string): StatementSyntax = {
    mkMember(text) match {
      case MemberSyntax.GlobalStatementSyntax(statement) => statement
      case _ => throw new AssertionError("Expected statement")
    }
  }

  def mkFunctionMember(text: string): MemberSyntax.FunctionDeclarationSyntax = {
    mkMember(text) match {
      case member: MemberSyntax.FunctionDeclarationSyntax => member
      case _ => throw new AssertionError("Expected function declaration")
    }
  }

  def mkClassMember(text: string): MemberSyntax.ClassDeclarationSyntax = {
    mkMember(text) match {
      case member: MemberSyntax.ClassDeclarationSyntax => member
      case _ => throw new AssertionError("Expected class declaration")
    }
  }

  def mkEnumMember(text: string): MemberSyntax.EnumDeclarationSyntax = {
    mkMember(text) match {
      case member: MemberSyntax.EnumDeclarationSyntax => member
      case _ => throw new AssertionError("Expected enum declaration")
    }
  }

  /** The traits a `derive` attribute names, in source order. */
  def derivedNames(attribute: DeriveAttributeSyntax): Seq[String] = {
    def walk(traits: List[DerivedTraitSyntax]): Seq[String] =
      traits match {
        case List.Nil              => Seq.empty
        case List.Cons(head, tail) => head.name.text +: walk(tail)
      }
    walk(attribute.traits)
  }

  def assertFunctionMember(
      member: MemberSyntax
  ): MemberSyntax.FunctionDeclarationSyntax = {
    member match {
      case member: MemberSyntax.FunctionDeclarationSyntax => member
      case _ => throw new AssertionError("Expected function declaration")
    }
  }

  /** Diagnostics the parser reported, without binding. A parse-time report has
    * nowhere else to surface: `mkFailingCompilation` would bind first and bury
    * it under whatever the malformed declaration then fails to type.
    */
  def treeDiagnosticMessages(tree: SyntaxTree): Seq[String] = {
    def walk(diagnostics: Diagnostics): Seq[String] =
      diagnostics match {
        case Diagnostics.Empty => Seq.empty
        case Diagnostics.Node(left, head, right) =>
          walk(left) ++ Seq(head.message) ++ walk(right)
      }
    walk(tree.diagnostics)
  }

  /** The heads of every given the source registered, in source order.
    * `binder.givens` accumulates in reverse, so this flips it back.
    *
    * The prelude's own givens are left out; `preludeGivenHeads` has them.
    */
  def givenHeads(comp: Compilation): Seq[String] =
    allGivenHeads(comp).filterNot(_._2).map(_._1)

  def preludeGivenHeads(comp: Compilation): Seq[String] =
    allGivenHeads(comp).filter(_._2).map(_._1)

  /** The goal each interned evidence record proves, in interning order — which
    * is the order the records are laid out and built in.
    */
  def evidenceRecordGoals(comp: Compilation): Seq[String] = {
    def walk(records: List[BoundEvidenceRecord]): Seq[String] =
      records match {
        case List.Nil              => Seq.empty
        case List.Cons(head, tail) => head.goal.toString() +: walk(tail)
      }
    walk(comp.binder.evidenceRecordsInOrder())
  }

  private def allGivenHeads(comp: Compilation): Seq[(String, Boolean)] = {
    def walk(givens: List[BoundGiven]): Seq[(String, Boolean)] =
      givens match {
        case List.Nil => Seq.empty
        case List.Cons(head, tail) =>
          walk(tail) :+ (head.head.toString(), isPreludeGiven(head.symbol))
      }
    walk(comp.binder.givens)
  }

  /** `kind:name` for each member of `symbol`, in definition order — which is
    * the order the emitter turns into argument slots.
    */
  def memberSignature(symbol: Symbol): Seq[String] = {
    def walk(members: List[Symbol]): Seq[String] =
      members match {
        case List.Nil => Seq.empty
        case List.Cons(head, tail) =>
          (head.kind.toString + ":" + head.name) +: walk(tail)
      }
    walk(symbol.members())
  }

  def mkGivenMember(text: string): MemberSyntax.GivenDeclarationSyntax = {
    mkMember(text) match {
      case member: MemberSyntax.GivenDeclarationSyntax => member
      case _ => throw new AssertionError("Expected given declaration")
    }
  }

  def mkTraitMember(text: string): MemberSyntax.TraitDeclarationSyntax = {
    mkMember(text) match {
      case member: MemberSyntax.TraitDeclarationSyntax => member
      case _ => throw new AssertionError("Expected trait declaration")
    }
  }

  def mkMember(text: string): MemberSyntax = {
    val tree = mkSyntaxTree(text)
    assertSingle(tree.root.members)
  }

  def assertSingle[T](items: List[T]): T = {
    items match {
      case List.Nil =>
        throw new AssertionError("expected one item, found zero")
      case List.Cons(head, tail) =>
        if (tail.isEmpty) head
        else
          throw new AssertionError(
            "expected one item, found " + items.length
          )
    }
  }

  def assertSingle[T](items: Array[T]): T = {
    if (items.length == 0)
      throw new AssertionError("expected one item, found zero")
    else if (items.length > 1)
      throw new AssertionError(
        "expected one item, found " + items.length
      )
    else items(0)
  }

  def assertIndex[T](i: int, list: List[T]): T = {
    if (i < 0 || i >= list.length)
      throw new AssertionError("index out of bounds: " + i)
    list.getUnsafe(i)
  }

  def assertBinaryExpr(expression: Expression): Expression.Binary = {
    expression match {
      case expr: Expression.Binary => expr
      case _ => throw new AssertionError("expected binary expression")
    }
  }

  def assertMemberAccess(
      expression: Expression
  ): Expression.MemberAccess = {
    expression match {
      case expr: Expression.MemberAccess => expr
      case _ =>
        throw new AssertionError("expected member access expression")
    }
  }

  def assertMatch(
      expression: Expression
  ): Expression.Match = {
    expression match {
      case expr: Expression.Match => expr
      case _ => throw new AssertionError("expected match expression")
    }
  }

  def assertLiteralPattern(
      expression: PatternSyntax
  ): PatternSyntax.Literal = {
    expression match {
      case expr: PatternSyntax.Literal => expr
      case _ => throw new AssertionError("expected literal pattern")
    }
  }

  def assertAssignmentExpr(
      expression: Expression
  ): Expression.Assignment = {
    expression match {
      case expr: Expression.Assignment => expr
      case _ =>
        throw new AssertionError("expected assignment expression")
    }
  }

  def assertIfExpr(
      expression: Expression
  ): Expression.If = {
    expression match {
      case expr: Expression.If => expr
      case _ => throw new AssertionError("expected if expression")
    }
  }

  def assertCallExpr(
      expression: Expression
  ): Expression.Call = {
    expression match {
      case expr: Expression.Call => expr
      case _ => throw new AssertionError("expected call expression")
    }
  }

  def assertWhileExpr(
      expression: Expression
  ): Expression.While = {
    expression match {
      case expr: Expression.While => expr
      case _ => throw new AssertionError("expected while expression")
    }
  }

  def assertGroupExpr(expr: Expression): Expression.Group = {
    expr match {
      case expr: Expression.Group => expr
      case _ => throw new AssertionError("expected group expression")
    }
  }

  def assertBlockExpr(expr: Expression): Expression.Block = {
    expr match {
      case expr: Expression.Block => expr
      case _ => throw new AssertionError("expected block expression")
    }
  }

  def assertUnaryExpr(expr: Expression): Expression.Unary = {
    expr match {
      case expr: Expression.Unary => expr
      case _ => throw new AssertionError("expected unary expression")
    }
  }

  def assertIsExpr(expr: Expression): Expression.Is = {
    expr match {
      case expr: Expression.Is => expr
      case _ => throw new AssertionError("expected is expression")
    }
  }

  def assertVariableDeclaration(
      statement: StatementSyntax
  ): StatementSyntax.VariableDeclarationStatement = {
    statement match {
      case stmt: StatementSyntax.VariableDeclarationStatement => stmt
      case _ => throw new AssertionError("Expected variable declaration")
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
        throw new AssertionError("Expected number token, got: " + token.value)
    }
  }

  def assertNumberExpr(expected: int, expression: Expression): Unit = {
    expression match {
      case Expression.Literal(_, SyntaxTokenValue.Number(n)) =>
        assert(n == expected)
      case _ => throw new AssertionError("Expected number expression")
    }
  }

  def assertBoolExpr(expected: bool, expression: Expression): Unit = {
    expression match {
      case Expression.Literal(_, SyntaxTokenValue.Boolean(b)) =>
        assert(b == expected)
      case _ => throw new AssertionError("Expected boolean expression")
    }
  }

  def assertTrueExpr(expression: Expression): Unit = {
    expression match {
      case Expression.Literal(_, SyntaxTokenValue.Boolean(true)) =>
      case _ => throw new AssertionError("Expected true expression")
    }
  }

  def assertFalseExpr(expression: Expression): Unit = {
    expression match {
      case Expression.Literal(_, SyntaxTokenValue.Boolean(false)) =>
      case _ => throw new AssertionError("Expected false expression")
    }
  }

  def assertIdentifierExpr(expected: string, expression: Expression): Unit = {
    expression match {
      case Expression.IdentifierName(
            SimpleNameSyntax.IdentifierNameSyntax(token)
          ) =>
        assert(token.text == expected)
      case _ =>
        throw new AssertionError("Expected identifier expression")
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
        throw new AssertionError("Expected identifier expression")
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
        throw new AssertionError("Expected generic identifier expression")
    }
  }

  def assertTokenKind(expected: int, token: SyntaxToken): Unit =
    if (expected != token.kind)
      throw new AssertionError(
        "expected " + SyntaxFacts.getKindName(expected) + ", got " + SyntaxFacts
          .getKindName(token.kind)
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
      throw new AssertionError("expected Some, found None")
  }

  def assertNone[T](option: Option[T]): Unit = {
    if (option.isDefined())
      throw new AssertionError("expected None, found Some")
  }

  def assertEmpty[T](list: List[T]): Unit = {
    if (!list.isEmpty)
      throw new AssertionError(
        "expected empty list, found " + list.length + " items"
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

  /** The names the prelude defines at the root. */
  private val preludeRootNames: Set[String] = Set(
    "any",
    "int",
    "string",
    "bool",
    "char",
    "unit",
    "Array",
    "println",
    "print",
    "panic",
    "exit",
    "assert",
    "mod",
    "File",
    "Path",
    "Eq",
    "Ord",
    "Show"
  )

  /** A prelude given, and — one level down, on `$Program` — the static field
    * holding its evidence record. Both are named after what they prove.
    */
  private def isPreludeGiven(symbol: Symbol): Boolean =
    symbol.name.startsWith("$given$Eq$") ||
      symbol.name.startsWith("$given$Ord$") ||
      symbol.name.startsWith("$given$Show$")

  /** A static field the binder synthesizes on `$Program`: an evidence record
    * or an enum case's singleton. Both are placement details rather than
    * anything the source declared.
    */
  private def isSynthesizedField(symbol: Symbol): Boolean =
    symbol.name.startsWith("$case$")

  /** The symbol chain with everything the prelude and the binder put there
    * removed.
    *
    * Skipping it rather than asserting past it is what keeps a test about
    * `val x = 12` from having to be updated whenever the prelude gains a
    * given. The prelude has its own tests for what it defines.
    *
    * The root's own members are matched by name; below the root only the
    * synthesized fields on `$Program` are, so a source that declares a method
    * named `mod` still shows up.
    */
  def enumNonBuiltinSymbols(
      compilation: Compilation
  ): ChainEnumerator[Symbol] = {
    def chainOf(symbol: Symbol): Chain[Symbol] = {
      val children = chainOfList(symbol.members(), false)
      if (symbol.kind == SymbolKind.Block) children
      else children.prepend(symbol)
    }

    def chainOfList(members: List[Symbol], atRoot: Boolean): Chain[Symbol] =
      members match {
        case List.Nil => Chain.Empty()
        case List.Cons(head, tail) =>
          val skip =
            isPreludeGiven(head) || isSynthesizedField(head) ||
              (atRoot && preludeRootNames.contains(head.name))

          if (skip) chainOfList(tail, atRoot)
          else chainOf(head).concat(chainOfList(tail, atRoot))
      }

    new ChainEnumerator(chainOfList(compilation.root.members(), true))
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
    assert(enumerator.moveNext(), "expected " + kind + " " + name)
    val symbol = enumerator.current()
    assert(
      symbol.name == name && symbol.kind == kind,
      "expected " + kind + " " + name + ", found " + symbol.kind + " " +
        symbol.name
    )
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
        "Unexpected symbol: " + symbol.kind + " " + symbol.name
      )
    }
  }

  def assertInferExprTypeWithSetup(
      setup: string,
      expression: string,
      expectedType: string
  ): Unit = {
    val comp = mkCompilation(setup + "\n\nval typeTestSymbol = " + expression)
    val program = assertSome(comp.root.lookup("$Program"))
    val symbol = assertSome(program.lookup("typeTestSymbol"))
    assertSymbolType(comp, symbol, expectedType)
  }

  def assertCheckExprTypeWithSetup(
      setup: string,
      expression: string,
      expectedType: string
  ): Unit = {
    val comp = mkCompilation(
      setup + "\n\nval typeTestSymbol: " + expectedType + " = " + expression
    )
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

  def assertInferExprType(expression: string, expectedType: string): Unit = {
    val comp = mkCompilation("val x = " + expression)
    val symbols = enumNonBuiltinSymbols(comp)

    assertProgramSymbol(symbols)

    val x = assertSymbol(symbols, SymbolKind.Field, "x")
    assertSymbolType(comp, x, expectedType)

    assertMainSymbol(symbols)
    //    assertNoSymbols(symbols)
  }

  def assertCheckExprType(expression: string, expectedType: string): Unit = {
    val comp = mkCompilation("val x: " + expectedType + " = " + expression)
    val symbols = enumNonBuiltinSymbols(comp)

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
        throw new AssertionError(
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
        throw new AssertionError("Expected exec result, got: " + result)
    }
  }

  /** The raw result, for programs that do not end by evaluating to something —
    * `exit` and `panic` both stop the run without returning a value.
    */
  def execResult(program: string): InterpretResult =
    mkCompilation(program).exec()

  /** What the program wrote to stdout, which for `print` and `println` is the
    * whole of what they do.
    */
  def execOutput(program: string): String = {
    val buffer = new java.io.ByteArrayOutputStream()
    Console.withOut(buffer) {
      mkCompilation(program).exec()
    }
    buffer.toString("utf-8")
  }

  def assertValueInt(value: Value, expected: int): Unit = {
    value match {
      case Value.Int(v) =>
        assert(v == expected)
      case _ =>
        throw new AssertionError(
          "Expected " + string(expected) + ", got: " + value
        )
    }
  }

  def assertValueBool(value: Value, expected: bool): Unit = {
    value match {
      case Value.Bool(v) =>
        assert(v == expected)
      case _ =>
        throw new AssertionError(
          "Expected " + string(expected) + ", got: " + value
        )
    }
  }

  def assertValueString(value: Value, expected: string): Unit = {
    value match {
      case Value.String(v) =>
        assert(v == expected)
      case _ =>
        throw new AssertionError("Expected string value, got: " + value)
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
