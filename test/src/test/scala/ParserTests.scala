import panther.*
import TestHelpers.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ParserTests extends AnyFunSpec with Matchers {

  describe("Parser") {
    it("should parse binary expressions") {
      val expr = mkBinaryExpr("1 + 2")
      assertNumberExpr(1, expr.left)
      assertNumberExpr(2, expr.right)
      assertTokenKind(SyntaxKind.PlusToken, expr.operator)
    }

    it("should parse unary expressions") {
      val expr = mkUnaryExpr("-1")
      assertTokenKind(SyntaxKind.DashToken, expr.operator)
      assertNumberExpr(1, expr.expression)
    }

    it("should parse grouped expressions") {
      val expr = mkGroupExpr("(12)")
      assertTokenKind(SyntaxKind.OpenParenToken, expr.openParen)
      assertNumberExpr(12, expr.expression)
      assertTokenKind(SyntaxKind.CloseParenToken, expr.closeParen)
    }

    it("should handle operator precedence") {
      val expr = mkBinaryExpr("1 + 2 * 3")
      assertNumberExpr(1, expr.left)
      assertTokenKind(SyntaxKind.PlusToken, expr.operator)
      val right = assertBinaryExpr(expr.right)
      assertNumberExpr(2, right.left)
      assertTokenKind(SyntaxKind.StarToken, right.operator)
      assertNumberExpr(3, right.right)
    }

    it("should handle operator associativity") {
      val expr = mkBinaryExpr("1 - 2 - 3")
      val left = assertBinaryExpr(expr.left)
      assertNumberExpr(1, left.left)
      assertTokenKind(SyntaxKind.DashToken, left.operator)
      assertNumberExpr(2, left.right)
      assertTokenKind(SyntaxKind.DashToken, expr.operator)
      assertNumberExpr(3, expr.right)
    }

    it("should bind == tighter than ||") {
      // `a == b || c == d` is two comparisons joined by ||, not
      // `a == (b || c) == d`.
      val expr = mkBinaryExpr("a == b || c == d")
      assertTokenKind(SyntaxKind.PipePipeToken, expr.operator)

      val left = assertBinaryExpr(expr.left)
      assertIdentifierExpr("a", left.left)
      assertTokenKind(SyntaxKind.EqualsEqualsToken, left.operator)
      assertIdentifierExpr("b", left.right)

      val right = assertBinaryExpr(expr.right)
      assertIdentifierExpr("c", right.left)
      assertTokenKind(SyntaxKind.EqualsEqualsToken, right.operator)
      assertIdentifierExpr("d", right.right)
    }

    it("should bind == tighter than &&") {
      val expr = mkBinaryExpr("a == b && c == d")
      assertTokenKind(SyntaxKind.AmpersandAmpersandToken, expr.operator)
      assertBinaryExpr(expr.left)
      assertBinaryExpr(expr.right)
    }

    it("should bind relational operators tighter than ==") {
      // `a < b == c < d` compares the two relational results.
      val expr = mkBinaryExpr("a < b == c < d")
      assertTokenKind(SyntaxKind.EqualsEqualsToken, expr.operator)
      assertTokenKind(
        SyntaxKind.LessThanToken,
        assertBinaryExpr(expr.left).operator
      )
      assertTokenKind(
        SyntaxKind.LessThanToken,
        assertBinaryExpr(expr.right).operator
      )
    }

    it("should parse assignment expressions") {
      val expr = mkAssignmentExpr("a = 1")
      assertTokenKind(SyntaxKind.EqualsToken, expr.equals)
      assertNumberExpr(1, expr.right)
      assertIdentifierExpr("a", expr.left)
    }

    it("should parse call expressions with no arguments") {
      val expr = mkCallExpr("f()")
      assertIdentifierExpr("f", expr.name)
      assertNone(expr.genericArguments)
      assertTokenKind(SyntaxKind.OpenParenToken, expr.openParen)
      assertEmpty(expr.arguments.expressions)
      assertTokenKind(SyntaxKind.CloseParenToken, expr.closeParen)
    }

    it("should parse call expressions with one argument") {
      val expr = mkCallExpr("f(1)")
      assertIdentifierExpr("f", expr.name)
      assertNone(expr.genericArguments)
      assertTokenKind(SyntaxKind.OpenParenToken, expr.openParen)
      val arg = assertSingle(expr.arguments.expressions)
      assertNumberExpr(1, arg.expression)
      assertTokenKind(SyntaxKind.CloseParenToken, expr.closeParen)
    }

    it("should parse call expressions with multiple arguments") {
      val expr = mkCallExpr("f(1, 2)")
      assertIdentifierExpr("f", expr.name)
      val args = expr.arguments
      assertNumberExpr(1, assertIndex(0, args.expressions).expression)
      assertNumberExpr(2, assertIndex(1, args.expressions).expression)
    }

    it("should parse call expressions with generic arguments") {
      val expr = mkCallExpr("f[int](1)")
      val ident = assertGenericIdentifierExpr(expr.name)
      assertTokenText("f", ident.identifier)
      assertTokenKind(
        SyntaxKind.OpenBracketToken,
        ident.typeArgumentlist.lessThanToken
      )
      ident.typeArgumentlist.arguments.length shouldBe 1
      val genArg = ident.typeArgumentlist.arguments(0)
      assertName("int", genArg.name)
      assertNone(genArg.separator)
      assertTokenKind(
        SyntaxKind.CloseBracketToken,
        ident.typeArgumentlist.greaterThanToken
      )

      val args = expr.arguments
      assertNumberExpr(1, assertSingle(args.expressions).expression)
    }

    it("should parse if expressions") {
      val expr = mkIfExpr("if (true) 1 else 2")
      assertTokenKind(SyntaxKind.IfKeyword, expr.ifKeyword)
      assertTrueExpr(expr.condition)
      assertNumberExpr(1, expr.thenExpr)

      val elseExpr = assertSome(expr.elseExpr)
      assertTokenKind(SyntaxKind.ElseKeyword, elseExpr.elseKeyword)
      assertNumberExpr(2, elseExpr.expression)
    }

    it("should parse while expressions") {
      val expr = mkWhileExpr("while (true) 1")
      assertTokenKind(SyntaxKind.WhileKeyword, expr.whileKeyword)
      assertTokenKind(SyntaxKind.OpenParenToken, expr.openParen)
      assertTrueExpr(expr.condition)
      assertTokenKind(SyntaxKind.CloseParenToken, expr.closeParen)
      assertNumberExpr(1, expr.body)
    }

    it("should parse block expressions with expression") {
      val expr = mkBlockExpr("{ 1 }")
      assertTokenKind(SyntaxKind.OpenBraceToken, expr.openBrace)
      assertEmpty(expr.block.statements)
      val blockExpr = assertSome(expr.block.expression)
      assertNumberExpr(1, blockExpr)
      assertTokenKind(SyntaxKind.CloseBraceToken, expr.closeBrace)
    }

    it("should parse block expressions with statement") {
      val expr = mkBlockExpr(
        "{\n" +
          "  val a = 1\n" +
          "  a\n" +
          "}"
      )
      // {
      assertTokenKind(SyntaxKind.OpenBraceToken, expr.openBrace)

      // val a = 1
      val statements = expr.block.statements
      val statement = assertSingle(statements)
      val declaration = assertVariableDeclaration(statement)
      assertTokenKind(SyntaxKind.ValKeyword, declaration.valOrVarKeyword)
      assertTokenText("a", declaration.identifier)
      assertTokenKind(SyntaxKind.EqualsToken, declaration.equalToken)
      assertNumberExpr(1, declaration.expression)

      // a
      val blockExpr = assertSome(expr.block.expression)
      assertIdentifierExpr("a", blockExpr)

      // }
      assertTokenKind(SyntaxKind.CloseBraceToken, expr.closeBrace)
    }

    it("should parse dot on new line") {
      val expr = mkMemberAccessExpr("a\n.b")
      assertIdentifierExpr("a", expr.left)
      assertTokenKind(SyntaxKind.DotToken, expr.dotToken)
      assertSimpleNameIdentifierExpr("b", expr.right)
    }

    it("should parse simple match expressions") {
      val expr = mkMatchExpr("1 match { case 1 => 2 }")
      assertTokenKind(SyntaxKind.MatchKeyword, expr.matchKeyword)
      assertNumberExpr(1, expr.expression)

      val kase = expr.cases.head
      assertTokenKind(SyntaxKind.CaseKeyword, kase.caseKeyword)
      val pattern = assertLiteralPattern(kase.pattern)

      assertNumberToken(1, pattern.value)
      assertTokenKind(SyntaxKind.EqualsGreaterThanToken, kase.arrow)
      assertNumberExpr(2, assertSome(kase.block.expression))
    }

    it("should parse empty match case") {
      val expr = mkSyntaxTree(
        "enum Test {\n" +
          "  case Empty()\n" +
          "  case One(one: int)\n" +
          "}\n" +
          "\n" +
          "x match {\n" +
          "  case Test.Empty() => 2\n" +
          "  case Test.One(1) => 3\n" +
          "}"
      )
      expr.diagnostics.count() shouldBe 0
    }

    it("should parse is expressions") {
      val expr = mkIsExpr("x is int")
      assertIdentifierExpr("x", expr.expression)
      assertTokenKind(SyntaxKind.IsKeyword, expr.isKeyword)
      // TODO: add assertion for the type name
    }

    it("should parse functions with no parameters") {
      val fn = mkFunctionMember("def f() = { 1 }")
      assertTokenKind(SyntaxKind.DefKeyword, fn.defKeyword)
      assertTokenText("f", fn.identifier)
      assertNone(fn.genericParameters)
      assertTokenKind(SyntaxKind.OpenParenToken, fn.openParenToken)
      assertEmpty(fn.parameters)
      assertTokenKind(SyntaxKind.CloseParenToken, fn.closeParenToken)
      assertNone(fn.typeAnnotation)

      val body = assertSome(fn.body)
      assertTokenKind(SyntaxKind.EqualsToken, body.equalToken)
      val block = assertBlockExpr(body.expression)
      val expr = assertSome(block.block.expression)
      assertNumberExpr(1, expr)
    }

    it("should parse functions with parameters") {
      val fn = mkFunctionMember("def f(a: int, b: int) = { a + b }")
      assertTokenKind(SyntaxKind.DefKeyword, fn.defKeyword)
      assertTokenText("f", fn.identifier)
      assertNone(fn.genericParameters)
      assertTokenKind(SyntaxKind.OpenParenToken, fn.openParenToken)

      val parameters = fn.parameters
      val a = assertIndex(0, parameters)
      assertTokenText("a", a.identifier)
      assertTokenKind(SyntaxKind.ColonToken, a.typeAnnotation.colonToken)
      assertName("int", a.typeAnnotation.typ)

      val b = assertIndex(1, parameters)
      assertTokenText("b", b.identifier)
      assertTokenKind(SyntaxKind.ColonToken, b.typeAnnotation.colonToken)
      assertName("int", b.typeAnnotation.typ)

      assertTokenKind(SyntaxKind.CloseParenToken, fn.closeParenToken)
      assertNone(fn.typeAnnotation)

      val body = assertSome(fn.body)
      assertTokenKind(SyntaxKind.EqualsToken, body.equalToken)
      val block = assertBlockExpr(body.expression)
      val expr = assertSome(block.block.expression)
      val binary = assertBinaryExpr(expr)
      assertIdentifierExpr("a", binary.left)
      assertTokenKind(SyntaxKind.PlusToken, binary.operator)
      assertIdentifierExpr("b", binary.right)
    }

    it("should parse functions with return type") {
      val fn = mkFunctionMember("def f(): int = { 1 }")
      assertTokenKind(SyntaxKind.DefKeyword, fn.defKeyword)
      assertTokenText("f", fn.identifier)
      assertNone(fn.genericParameters)
      assertTokenKind(SyntaxKind.OpenParenToken, fn.openParenToken)
      assertEmpty(fn.parameters)
      assertTokenKind(SyntaxKind.CloseParenToken, fn.closeParenToken)

      val typeAnnotation = assertSome(fn.typeAnnotation)
      assertTokenKind(SyntaxKind.ColonToken, typeAnnotation.colonToken)
      assertName("int", typeAnnotation.typ)

      val body = assertSome(fn.body)
      assertTokenKind(SyntaxKind.EqualsToken, body.equalToken)
      val block = assertBlockExpr(body.expression)
      val expr = assertSome(block.block.expression)
      assertNumberExpr(1, expr)
    }

    it("should parse traits with no type parameters") {
      val decl = mkTraitMember("trait Show { def show(): string }")
      assertTokenKind(SyntaxKind.TraitKeyword, decl.traitKeyword)
      assertTokenText("Show", decl.identifier)
      assertNone(decl.genericParameters)
      assertTokenKind(SyntaxKind.OpenBraceToken, decl.template.openBrace)
      assertTokenKind(SyntaxKind.CloseBraceToken, decl.template.closeBrace)
    }

    it("should parse traits with type parameters") {
      val decl = mkTraitMember("trait Eq[T] { def equals(a: T, b: T): bool }")
      assertTokenText("Eq", decl.identifier)

      val generics = assertSome(decl.genericParameters)
      val typeParam = assertSingle(generics.parameters.items)
      assertTokenText("T", typeParam.identifier)
      assertNone(typeParam.variance)
    }

    it("should parse an unconditional given") {
      val decl = mkGivenMember(
        "given Eq[int] { def equals(a: int, b: int): bool = a == b }"
      )
      assertTokenKind(SyntaxKind.GivenKeyword, decl.givenKeyword)
      assertName("Eq[int]", decl.name)

      // nothing to bind, so no parameter list and no arrow
      assertNone(decl.genericParameters)
      assertNone(decl.arrowToken)
    }

    it("should parse a conditional given") {
      val decl = mkGivenMember(
        "given [T: Ord] => Ord[Box[T]] { def compare(a: Box[T], b: Box[T]): int = 0 }"
      )
      val generics = assertSome(decl.genericParameters)
      val typeParam = assertSingle(generics.parameters.items)
      assertTokenText("T", typeParam.identifier)
      assertName("Ord", assertSome(typeParam.bounds).name)

      assertTokenKind(
        SyntaxKind.EqualsGreaterThanToken,
        assertSome(decl.arrowToken)
      )
      assertName("Ord[Box[T]]", decl.name)
    }

    it("should parse a context bound on a function") {
      val fn = mkFunctionMember("def same[K: Eq](a: K, b: K): bool = true")
      val generics = assertSome(fn.genericParameters)
      val typeParam = assertSingle(generics.parameters.items)

      assertTokenText("K", typeParam.identifier)
      val bounds = assertSome(typeParam.bounds)
      assertTokenKind(SyntaxKind.ColonToken, bounds.token)
      assertName("Eq", bounds.name)
    }

    it("should parse context bounds on some but not all parameters") {
      val fn = mkFunctionMember("def f[K: Eq, V](a: K, b: V): bool = true")
      val generics = assertSome(fn.genericParameters)

      val k = assertIndex(0, generics.parameters.items)
      assertTokenText("K", k.identifier)
      assertName("Eq", assertSome(k.bounds).name)

      val v = assertIndex(1, generics.parameters.items)
      assertTokenText("V", v.identifier)
      assertNone(v.bounds)
    }

    /** A requirement is a `def` with no body. `FunctionDeclarationSyntax`
      * already models the body as optional, so a trait member needs no separate
      * syntax.
      */
    it("should parse trait members as bodiless functions") {
      val decl = mkTraitMember("trait Eq[T] { def equals(a: T, b: T): bool }")
      val member = assertSingle(decl.template.members)
      val method = member match {
        case method: MemberSyntax.FunctionDeclarationSyntax => method
        case _ => throw new AssertionError("expected a function declaration")
      }

      assertTokenText("equals", method.identifier)
      assertNone(method.body)
      assertName("bool", assertSome(method.typeAnnotation).typ)
    }

    /** An operator declaration is a function declaration whose name is the
      * operator's own text. What makes it an operator is the token a trait
      * claims, not a different kind of syntax (ADR 0004).
      */
    it("should parse an operator declaration in a trait") {
      val decl = mkTraitMember(
        "trait Eq[T] { operator ==(a: T, b: T): bool }"
      )
      val method = assertFunctionMember(assertSingle(decl.template.members))

      assertTokenText("operator", method.defKeyword)
      assertTokenKind(SyntaxKind.EqualsEqualsToken, method.identifier)
      assertTokenText("==", method.identifier)
      assertNone(method.body)
      assertName("bool", assertSome(method.typeAnnotation).typ)
    }

    it("should parse an operator implementation in a given") {
      val decl = mkGivenMember(
        "given Eq[int] { operator ==(a: int, b: int): bool = a == b }"
      )
      val method = assertFunctionMember(assertSingle(decl.template.members))

      assertTokenText("==", method.identifier)
      assertSome(method.body)
    }

    it("should parse every comparison operator a trait can declare") {
      val decl = mkTraitMember(
        "trait Ord[T] { operator <(a: T, b: T): bool\n" +
          "operator <=(a: T, b: T): bool\n" +
          "operator >(a: T, b: T): bool\n" +
          "operator >=(a: T, b: T): bool }"
      )
      val names = decl.template.members match {
        case members =>
          def walk(items: List[MemberSyntax]): Seq[String] =
            items match {
              case List.Nil => Seq.empty
              case List.Cons(head, tail) =>
                assertFunctionMember(head).identifier.text +: walk(tail)
            }
          walk(members)
      }

      names shouldBe Seq("<", "<=", ">", ">=")
    }

    /** `operator` stays an identifier everywhere else. It cannot be reserved:
      * the compiler's own sources use it as a field and parameter name, and
      * `pncs` has to compile its transpiled twin.
      */
    it("should keep operator usable as an identifier") {
      val decl = mkClassMember(
        "class BinaryOperator(operator: int)"
      )
      val parameter = assertSingle(decl.parameters)
      assertTokenText("operator", parameter.identifier)
    }

    it("should reject a token that is not a binary operator") {
      val tree = mkSyntaxTree("trait Weird[T] { operator ~(a: T, b: T): bool }")
      treeDiagnosticMessages(tree) should contain(
        "~ is not a binary operator and cannot be declared"
      )
    }

    /** `[derive(…)]` is unambiguous at member position: no expression in the
      * language starts with `[` (ADR 0004).
      */
    it("should parse a derive attribute on a class") {
      val decl =
        mkClassMember("[derive(Eq, Ord, Show)]\nclass Point(x: int, y: int)")
      val attribute = assertSome(decl.derives)

      assertTokenKind(SyntaxKind.OpenBracketToken, attribute.openBracketToken)
      assertTokenText("derive", attribute.deriveToken)
      derivedNames(attribute) shouldBe Seq("Eq", "Ord", "Show")
    }

    it("should parse a derive attribute on an enum") {
      val decl = mkEnumMember(
        "[derive(Eq)]\nenum Color { case Red()\ncase Green() }"
      )
      derivedNames(assertSome(decl.derives)) shouldBe Seq("Eq")
    }

    it("should parse a class with no derive attribute") {
      assertNone(mkClassMember("class Point(x: int, y: int)").derives)
    }

    it("should reject derive on a declaration with no parameters") {
      val tree = mkSyntaxTree("[derive(Eq)]\ndef f(): int = 1")
      treeDiagnosticMessages(tree) should contain(
        "derive cannot be applied to a function"
      )
    }

    it("should reject an attribute that is not derive") {
      val tree = mkSyntaxTree("[trace(Eq)]\nclass Point(x: int)")
      treeDiagnosticMessages(tree) should contain("trace is not an attribute")
    }

    /** A name is only generic when the `[` is on its line. Without this the
      * `using` that opens every transpiled file swallows the attribute below
      * it, reading `int[derive(…)]` as a generic name.
      */
    it("should not read an attribute as the preceding using's type arguments") {
      val source = "using panther.int\n\n[derive(Eq)]\nclass P(x: int)"
      treeDiagnosticMessages(mkSyntaxTree(source)) shouldBe empty
      derivedNames(assertSome(mkClassMember(source).derives)) shouldBe Seq("Eq")
    }

    it("should still parse a generic name written on one line") {
      val decl = mkClassMember("class Box(items: List[int])")
      assertTokenText("Box", decl.identifier)
    }
  }
}
