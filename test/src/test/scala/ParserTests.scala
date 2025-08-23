import panther.{assert => _, *}
import TestHelpers._
import utest._

object ParserTests extends TestSuite {
  val tests = Tests {
    test("expressions") {
      test("binary") {
        val expr = mkBinaryExpr("1 + 2")
        assertNumberExpr(1, expr.left)
        assertNumberExpr(2, expr.right)
        assertTokenKind(SyntaxKind.PlusToken, expr.operator)
      }

      test("unary") {
        val expr = mkUnaryExpr("-1")
        assertTokenKind(SyntaxKind.DashToken, expr.operator)
        assertNumberExpr(1, expr.expression)
      }

      test("groups") {
        val expr = mkGroupExpr("(12)")
        assertTokenKind(SyntaxKind.OpenParenToken, expr.openParen)
        assertNumberExpr(12, expr.expression)
        assertTokenKind(SyntaxKind.CloseParenToken, expr.closeParen)
      }

      test("precedence") {
        val expr = mkBinaryExpr("1 + 2 * 3")
        assertNumberExpr(1, expr.left)
        assertTokenKind(SyntaxKind.PlusToken, expr.operator)
        val right = assertBinaryExpr(expr.right)
        assertNumberExpr(2, right.left)
        assertTokenKind(SyntaxKind.StarToken, right.operator)
        assertNumberExpr(3, right.right)
      }

      test("associativity") {
        val expr = mkBinaryExpr("1 - 2 - 3")
        val left = assertBinaryExpr(expr.left)
        assertNumberExpr(1, left.left)
        assertTokenKind(SyntaxKind.DashToken, left.operator)
        assertNumberExpr(2, left.right)
        assertTokenKind(SyntaxKind.DashToken, expr.operator)
        assertNumberExpr(3, expr.right)
      }

      test("assignment") {
        val expr = mkAssignmentExpr("a = 1")
        assertTokenKind(SyntaxKind.EqualsToken, expr.equals)
        assertNumberExpr(1, expr.right)
        assertIdentifierExpr("a", expr.left)
      }

      test("calls") {
        test("with no arguments") {
          val expr = mkCallExpr("f()")
          assertIdentifierExpr("f", expr.name)
          assertNone(expr.genericArguments)
          assertTokenKind(SyntaxKind.OpenParenToken, expr.openParen)
          assertEmpty(expr.arguments.expressions)
          assertTokenKind(SyntaxKind.CloseParenToken, expr.closeParen)
        }

        test("with one argument") {
          val expr = mkCallExpr("f(1)")
          assertIdentifierExpr("f", expr.name)
          assertNone(expr.genericArguments)
          assertTokenKind(SyntaxKind.OpenParenToken, expr.openParen)
          val arg = assertSingle(expr.arguments.expressions)
          assertNumberExpr(1, arg.expression)
          assertTokenKind(SyntaxKind.CloseParenToken, expr.closeParen)
        }

        test("with multiple arguments") {
          val expr = mkCallExpr("f(1, 2)")
          assertIdentifierExpr("f", expr.name)
          val args = expr.arguments
          assertNumberExpr(1, assertIndex(0, args.expressions).expression)
          assertNumberExpr(2, assertIndex(1, args.expressions).expression)
        }

        test("with generic arguments") {
          val expr = mkCallExpr("f[int](1)")
          val ident = assertGenericIdentifierExpr(expr.name)
          assertTokenText("f", ident.identifier)
          assertTokenKind(
            SyntaxKind.OpenBracketToken,
            ident.typeArgumentlist.lessThanToken
          )
          assert(ident.typeArgumentlist.arguments.length == 1)
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
      }

      test("ifs") {
        val expr = mkIfExpr("if (true) 1 else 2")
        assertTokenKind(SyntaxKind.IfKeyword, expr.ifKeyword)
        assertTrueExpr(expr.condition)
        assertNumberExpr(1, expr.thenExpr)

        val elseExpr = assertSome(expr.elseExpr)
        assertTokenKind(SyntaxKind.ElseKeyword, elseExpr.elseKeyword)
        assertNumberExpr(2, elseExpr.expression)
      }

      test("whiles") {
        val expr = mkWhileExpr("while (true) 1")
        assertTokenKind(SyntaxKind.WhileKeyword, expr.whileKeyword)
        assertTokenKind(SyntaxKind.OpenParenToken, expr.openParen)
        assertTrueExpr(expr.condition)
        assertTokenKind(SyntaxKind.CloseParenToken, expr.closeParen)
        assertNumberExpr(1, expr.body)
      }

      test("blocks") {
        test("with expression") {
          val expr = mkBlockExpr("{ 1 }")
          assertTokenKind(SyntaxKind.OpenBraceToken, expr.openBrace)
          assertEmpty(expr.block.statements)
          val blockExpr = assertSome(expr.block.expression)
          assertNumberExpr(1, blockExpr)
          assertTokenKind(SyntaxKind.CloseBraceToken, expr.closeBrace)
        }

        test("with statement") {
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
      }

      test("dot on new line") {
        val expr = mkMemberAccessExpr("a\n.b")
        assertIdentifierExpr("a", expr.left)
        assertTokenKind(SyntaxKind.DotToken, expr.dotToken)
        assertSimpleNameIdentifierExpr("b", expr.right)
      }

      test("matches") {
        test("simple match") {
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

        test("empty match case") {
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
          assert(expr.diagnostics.count() == 0)
        }
      }

      test("is expressions") {
        val expr = mkIsExpr("x is int")
        assertIdentifierExpr("x", expr.expression)
        assertTokenKind(SyntaxKind.IsKeyword, expr.isKeyword)
        // TODO: add assertion for the type name
      }
    }

    test("functions") {
      test("with no parameters") {
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

      test("with parameters") {
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

      test("with return type") {
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
    }
  }
}
