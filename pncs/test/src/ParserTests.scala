import panther.{bool, int, panic, string}
import utest.*

object ParserTests extends TestSuite {

  val tests = Tests {
    def mkSyntaxTree(text: string): SyntaxTree =
      MakeSyntaxTree.parseContent(text)

    def mkSyntaxTreeExpr(text: string): Expression = {
      mkSyntaxTreeStatement(text) match {
        case StatementSyntax.ExpressionStatement(expr) => expr
        case _ => panic("Expected expression")
      }
    }

    def mkSyntaxTreeStatement(text: string): StatementSyntax = {
      mkSyntaxTreeMember(text) match {
        case MemberSyntax.GlobalStatementSyntax(statement) => statement
        case _ => panic("Expected statement")
      }
    }

    def mkSyntaxTreeMember(text: string): MemberSyntax = {
      val tree = mkSyntaxTree(text)
      val root = tree.root
      assert(root.members.length == 1)
      root.members match {
        case List.Cons(member, List.Nil) => member

        case _ => panic("Expected single expression")
      }
    }

    def numberExpr(expression: Expression): int = {
      expression match {
        case Expression.LiteralExpression(_, SyntaxTokenValue.Number(n)) =>
          n
        case _ => panic("Expected number expression")
      }
    }

    def boolExpr(expression: Expression): bool = {
      expression match {
        case Expression.LiteralExpression(_, SyntaxTokenValue.Boolean(n)) =>
          n
        case _ => panic("Expected number expression")
      }
    }

    def tokenKind(token: SyntaxToken): int = token.kind

    def identifierExpr(expression: Expression): string = {
      expression match {
        case Expression.IdentifierName(name) =>
          name.identifier.text
        case _ => panic("Expected identifierExpr expression")
      }
    }

    test("Parser") {

      test("expressions") {

        test("binary") {
          val expr = mkSyntaxTreeExpr("1 + 2")
          expr match {
            case Expression.BinaryExpression(left, op, right) =>
              assert(numberExpr(left) == 1)
              assert(tokenKind(op) == SyntaxKind.PlusToken)
              assert(numberExpr(right) == 2)

            case _ => assert(false)
          }

        }

        test("unary") {
          val expr = mkSyntaxTreeExpr("-1")
          expr match {
            case Expression.UnaryExpression(op, operand) =>
              assert(tokenKind(op) == SyntaxKind.DashToken)
              assert(numberExpr(operand) == 1)

            case _ => assert(false)
          }
        }

        test("parenthesized") {
          val expr = mkSyntaxTreeExpr("(1)")
          expr match {
            case Expression.GroupExpression(open, expression, close) =>
              assert(tokenKind(open) == SyntaxKind.OpenParenToken)
              assert(numberExpr(expression) == 1)
              assert(tokenKind(close) == SyntaxKind.CloseParenToken)

            case _ => assert(false)
          }
        }

        test("precedence") {
          val expr = mkSyntaxTreeExpr("1 + 2 * 3")
          expr match {
            case Expression.BinaryExpression(left, op, right) =>
              assert(numberExpr(left) == 1)
              assert(tokenKind(op) == SyntaxKind.PlusToken)

              right match {
                case Expression.BinaryExpression(left, op, right) =>
                  assert(numberExpr(left) == 2)
                  assert(tokenKind(op) == SyntaxKind.StarToken)
                  assert(numberExpr(right) == 3)

                case _ => assert(false)
              }

            case _ => assert(false)
          }
        }

        test("associativity") {
          val expr = mkSyntaxTreeExpr("1 - 2 - 3")
          expr match {
            case Expression.BinaryExpression(left, op, right) =>
              left match {
                case Expression.BinaryExpression(left, op, right) =>
                  assert(numberExpr(left) == 1)
                  assert(tokenKind(op) == SyntaxKind.DashToken)
                  assert(numberExpr(right) == 2)
                case _ => assert(false)
              }
              assert(tokenKind(op) == SyntaxKind.DashToken)
              assert(numberExpr(right) == 3)

            case _ => assert(false)
          }
        }

        test("assignment") {
          val expr = mkSyntaxTreeExpr("a = 1")
          expr match {
            case Expression.AssignmentExpression(left, op, right) =>
              assert(tokenKind(op) == SyntaxKind.EqualsToken)
              assert(identifierExpr(left) == "a")
              assert(numberExpr(right) == 1)

            case _ => assert(false)
          }
        }

        test("function call") {
          test("with multiple arguments") {
            val expr = mkSyntaxTreeExpr("f(1, 2)")
            expr match {
              case Expression.CallExpression(name, open, args, close) =>
                assert(identifierExpr(name) == "f")
                assert(tokenKind(open) == SyntaxKind.OpenParenToken)
                assert(args.expressions.length == 2)
                assert(
                  numberExpr(args.expressions.getUnsafe(0).expression) == 1
                )
                assert(
                  numberExpr(args.expressions.getUnsafe(1).expression) == 2
                )
                assert(tokenKind(close) == SyntaxKind.CloseParenToken)

              case _ => assert(false)
            }
          }

          test("with no arguments") {
            val expr = mkSyntaxTreeExpr("f()")
            expr match {
              case Expression.CallExpression(name, open, args, close) =>
                assert(identifierExpr(name) == "f")
                assert(tokenKind(open) == SyntaxKind.OpenParenToken)
                assert(args.expressions.length == 0)
                assert(tokenKind(close) == SyntaxKind.CloseParenToken)

              case _ => assert(false)
            }
          }

          test("with single argument") {
            val expr = mkSyntaxTreeExpr("f(1)")
            expr match {
              case Expression.CallExpression(name, open, args, close) =>
                assert(identifierExpr(name) == "f")
                assert(tokenKind(open) == SyntaxKind.OpenParenToken)
                assert(args.expressions.length == 1)
                assert(
                  numberExpr(args.expressions.getUnsafe(0).expression) == 1
                )
                assert(tokenKind(close) == SyntaxKind.CloseParenToken)

              case _ => assert(false)
            }
          }
        }

        test("if") {
          val expr = mkSyntaxTreeExpr("if (true) 1 else 2")
          expr match {
            case Expression.If(
                  ifKeyword,
                  openParen,
                  condition,
                  closeParen,
                  thenExpression,
                  Option.Some(ElseSyntax(elseKeyword, elseExpression))
                ) =>
              assert(tokenKind(ifKeyword) == SyntaxKind.IfKeyword)
              assert(tokenKind(openParen) == SyntaxKind.OpenParenToken)
              assert(tokenKind(closeParen) == SyntaxKind.CloseParenToken)
              assert(tokenKind(elseKeyword) == SyntaxKind.ElseKeyword)
              assert(boolExpr(condition))
              assert(numberExpr(thenExpression) == 1)
              assert(numberExpr(elseExpression) == 2)

            case _ => assert(false)
          }
        }

        test("while") {
          val expr = mkSyntaxTreeExpr("while (true) 1")
          expr match {
            case Expression.WhileExpression(
                  whileKeyword,
                  openParen,
                  condition,
                  closeParen,
                  body
                ) =>
              assert(tokenKind(whileKeyword) == SyntaxKind.WhileKeyword)
              assert(tokenKind(openParen) == SyntaxKind.OpenParenToken)
              assert(tokenKind(closeParen) == SyntaxKind.CloseParenToken)
              assert(boolExpr(condition))
              assert(numberExpr(body) == 1)

            case _ => assert(false)
          }
        }

        test("block") {
          val expr = mkSyntaxTreeExpr("{ 1 }")
          expr match {
            case Expression.BlockExpression(
                  open,
                  BlockExpressionListSyntax(
                    statements,
                    Option.Some(expression)
                  ),
                  close
                ) =>
              assert(tokenKind(open) == SyntaxKind.OpenBraceToken)
              assert(statements.length == 0)
              assert(numberExpr(expression) == 1)
              assert(tokenKind(close) == SyntaxKind.CloseBraceToken)

            case _ => assert(false)
          }
        }
      }
      test("functions") {

        test("no parameters") {
          val expr = mkSyntaxTreeMember("def f() = { 1 }")
          expr match {
            case MemberSyntax.FunctionDeclarationSyntax(
                  defKeyword,
                  name,
                  genericParameters,
                  openParenToken,
                  parameters,
                  closeParenToken,
                  typeAnnotation,
                  body
                ) =>
              assert(tokenKind(defKeyword) == SyntaxKind.DefKeyword)
              assert(name.text == "f")
              assert(parameters.length == 0)
              assert(typeAnnotation.isEmpty())
              body match {
                case Option.Some(
                      FunctionBodySyntax(
                        equalToken,
                        Expression.BlockExpression(
                          openBrace,
                          BlockExpressionListSyntax(
                            statements,
                            Option.Some(expression)
                          ),
                          closeBrace
                        )
                      )
                    ) =>
                  assert(tokenKind(equalToken) == SyntaxKind.EqualsToken)
                  assert(tokenKind(openBrace) == SyntaxKind.OpenBraceToken)
                  assert(statements.length == 0)
                  assert(numberExpr(expression) == 1)
                  assert(tokenKind(closeBrace) == SyntaxKind.CloseBraceToken)

                case _ => assert(false)
              }

            case _ => assert(false)
          }
        }

        test("with parameters") {
          val expr = mkSyntaxTreeMember("def f(a: int, b: int) = { 1 }")
          expr match {
            case MemberSyntax.FunctionDeclarationSyntax(
                  defKeyword,
                  name,
                  genericParameters,
                  openParenToken,
                  parameters,
                  closeParenToken,
                  typeAnnotation,
                  body
                ) =>
              assert(tokenKind(defKeyword) == SyntaxKind.DefKeyword)
              assert(name.text == "f")
              assert(parameters.length == 2)
              assert(parameters.getUnsafe(0).identifier.text == "a")
              assert(parameters.getUnsafe(1).identifier.text == "b")
              assert(typeAnnotation.isEmpty())
              body match {
                case Option.Some(
                      FunctionBodySyntax(
                        equalToken,
                        Expression.BlockExpression(
                          openBrace,
                          BlockExpressionListSyntax(
                            statements,
                            Option.Some(expression)
                          ),
                          closeBrace
                        )
                      )
                    ) =>
                  assert(tokenKind(equalToken) == SyntaxKind.EqualsToken)
                  assert(tokenKind(openBrace) == SyntaxKind.OpenBraceToken)
                  assert(statements.length == 0)
                  assert(numberExpr(expression) == 1)
                  assert(tokenKind(closeBrace) == SyntaxKind.CloseBraceToken)

                case _ => assert(false)
              }

            case _ => assert(false)
          }
        }

        test("with return type") {
          val expr = mkSyntaxTreeMember("def f(): int = { 1 }")
          expr match {
            case MemberSyntax.FunctionDeclarationSyntax(
                  defKeyword,
                  name,
                  genericParameters,
                  openParenToken,
                  parameters,
                  closeParenToken,
                  typeAnnotation,
                  body
                ) =>
              assert(tokenKind(defKeyword) == SyntaxKind.DefKeyword)
              assert(name.text == "f")
              assert(parameters.length == 0)
              typeAnnotation match {
                case Option.Some(
                      TypeAnnotationSyntax(
                        colon,
                        NameSyntax.SimpleName(
                          SimpleNameSyntax.IdentifierNameSyntax(identifier)
                        )
                      )
                    ) =>
                  assert(tokenKind(colon) == SyntaxKind.ColonToken)
                  assert(identifier.text == "int")
                case _ => assert(false)
              }
              body match {
                case Option.Some(
                      FunctionBodySyntax(
                        equalToken,
                        Expression.BlockExpression(
                          openBrace,
                          BlockExpressionListSyntax(
                            statements,
                            Option.Some(expression)
                          ),
                          closeBrace
                        )
                      )
                    ) =>
                  assert(tokenKind(equalToken) == SyntaxKind.EqualsToken)
                  assert(tokenKind(openBrace) == SyntaxKind.OpenBraceToken)
                  assert(statements.length == 0)
                  assert(numberExpr(expression) == 1)
                  assert(tokenKind(closeBrace) == SyntaxKind.CloseBraceToken)

                case _ => assert(false)
              }

            case _ => assert(false)
          }
        }
      }
    }
  }
}
