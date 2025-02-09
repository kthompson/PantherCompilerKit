import panther.string
import utest._

object LexerTests extends TestSuite {

  val tests = Tests {
    def mkTokens(text: string): Array[SyntaxToken] = {
      val sourceFile = new SourceFile(text, "test.pn")
      val diagnostics = new DiagnosticBag()
      val lexer = new Lexer(sourceFile, diagnostics)
      MakeTokenList.create(lexer)
    }

    test("Lexer") {
      test("supports") {
        test("numbers") {
          val tokens = mkTokens("1 + 2")
          assert(tokens.length == 4)
          assert(tokens(0).kind == SyntaxKind.NumberToken)
          assert(tokens(1).kind == SyntaxKind.PlusToken)
          assert(tokens(2).kind == SyntaxKind.NumberToken)
          assert(tokens(3).kind == SyntaxKind.EndOfInputToken)
        }

        test("strings") {
          val tokens = mkTokens("\"hello\"")
          assert(tokens.length == 2)
          assert(tokens(0).kind == SyntaxKind.StringToken)
          assert(tokens(1).kind == SyntaxKind.EndOfInputToken)
        }

        test("identifiers") {
          val tokens = mkTokens("foo")
          assert(tokens.length == 2)
          assert(tokens(0).kind == SyntaxKind.IdentifierToken)
          assert(tokens(1).kind == SyntaxKind.EndOfInputToken)
        }

        test("keywords") {
          val tokens = mkTokens("val x = 10")
          assert(tokens.length == 5)
          assert(tokens(0).kind == SyntaxKind.ValKeyword)
          assert(tokens(1).kind == SyntaxKind.IdentifierToken)
          assert(tokens(2).kind == SyntaxKind.EqualsToken)
          assert(tokens(3).kind == SyntaxKind.NumberToken)
          assert(tokens(4).kind == SyntaxKind.EndOfInputToken)
        }

        test("operators") {
          val tokens = mkTokens("+-*/")
          assert(tokens.length == 5)
          assert(tokens(0).kind == SyntaxKind.PlusToken)
          assert(tokens(1).kind == SyntaxKind.DashToken)
          assert(tokens(2).kind == SyntaxKind.StarToken)
          assert(tokens(3).kind == SyntaxKind.SlashToken)
          assert(tokens(4).kind == SyntaxKind.EndOfInputToken)
        }

        test("parentheses") {
          val tokens = mkTokens("()")
          assert(tokens.length == 3)
          assert(tokens(0).kind == SyntaxKind.OpenParenToken)
          assert(tokens(1).kind == SyntaxKind.CloseParenToken)
          assert(tokens(2).kind == SyntaxKind.EndOfInputToken)
        }

        test("braces") {
          val tokens = mkTokens("{}")
          assert(tokens.length == 3)
          assert(tokens(0).kind == SyntaxKind.OpenBraceToken)
          assert(tokens(1).kind == SyntaxKind.CloseBraceToken)
          assert(tokens(2).kind == SyntaxKind.EndOfInputToken)
        }

        test("brackets") {
          val tokens = mkTokens("[]")
          assert(tokens.length == 3)
          assert(tokens(0).kind == SyntaxKind.OpenBracketToken)
          assert(tokens(1).kind == SyntaxKind.CloseBracketToken)
          assert(tokens(2).kind == SyntaxKind.EndOfInputToken)
        }

        test("comments") {
          val tokens = mkTokens("1 // comment")
          assert(tokens.length == 2)
          assert(tokens(0).kind == SyntaxKind.NumberToken)
          assert(tokens(0).trailing.length == 2)
          assert(tokens(0).trailing(0).kind == SyntaxKind.WhitespaceTrivia)
          assert(tokens(0).trailing(1).kind == SyntaxKind.LineCommentTrivia)
          assert(tokens(1).kind == SyntaxKind.EndOfInputToken)
        }

        test("whitespace") {
          val tokens = mkTokens("1 + 2")
          assert(tokens.length == 4)
          assert(tokens(0).leading.length == 0)
          assert(tokens(0).trailing.length == 1)
          assert(tokens(0).trailing(0).kind == SyntaxKind.WhitespaceTrivia)

          assert(tokens(1).kind == SyntaxKind.PlusToken)
          assert(tokens(1).leading.length == 0)
          assert(tokens(1).trailing.length == 1)
          assert(tokens(1).trailing(0).kind == SyntaxKind.WhitespaceTrivia)

          assert(tokens(2).kind == SyntaxKind.NumberToken)
          assert(tokens(2).leading.length == 0)
          assert(tokens(2).trailing.length == 0)

          assert(tokens(3).kind == SyntaxKind.EndOfInputToken)
          assert(tokens(3).leading.length == 0)
          assert(tokens(3).trailing.length == 0)
        }
      }
    }
  }
}
