import panther.{assert => _, *}
import utest._

object LexerTests extends TestSuite {
  def mkTokens(text: string): Array[SyntaxToken] = {
    val sourceFile = new SourceFile(text, "test.pn")
    val diagnostics = new DiagnosticBag()
    val lexer = new Lexer(sourceFile, diagnostics)
    MakeTokenList.create(lexer)
  }

  val tests = Tests {
    test("singleToken") {
      val tokens = mkTokens("1")
      assert(tokens.length == 2)
      assert(tokens(0).kind == SyntaxKind.NumberToken)
      assert(tokens(1).kind == SyntaxKind.EndOfInputToken)
    }

    test("multipleTokens") {
      val tokens = mkTokens("1 + 2")
      assert(tokens.length == 4)
      assert(tokens(0).kind == SyntaxKind.NumberToken)
      assert(tokens(1).kind == SyntaxKind.PlusToken)
      assert(tokens(2).kind == SyntaxKind.NumberToken)
      assert(tokens(3).kind == SyntaxKind.EndOfInputToken)
    }

    test("whitespace") {
      val tokens = mkTokens("1 + 2")
      assert(tokens.length == 4)

      assert(tokens(0).leading.length == 0)
      assert(tokens(0).trailing.length == 1)
      assert(tokens(0).trailing(0).kind == SyntaxKind.WhitespaceTrivia)
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

    test("strings") {
      test("simple string") {
        val tokens = mkTokens("\"hello\"")
        assert(tokens.length == 2)
        assert(tokens(0).kind == SyntaxKind.StringToken)
        assert(tokens(1).kind == SyntaxKind.EndOfInputToken)
      }

      test("fancy string") {
        val tokens = mkTokens("\"├──\"")
        assert(tokens.length == 2)
        assert(tokens(0).kind == SyntaxKind.StringToken)
        tokens(0).value match {
          case SyntaxTokenValue.String(value) =>
            assert(value == "├──")
          case _ =>
            throw new java.lang.AssertionError("Expected string value")
        }
        assert(tokens(1).kind == SyntaxKind.EndOfInputToken)
      }
    }

    test("numbers") {
      val tokens = mkTokens("123")
      assert(tokens.length == 2)
      assert(tokens(0).kind == SyntaxKind.NumberToken)
      assert(tokens(1).kind == SyntaxKind.EndOfInputToken)
    }

    test("identifiers") {
      val tokens = mkTokens("foo")
      assert(tokens.length == 2)
      assert(tokens(0).kind == SyntaxKind.IdentifierToken)
      assert(tokens(1).kind == SyntaxKind.EndOfInputToken)
    }

    test("keywords") {
      val tokens = mkTokens(
        "while if else namespace object out override new static to true using val"
      )
      assert(tokens.length == 14)
      assert(tokens(0).kind == SyntaxKind.WhileKeyword)
      assert(tokens(1).kind == SyntaxKind.IfKeyword)
      assert(tokens(2).kind == SyntaxKind.ElseKeyword)
      assert(tokens(3).kind == SyntaxKind.NamespaceKeyword)
      assert(tokens(4).kind == SyntaxKind.ObjectKeyword)
      assert(tokens(5).kind == SyntaxKind.OutKeyword)
      assert(tokens(6).kind == SyntaxKind.OverrideKeyword)
      assert(tokens(7).kind == SyntaxKind.NewKeyword)
      assert(tokens(8).kind == SyntaxKind.StaticKeyword)
      assert(tokens(9).kind == SyntaxKind.ToKeyword)
      assert(tokens(10).kind == SyntaxKind.TrueKeyword)
      assert(tokens(11).kind == SyntaxKind.UsingKeyword)
      assert(tokens(12).kind == SyntaxKind.ValKeyword)
      assert(tokens(13).kind == SyntaxKind.EndOfInputToken)
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
  }
}
