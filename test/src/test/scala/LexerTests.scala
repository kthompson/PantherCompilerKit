import panther.{assert => _, *}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LexerTests extends AnyFunSpec with Matchers {
  def mkTokens(text: string): Array[SyntaxToken] = {
    val sourceFile = new SourceFile(text, "test.pn")
    val diagnostics = new DiagnosticBag(CompilerSettingsFactory.default)
    val lexer = new Lexer(sourceFile, diagnostics)
    MakeTokenList.create(lexer)
  }

  describe("Lexer") {
    it("should handle single token") {
      val tokens = mkTokens("1")
      tokens.length shouldEqual 2
      tokens(0).kind shouldEqual SyntaxKind.NumberToken
      tokens(1).kind shouldEqual SyntaxKind.EndOfInputToken
    }

    it("should handle multiple tokens") {
      val tokens = mkTokens("1 + 2")
      tokens.length shouldEqual 4
      tokens(0).kind shouldEqual SyntaxKind.NumberToken
      tokens(1).kind shouldEqual SyntaxKind.PlusToken
      tokens(2).kind shouldEqual SyntaxKind.NumberToken
      tokens(3).kind shouldEqual SyntaxKind.EndOfInputToken
    }

    it("should handle whitespace trivia") {
      val tokens = mkTokens("1 + 2")
      tokens.length shouldEqual 4
      tokens(0).leading.length shouldEqual 0
      tokens(0).trailing.length shouldEqual 1
      tokens(0).trailing(0).kind shouldEqual SyntaxKind.WhitespaceTrivia
    }

    it("should handle comments") {
      val tokens = mkTokens("1 // comment")
      tokens.length shouldEqual 2
      tokens(0).kind shouldEqual SyntaxKind.NumberToken
      tokens(0).trailing.length shouldEqual 2
      tokens(0).trailing(0).kind shouldEqual SyntaxKind.WhitespaceTrivia
      tokens(0).trailing(1).kind shouldEqual SyntaxKind.LineCommentTrivia
      tokens(1).kind shouldEqual SyntaxKind.EndOfInputToken
    }

    it("should handle string tokens") {
      val tokens = mkTokens("\"hello\"")
      tokens.length shouldEqual 2
      tokens(0).kind shouldEqual SyntaxKind.StringToken
      tokens(1).kind shouldEqual SyntaxKind.EndOfInputToken
    }

    it("should handle number tokens") {
      val tokens = mkTokens("123")
      tokens.length shouldEqual 2
      tokens(0).kind shouldEqual SyntaxKind.NumberToken
      tokens(1).kind shouldEqual SyntaxKind.EndOfInputToken
    }

    it("should handle identifier tokens") {
      val tokens = mkTokens("foo")
      tokens.length shouldEqual 2
      tokens(0).kind shouldEqual SyntaxKind.IdentifierToken
      tokens(1).kind shouldEqual SyntaxKind.EndOfInputToken
    }

    it("should handle fancy strings") {
      val tokens = mkTokens("\"hello\\nworld\"")
      tokens.length shouldEqual 2
      tokens(0).kind shouldEqual SyntaxKind.StringToken
      tokens(1).kind shouldEqual SyntaxKind.EndOfInputToken
    }

    it("should handle keywords") {
      val tokens = mkTokens("if else while val var def")
      tokens.length shouldEqual 7 // 6 keywords + EndOfInputToken
      tokens(0).kind shouldEqual SyntaxKind.IfKeyword
      tokens(1).kind shouldEqual SyntaxKind.ElseKeyword
      tokens(2).kind shouldEqual SyntaxKind.WhileKeyword
      tokens(3).kind shouldEqual SyntaxKind.ValKeyword
      tokens(4).kind shouldEqual SyntaxKind.VarKeyword
      tokens(5).kind shouldEqual SyntaxKind.DefKeyword
      tokens(6).kind shouldEqual SyntaxKind.EndOfInputToken
    }

    it("should handle operators") {
      val tokens = mkTokens("+ - * / == != < <= > >=")
      tokens.length shouldEqual 11 // 10 operators + EndOfInputToken
      tokens(0).kind shouldEqual SyntaxKind.PlusToken
      tokens(1).kind shouldEqual SyntaxKind.DashToken
      tokens(2).kind shouldEqual SyntaxKind.StarToken
      tokens(3).kind shouldEqual SyntaxKind.SlashToken
      tokens(4).kind shouldEqual SyntaxKind.EqualsEqualsToken
      tokens(5).kind shouldEqual SyntaxKind.BangEqualsToken
      tokens(6).kind shouldEqual SyntaxKind.LessThanToken
      tokens(7).kind shouldEqual SyntaxKind.LessThanEqualsToken
      tokens(8).kind shouldEqual SyntaxKind.GreaterThanToken
      tokens(9).kind shouldEqual SyntaxKind.GreaterThanEqualsToken
      tokens(10).kind shouldEqual SyntaxKind.EndOfInputToken
    }

    it("should handle parentheses") {
      val tokens = mkTokens("()")
      tokens.length shouldEqual 3
      tokens(0).kind shouldEqual SyntaxKind.OpenParenToken
      tokens(1).kind shouldEqual SyntaxKind.CloseParenToken
      tokens(2).kind shouldEqual SyntaxKind.EndOfInputToken
    }

    it("should handle braces") {
      val tokens = mkTokens("{}")
      tokens.length shouldEqual 3
      tokens(0).kind shouldEqual SyntaxKind.OpenBraceToken
      tokens(1).kind shouldEqual SyntaxKind.CloseBraceToken
      tokens(2).kind shouldEqual SyntaxKind.EndOfInputToken
    }

    it("should handle brackets") {
      val tokens = mkTokens("[]")
      tokens.length shouldEqual 3
      tokens(0).kind shouldEqual SyntaxKind.OpenBracketToken
      tokens(1).kind shouldEqual SyntaxKind.CloseBracketToken
      tokens(2).kind shouldEqual SyntaxKind.EndOfInputToken
    }
  }
}