import panther._
import Helpers._
import TestFramework._

object LexerTests {
  def run(): unit = {

    suite("Lexer Tests")
    test("singleToken")
    singleToken()

    test("multipleTokens")
    multipleTokens()

    test("whitespace")
    whitespace()

    test("comments")
    comments()

    test("strings")
    strings()

    test("numbers")
    numbers()

    test("identifiers")
    identifiers()

    test("keywords")
    keywords()

    test("operators")
    operators()

    test("parentheses")
    parentheses()

    test("braces")
    braces()

    test("brackets")
    brackets()
  }

  def singleToken(): unit = {
    val tokens = mkTokens("1")
    Assert.intEqual(2, tokens.length)
    Assert.intEqual(SyntaxKind.NumberToken, tokens(0).kind)
    Assert.intEqual(SyntaxKind.EndOfInputToken, tokens(1).kind)
  }

  def multipleTokens(): unit = {
    val tokens = mkTokens("1 + 2")
    Assert.intEqual(4, tokens.length)
    Assert.intEqual(SyntaxKind.NumberToken, tokens(0).kind)
    Assert.intEqual(SyntaxKind.PlusToken, tokens(1).kind)
    Assert.intEqual(SyntaxKind.NumberToken, tokens(2).kind)
    Assert.intEqual(SyntaxKind.EndOfInputToken, tokens(3).kind)
  }

  def whitespace(): unit = {
    val tokens = mkTokens("1 + 2")
    Assert.intEqual(4, tokens.length)

    Assert.intEqual(0, tokens(0).leading.length)
    Assert.intEqual(1, tokens(0).trailing.length)
    Assert.intEqual(SyntaxKind.WhitespaceTrivia, tokens(0).trailing(0).kind)
  }

  def comments(): unit = {
    val tokens = mkTokens("1 // comment")
    Assert.intEqual(2, tokens.length)
    Assert.intEqual(SyntaxKind.NumberToken, tokens(0).kind)
    Assert.intEqual(2, tokens(0).trailing.length)
    Assert.intEqual(SyntaxKind.WhitespaceTrivia, tokens(0).trailing(0).kind)
    Assert.intEqual(SyntaxKind.LineCommentTrivia, tokens(0).trailing(1).kind)
    Assert.intEqual(SyntaxKind.EndOfInputToken, tokens(1).kind)
  }

  def strings(): unit = {
    val tokens = mkTokens("\"hello\"")
    Assert.intEqual(2, tokens.length)
    Assert.intEqual(SyntaxKind.StringToken, tokens(0).kind)
    Assert.intEqual(SyntaxKind.EndOfInputToken, tokens(1).kind)
  }

  def numbers(): unit = {
    val tokens = mkTokens("123")
    Assert.intEqual(2, tokens.length)
    Assert.intEqual(SyntaxKind.NumberToken, tokens(0).kind)
    Assert.intEqual(SyntaxKind.EndOfInputToken, tokens(1).kind)
  }

  def identifiers(): unit = {
    val tokens = mkTokens("foo")
    Assert.intEqual(2, tokens.length)
    Assert.intEqual(SyntaxKind.IdentifierToken, tokens(0).kind)
    Assert.intEqual(SyntaxKind.EndOfInputToken, tokens(1).kind)
  }

  def keywords(): unit = {
    val tokens = mkTokens(
      "while if else namespace object out override new static to true using val"
    )
    Assert.intEqual(14, tokens.length)
    Assert.intEqual(SyntaxKind.WhileKeyword, tokens(0).kind)
    Assert.intEqual(SyntaxKind.IfKeyword, tokens(1).kind)
    Assert.intEqual(SyntaxKind.ElseKeyword, tokens(2).kind)
    Assert.intEqual(SyntaxKind.NamespaceKeyword, tokens(3).kind)
    Assert.intEqual(SyntaxKind.ObjectKeyword, tokens(4).kind)
    Assert.intEqual(SyntaxKind.OutKeyword, tokens(5).kind)
    Assert.intEqual(SyntaxKind.OverrideKeyword, tokens(6).kind)
    Assert.intEqual(SyntaxKind.NewKeyword, tokens(7).kind)
    Assert.intEqual(SyntaxKind.StaticKeyword, tokens(8).kind)
    Assert.intEqual(SyntaxKind.ToKeyword, tokens(9).kind)
    Assert.intEqual(SyntaxKind.TrueKeyword, tokens(10).kind)
    Assert.intEqual(SyntaxKind.UsingKeyword, tokens(11).kind)
    Assert.intEqual(SyntaxKind.ValKeyword, tokens(12).kind)
    Assert.intEqual(SyntaxKind.EndOfInputToken, tokens(13).kind)

  }

  def operators(): unit = {
    val tokens = mkTokens("+-*/")
    Assert.intEqual(5, tokens.length)
    Assert.intEqual(SyntaxKind.PlusToken, tokens(0).kind)
    Assert.intEqual(SyntaxKind.DashToken, tokens(1).kind)
    Assert.intEqual(SyntaxKind.StarToken, tokens(2).kind)
    Assert.intEqual(SyntaxKind.SlashToken, tokens(3).kind)
    Assert.intEqual(SyntaxKind.EndOfInputToken, tokens(4).kind)
  }

  def parentheses(): unit = {
    val tokens = mkTokens("()")
    Assert.intEqual(3, tokens.length)
    Assert.intEqual(SyntaxKind.OpenParenToken, tokens(0).kind)
    Assert.intEqual(SyntaxKind.CloseParenToken, tokens(1).kind)
    Assert.intEqual(SyntaxKind.EndOfInputToken, tokens(2).kind)
  }

  def braces(): unit = {
    val tokens = mkTokens("{}")
    Assert.intEqual(3, tokens.length)
    Assert.intEqual(SyntaxKind.OpenBraceToken, tokens(0).kind)
    Assert.intEqual(SyntaxKind.CloseBraceToken, tokens(1).kind)
    Assert.intEqual(SyntaxKind.EndOfInputToken, tokens(2).kind)
  }

  def brackets(): unit = {
    val tokens = mkTokens("[]")
    Assert.intEqual(3, tokens.length)
    Assert.intEqual(SyntaxKind.OpenBracketToken, tokens(0).kind)
    Assert.intEqual(SyntaxKind.CloseBracketToken, tokens(1).kind)
    Assert.intEqual(SyntaxKind.EndOfInputToken, tokens(2).kind)
  }

}
