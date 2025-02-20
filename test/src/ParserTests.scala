import panther._
import Helpers._

object ParserTests {
  def run(): unit = {
    expressions()
    functions()
  }

  def expressions(): unit = {
    binary()
    unary()
    groups()
    precedence()
    associativity()
    assignment()
    calls()
    ifs()
    whiles()
    blocks()
  }

  def binary(): unit = {
    val expr = mkBinaryExpr("1 + 2")
    assertNumberExpr(1, expr.left)
    assertNumberExpr(2, expr.right)
    assertTokenKind(SyntaxKind.PlusToken, expr.operator)
  }

  def unary(): unit = {
    val expr = mkUnaryExpr("-1")
    assertTokenKind(SyntaxKind.DashToken, expr.operator)
    assertNumberExpr(1, expr.expression)
  }

  def groups(): unit = {
    val expr = mkGroupExpr("(12)")
    assertTokenKind(SyntaxKind.OpenParenToken, expr.openParen)
    assertNumberExpr(12, expr.expression)
    assertTokenKind(SyntaxKind.CloseParenToken, expr.closeParen)
  }

  def precedence(): unit = {
    val expr = mkBinaryExpr("1 + 2 * 3")
    assertNumberExpr(1, expr.left)
    assertTokenKind(SyntaxKind.PlusToken, expr.operator)
    val right = assertBinaryExpr(expr.right)
    assertNumberExpr(2, right.left)
    assertTokenKind(SyntaxKind.StarToken, right.operator)
    assertNumberExpr(3, right.right)
  }

  def associativity(): unit = {
    val expr = mkBinaryExpr("1 - 2 - 3")
    val left = assertBinaryExpr(expr.left)
    assertNumberExpr(1, left.left)
    assertTokenKind(SyntaxKind.DashToken, left.operator)
    assertNumberExpr(2, left.right)
    assertTokenKind(SyntaxKind.DashToken, expr.operator)
    assertNumberExpr(3, expr.right)
  }

  def assignment(): unit = {
    val expr = mkAssignmentExpr("a = 1")

    assertTokenKind(SyntaxKind.EqualsToken, expr.equals)
    assertNumberExpr(1, expr.right)
    assertIdentifierExpr("a", expr.left)
  }

  def calls(): unit = {}

  def ifs(): unit = {
    val expr = mkIfExpr("if (true) 1 else 2")

    assertTokenKind(SyntaxKind.IfKeyword, expr.ifKeyword)
    assertTrueExpr(expr.condition)
    assertNumberExpr(1, expr.thenExpr)

    val elseExpr = Assert.some(expr.elseExpr)
    assertTokenKind(SyntaxKind.ElseKeyword, elseExpr.elseKeyword)
    assertNumberExpr(2, elseExpr.expression)
  }

  def whiles(): unit = {}

  def blocks(): unit = {
    blockWithExpression()
    blockWithStatement()
  }

  def blockWithExpression(): unit = {
    val expr = mkBlockExpr("{ 1 }")
    assertTokenKind(SyntaxKind.OpenBraceToken, expr.openBrace)
    Assert.empty(expr.block.statements)
    val blockExpr = Assert.some(expr.block.expression)
    assertNumberExpr(1, blockExpr)
    assertTokenKind(SyntaxKind.CloseBraceToken, expr.closeBrace)
  }

  def blockWithStatement(): unit = {
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
    val statement = Assert.single(statements)
    val declaration = assertVariableDeclaration(statement)
    assertTokenKind(SyntaxKind.ValKeyword, declaration.valOrVarKeyword)
    assertTokenText("a", declaration.identifier)
    assertTokenKind(SyntaxKind.EqualsToken, declaration.equalToken)
    assertNumberExpr(1, declaration.expression)

    // a
    val blockExpr = Assert.some(expr.block.expression)
    assertIdentifierExpr("a", blockExpr)

    // }
    assertTokenKind(SyntaxKind.CloseBraceToken, expr.closeBrace)
  }

  def functions(): unit = {}

}
