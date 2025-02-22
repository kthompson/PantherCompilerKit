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

  def calls(): unit = {
    callWithNoArguments()
    callWithOneArgument()
    callWithMultipleArguments()
    callWithGenericArguments()
  }

  def callWithNoArguments(): unit = {
    val expr = mkCallExpr("f()")
    assertIdentifierExpr("f", expr.name)
    Assert.none(expr.genericArguments)
    assertTokenKind(SyntaxKind.OpenParenToken, expr.openParen)
    Assert.empty(expr.arguments.expressions)
    assertTokenKind(SyntaxKind.CloseParenToken, expr.closeParen)
  }

  def callWithOneArgument(): unit = {
    val expr = mkCallExpr("f(1)")
    assertIdentifierExpr("f", expr.name)
    Assert.none(expr.genericArguments)
    assertTokenKind(SyntaxKind.OpenParenToken, expr.openParen)
    val arg = Assert.single(expr.arguments.expressions)
    assertNumberExpr(1, arg.expression)
    assertTokenKind(SyntaxKind.CloseParenToken, expr.closeParen)
  }

  def callWithMultipleArguments(): unit = {
    val expr = mkCallExpr("f(1, 2)")
    assertIdentifierExpr("f", expr.name)
    val args = expr.arguments
    assertNumberExpr(1, Assert.index(0, args.expressions).expression)
    assertNumberExpr(2, Assert.index(1, args.expressions).expression)
  }

  def callWithGenericArguments(): unit = {
    val expr = mkCallExpr("f[int](1)")
    val ident = assertGenericIdentifierExpr(expr.name)
    assertTokenText("f", ident.identifier)
    assertTokenKind(
      SyntaxKind.OpenBracketToken,
      ident.typeArgumentlist.lessThanToken
    )
    Assert.intEqual(1, ident.typeArgumentlist.arguments.length)
    val genArg = ident.typeArgumentlist.arguments(0)
    assertName("int", genArg.name)
    Assert.none(genArg.separator)
    assertTokenKind(
      SyntaxKind.CloseBracketToken,
      ident.typeArgumentlist.greaterThanToken
    )

    val args = expr.arguments
    assertNumberExpr(1, Assert.single(args.expressions).expression)
  }

  def ifs(): unit = {
    val expr = mkIfExpr("if (true) 1 else 2")

    assertTokenKind(SyntaxKind.IfKeyword, expr.ifKeyword)
    assertTrueExpr(expr.condition)
    assertNumberExpr(1, expr.thenExpr)

    val elseExpr = Assert.some(expr.elseExpr)
    assertTokenKind(SyntaxKind.ElseKeyword, elseExpr.elseKeyword)
    assertNumberExpr(2, elseExpr.expression)
  }

  def whiles(): unit = {
    val expr = mkWhileExpr("while (true) 1")

    assertTokenKind(SyntaxKind.WhileKeyword, expr.whileKeyword)
    assertTokenKind(SyntaxKind.OpenParenToken, expr.openParen)
    assertTrueExpr(expr.condition)
    assertTokenKind(SyntaxKind.CloseParenToken, expr.closeParen)
    assertNumberExpr(1, expr.body)
  }

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

  def functions(): unit = {
    functionWithNoParameters()
    functionWithParameters()
    functionWithReturnType()
  }

  def functionWithNoParameters(): unit = {
    val fn = mkFunctionMember("def f() = { 1 }")
    assertTokenKind(SyntaxKind.DefKeyword, fn.defKeyword)
    assertTokenText("f", fn.identifier)
    Assert.none(fn.genericParameters)
    assertTokenKind(SyntaxKind.OpenParenToken, fn.openParenToken)
    Assert.empty(fn.parameters)
    assertTokenKind(SyntaxKind.CloseParenToken, fn.closeParenToken)
    Assert.none(fn.typeAnnotation)

    val body = Assert.some(fn.body)
    assertTokenKind(SyntaxKind.EqualsToken, body.equalToken)
    val block = assertBlockExpr(body.expression)
    val expr = Assert.some(block.block.expression)
    assertNumberExpr(1, expr)
  }

  def functionWithParameters(): unit = {
    val fn = mkFunctionMember("def f(a: int, b: int) = { a + b }")
    assertTokenKind(SyntaxKind.DefKeyword, fn.defKeyword)
    assertTokenText("f", fn.identifier)
    Assert.none(fn.genericParameters)
    assertTokenKind(SyntaxKind.OpenParenToken, fn.openParenToken)

    val parameters = fn.parameters
    val a = Assert.index(0, parameters)
    assertTokenText("a", a.identifier)
    assertTokenKind(SyntaxKind.ColonToken, a.typeAnnotation.colonToken)
    assertName("int", a.typeAnnotation.typ)

    val b = Assert.index(1, parameters)
    assertTokenText("b", b.identifier)
    assertTokenKind(SyntaxKind.ColonToken, b.typeAnnotation.colonToken)
    assertName("int", b.typeAnnotation.typ)

    assertTokenKind(SyntaxKind.CloseParenToken, fn.closeParenToken)
    Assert.none(fn.typeAnnotation)

    val body = Assert.some(fn.body)
    assertTokenKind(SyntaxKind.EqualsToken, body.equalToken)
    val block = assertBlockExpr(body.expression)
    val expr = Assert.some(block.block.expression)
    val binary = assertBinaryExpr(expr)
    assertIdentifierExpr("a", binary.left)
    assertTokenKind(SyntaxKind.PlusToken, binary.operator)
    assertIdentifierExpr("b", binary.right)
  }

  def functionWithReturnType(): unit = {
    val fn = mkFunctionMember("def f(): int = { 1 }")
    assertTokenKind(SyntaxKind.DefKeyword, fn.defKeyword)
    assertTokenText("f", fn.identifier)
    Assert.none(fn.genericParameters)
    assertTokenKind(SyntaxKind.OpenParenToken, fn.openParenToken)
    Assert.empty(fn.parameters)
    assertTokenKind(SyntaxKind.CloseParenToken, fn.closeParenToken)

    val typeAnnotation = Assert.some(fn.typeAnnotation)
    assertTokenKind(SyntaxKind.ColonToken, typeAnnotation.colonToken)
    assertName("int", typeAnnotation.typ)

    val body = Assert.some(fn.body)
    assertTokenKind(SyntaxKind.EqualsToken, body.equalToken)
    val block = assertBlockExpr(body.expression)
    val expr = Assert.some(block.block.expression)
    assertNumberExpr(1, expr)
  }
}
