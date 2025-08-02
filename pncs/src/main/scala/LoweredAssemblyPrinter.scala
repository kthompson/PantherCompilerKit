import panther._

class LoweredAssemblyPrinter(
    binder: Binder,
    sb: IndentedStringBuilder
) {

  val ast = new AstPrinter(true, sb)
  val symbolPrinter = SymbolPrinter(binder, ast)

  override def toString(): string = sb.toString()

  def printAssembly(assembly: LoweredAssembly): unit = {
    ast.writeWithColor(ColorPalette.Brackets(0), "Lowered Assembly")
    ast.appendLine("")
    ast.indent()
    assembly.entryPoint match {
      case Option.None =>
      case Option.Some(value) =>
        ast.writeWithColor(ColorPalette.Brackets(1), "Entry Point")
        ast.appendLine("")
        ast.indent()
        symbolPrinter.printSimpleSymbol(value)
        ast.appendLine("")
        ast.unindent()
    }

    if (!assembly.functionBodies.list.isEmpty) {
      ast.writeWithColor(ColorPalette.Brackets(1), "Function Bodies:")
      ast.appendLine("")
      ast.indent()

      printFunctionBodies(assembly.functionBodies.list)
      ast.unindent()
    }

    ast.unindent()
    ast.appendLine("")
  }

  def printFunctionBodies(
      functionBodies: List[KeyValue[Symbol, LoweredBlock]]
  ): unit = {
    functionBodies match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        printFunctionBody(head.key, head.value)
        printFunctionBodies(tail)
    }
  }

  def printFunctionBody(
      symbol: Symbol,
      body: LoweredBlock
  ): unit = {

    symbolPrinter.printSimpleSymbol(symbol)
    ast.writeWithColor(ColorPalette.Punctuation, " = ")

    printBlock(body)
    ast.appendLine("")
  }

  def printBlock(block: LoweredBlock): unit = {
    if (block.statements.isEmpty()) {
      printExpression(block.expression)
    } else {
      ast.writeWithColor(ColorPalette.Punctuation, "{")
      sb.appendLine("")

      sb.indent()
      printStatements(block.statements)

      printExpression(block.expression)
      sb.appendLine("")

      sb.unindent()
      ast.writeWithColor(ColorPalette.Punctuation, "}")
      sb.appendLine("")
    }
  }

  def printStatements(value: Chain[LoweredStatement]): unit = {
    value.uncons() match {
      case Option.None => ()
      case Option.Some(Tuple2(head, tail)) =>
        printStatement(head)
        printStatements(tail)
    }
  }

  def printStatement(statement: LoweredStatement): unit = {
    statement match {
      case LoweredStatement.Error                 => printErrorStatement()
      case stmt: LoweredStatement.AssignField     => printAssignField(stmt)
      case stmt: LoweredStatement.AssignLocal     => printAssignLocal(stmt)
      case stmt: LoweredStatement.ConditionalGoto => printConditionalGoto(stmt)
      case stmt: LoweredStatement.ExpressionStatement =>
        printExpressionStatement(stmt)
      case stmt: LoweredStatement.Goto => printGoto(stmt)
      case stmt: LoweredStatement.LabelDeclaration =>
        printLabelDeclaration(stmt)

      case stmt: LoweredStatement.Return =>
        ???
    }
  }

  def printExpressionStatement(
      statement: LoweredStatement.ExpressionStatement
  ): unit = {
    printExpression(statement.expression)
    sb.appendLine("")
  }

  def printLabelDeclaration(
      declaration: LoweredStatement.LabelDeclaration
  ): unit = {
    ast.writeWithColor(ColorPalette.Keyword, "label ")
    ast.writeWithColor(ColorPalette.Identifier, declaration.label.name)
    sb.appendLine("")
  }

  def printGoto(goto: LoweredStatement.Goto): unit = {
    ast.writeWithColor(ColorPalette.Keyword, "goto ")
    ast.writeWithColor(ColorPalette.Identifier, goto.label.name)
    sb.appendLine("")
  }

  def printConditionalGoto(goto: LoweredStatement.ConditionalGoto): unit = {
    ast.writeWithColor(ColorPalette.Keyword, "goto ")
    ast.writeWithColor(ColorPalette.Identifier, goto.label.name)
    ast.writeWithColor(ColorPalette.Keyword, " if ")
    printExpression(goto.condition)
    sb.appendLine("")
  }

  def printAssignLocal(statement: LoweredStatement.AssignLocal): unit = {
    ast.writeWithColor(ColorPalette.Keyword, "var ")
//    writeWithColor(ColorPalette.Identifier, statement.local.name)
    symbolPrinter.printSimpleSymbol(statement.local)
    ast.writeWithColor(ColorPalette.Punctuation, " = ")
    printExpression(statement.expression)
    sb.appendLine("")
  }

  def printErrorStatement(): unit = {
    ast.writeWithColor(ColorPalette.Error, "Error")
    sb.appendLine("")
  }

  def printAssignField(statement: LoweredStatement.AssignField): unit = {
    printLeftHandSide(statement.receiver)
    ast.writeWithColor(ColorPalette.Punctuation, ".")
    symbolPrinter.printSimpleSymbol(statement.field)
    ast.writeWithColor(ColorPalette.Punctuation, " = ")
    printExpression(statement.expression)
    sb.appendLine("")
  }

  def printLeftHandSide(leftHandSide: LoweredLeftHandSide): unit = {
    leftHandSide match {
      case LoweredLeftHandSide.Variable(symbol) =>
        ast.writeWithColor(ColorPalette.Identifier, symbol.name)
      case left: LoweredLeftHandSide.MemberAccess =>
        printMemberAccess(left)
    }
  }

  def printMemberAccess(value: LoweredLeftHandSide.MemberAccess): unit = {
    printLeftHandSide(value.left)
    ast.writeWithColor(ColorPalette.Punctuation, ".")
    ast.append(value.symbol.name)
  }

  def printExpression(expression: LoweredExpression): unit = {
    expression match {
      case LoweredExpression.Error =>
        printError()
      case expr: LoweredExpression.BinaryExpression =>
        printBinaryExpression(expr)
      case expr: LoweredExpression.BooleanLiteral =>
        printBooleanLiteral(expr)
      case expr: LoweredExpression.Cast =>
        printCast(expr)
      case expr: LoweredExpression.Call =>
        printCall(expr)
      case expr: LoweredExpression.CharacterLiteral =>
        printCharacterLiteral(expr)
      case expr: LoweredExpression.IntegerLiteral =>
        printIntegerLiteral(expr)
      case expr: LoweredExpression.MemberAccess =>
        printMemberAccessExpression(expr)
      case expr: LoweredExpression.New =>
        printNew(expr)
      case expr: LoweredExpression.StringLiteral =>
        printStringLiteral(expr)
      case expr: LoweredExpression.Unary =>
        printUnary(expr)
      case LoweredExpression.Unit =>
        printUnit()
      case expr: LoweredExpression.Variable =>
        printVariable(expr)
    }
  }

  def printMemberAccessExpression(
      value: LoweredExpression.MemberAccess
  ): unit = {
    printLeftHandSide(value.left)
    ast.writeWithColor(ColorPalette.Punctuation, ".")
    ast.append(value.symbol.name)
  }

  def printNew(value: LoweredExpression.New): unit = {
    ast.writeWithColor(ColorPalette.Keyword, "new ")
    ast.append(value.constructor.parent.get().name)
    ast.writeWithColor(ColorPalette.Punctuation, "(")
    printExpressions(value.arguments.uncons())
    ast.writeWithColor(ColorPalette.Punctuation, ")")
  }

  def printVariable(expr: LoweredExpression.Variable): Unit = {
    ast.writeWithColor(ColorPalette.Identifier, expr.symbol.name)
  }

  def printUnit(): Unit = {
    ast.writeWithColor(ColorPalette.Keyword, "()")
  }

  def printUnary(expr: LoweredExpression.Unary): Unit = {
    ast.writeWithColor(
      ColorPalette.Punctuation,
      SyntaxFacts.getUnaryOperatorText(expr.operator)
    )
    ast.writeWithColor(ColorPalette.Punctuation, "(")
    printExpression(expr.operand)
    ast.writeWithColor(ColorPalette.Punctuation, ")")
  }

  def printStringLiteral(
      expr: LoweredExpression.StringLiteral
  ): Unit = {
    ast.writeWithColor(ColorPalette.String, "\"" + expr.value + "\"")
  }

  def printIntegerLiteral(
      expr: LoweredExpression.IntegerLiteral
  ): Unit = {
    ast.writeWithColor(ColorPalette.Number, expr.value.toString())
  }

  def printCharacterLiteral(
      expr: LoweredExpression.CharacterLiteral
  ): Unit = {
    ast.writeWithColor(ColorPalette.String, "'" + expr.value + "'")
  }

  def printCall(call: LoweredExpression.Call): unit = {
    ast.append(call.method.name)
    ast.writeWithColor(ColorPalette.Punctuation, "(")
    printExpressions(call.arguments.uncons())
    ast.writeWithColor(ColorPalette.Punctuation, ")")
  }

  def printExpressions(
      value: Option[Tuple2[LoweredExpression, Chain[LoweredExpression]]]
  ): unit = {
    value match {
      case Option.None => ()
      case Option.Some(Tuple2(value, tail)) =>
        printExpression(value)
        val chain = tail.uncons()
        if (!chain.isEmpty()) {
          ast.writeWithColor(ColorPalette.Punctuation, ", ")
          printExpressions(chain)
        }
    }
  }

  def printError(): unit = {
    ast.writeWithColor(ColorPalette.Error, "Error")
  }

  def printBinaryExpression(expr: LoweredExpression.BinaryExpression): unit = {
    printExpression(expr.left)
    ast.writeWithColor(
      ColorPalette.Punctuation,
      " " + SyntaxFacts.getBinaryOperatorText(expr.operator) + " "
    )
    printExpression(expr.right)
  }

  def printBooleanLiteral(expr: LoweredExpression.BooleanLiteral): unit = {
    ast.writeWithColor(ColorPalette.Keyword, expr.value.toString)
  }

  def printCast(expr: LoweredExpression.Cast): unit = {
    ast.printType(expr.resultType)
    ast.writeWithColor(ColorPalette.Punctuation, "(")
    printExpression(expr.operand)
    ast.writeWithColor(ColorPalette.Punctuation, ")")
  }

}
