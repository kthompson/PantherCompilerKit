import panther._

class BoundAssemblyPrinter(binder: Binder) {
  val astPrinter = new AstPrinter(true, true)
  val symbolPrinter = new SymbolPrinter(binder)

  var indentString = "  "

  def indent(): unit = {
    indentString = indentString + "  "
  }

  def unindent(): unit = {
    if (indentString.length >= 2) {
      indentString = indentString.substring(0, indentString.length - 2)
    }
  }

  def writeIndent(): unit = {
    print(indentString)
  }

  def writeWithColor(color: string, text: string): unit = {
    astPrinter.writeColor(color)
    astPrinter.write(text)
    astPrinter.writeClear()
  }

  def printAssembly(assembly: BoundAssembly): Unit = {
    println("=== Bound Assembly ===")
    assembly.entryPoint match {
      case Option.None =>
      case Option.Some(value) =>
        print("Entry Point ")
        symbolPrinter.printSimpleSymbol(value)
        println()
    }

    if (!assembly.functionBodies.list.isEmpty) {
      println("Function Bodies:")
      indent()

      printFunctionBodies(assembly.functionBodies.list)
      unindent()
    }
  }

  def printFunctionBodies(
      functionBodies: List[KeyValue[Symbol, BoundExpression]]
  ): Unit = {
    functionBodies match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        printFunctionBody(head.key, head.value)
        printFunctionBodies(tail)
    }
  }

  def printFunctionBody(
      symbol: Symbol,
      body: BoundExpression
  ): unit = {
    val printer = new AstPrinter(true, true)
    symbolPrinter.printSimpleSymbol(symbol)
    writeWithColor(ColorPalette.Punctuation, " = ")
    printExpression(body)
    unindent()
    println()
  }

  def printExpression(
      expression: BoundExpression
  ): unit = {
    expression match {
      case expr: BoundExpression.Error            => printError(expr)
      case expr: BoundExpression.Assignment       => printAssignment(expr)
      case expr: BoundExpression.BinaryExpression => printBinaryExpression(expr)
      case expr: BoundExpression.Block            => printBlock(expr)
      case expr: BoundExpression.BooleanLiteral   => printBooleanLiteral(expr)
      case expr: BoundExpression.CallExpression   => printCallExpression(expr)
      case expr: BoundExpression.CastExpression   => printCastExpression(expr)
      case expr: BoundExpression.CharacterLiteral => printCharacterLiteral(expr)
      case expr: BoundExpression.ForExpression    => printForExpression(expr)
      case expr: BoundExpression.IfExpression     => printIfExpression(expr)
      case expr: BoundExpression.IndexExpression  => printIndexExpression(expr)
      case expr: BoundExpression.IntLiteral       => printIntLiteral(expr)
      case expr: BoundExpression.MemberAccess     => printMemberAccess(expr)
      case expr: BoundExpression.NewExpression    => printNewExpression(expr)
      case expr: BoundExpression.StringLiteral    => printStringLiteral(expr)
      case expr: BoundExpression.UnaryExpression  => printUnaryExpression(expr)
      case expr: BoundExpression.UnitExpression   => printUnitExpression(expr)
      case expr: BoundExpression.Variable         => printVariable(expr)
      case expr: BoundExpression.WhileExpression  => printWhileExpression(expr)
    }
  }

  def printError(expr: BoundExpression.Error): unit = ???
  def printAssignment(expr: BoundExpression.Assignment): unit = {
    writeIndent()
    writeWithColor(ColorPalette.Identifier, expr.variable.name)
    writeWithColor(ColorPalette.Punctuation, " = ")
    printExpression(expr.expression)
  }
  def printBinaryExpression(expr: BoundExpression.BinaryExpression): unit = {
    printExpression(expr.left)
    writeWithColor(ColorPalette.Punctuation, " ")
    writeWithColor(
      ColorPalette.Punctuation,
      SyntaxFacts.getBinaryOperatorText(expr.operator)
    )
    writeWithColor(ColorPalette.Punctuation, " ")
    printExpression(expr.right)
  }
  def printBlock(expr: BoundExpression.Block): unit = {
    writeIndent()
    writeWithColor(ColorPalette.Punctuation, "{")
    println()

    printStatements(expr.statements)

    writeIndent()
    writeWithColor(ColorPalette.Punctuation, "}")
    println()
  }

  def printBooleanLiteral(expr: BoundExpression.BooleanLiteral): unit =
    writeWithColor(
      ColorPalette.Keyword,
      if (expr.value) "true" else "false"
    )
  def printCallExpression(expr: BoundExpression.CallExpression): unit = {
    printExpression(expr.method)
    writeWithColor(ColorPalette.Punctuation, "(")

    if (!expr.genericArguments.isEmpty) {
      writeWithColor(ColorPalette.Punctuation, "<")
      astPrinter._printTypes(expr.genericArguments, ", ", true)
      writeWithColor(ColorPalette.Punctuation, ">")
    }

    writeWithColor(ColorPalette.Punctuation, "(")
    printExpressions(expr.arguments)
    writeWithColor(ColorPalette.Punctuation, ")")
  }
  def printCastExpression(expr: BoundExpression.CastExpression): unit = {
    writeWithColor(ColorPalette.Keyword, expr.targetType.toString())
    writeWithColor(ColorPalette.Punctuation, "(")
    printExpression(expr.expression)
    writeWithColor(ColorPalette.Punctuation, ")")
  }
  def printCharacterLiteral(expr: BoundExpression.CharacterLiteral): unit =
    writeWithColor(ColorPalette.String, "'" + expr.value + "'")
  def printForExpression(expr: BoundExpression.ForExpression): unit = ???
  def printIfExpression(expr: BoundExpression.IfExpression): unit = {
    writeWithColor(ColorPalette.Keyword, "if")
    writeWithColor(ColorPalette.Punctuation, " (")
    printExpression(expr.cond)
    writeWithColor(ColorPalette.Punctuation, ") ")

    printExpression(expr.thenExpr)

    expr.elseExpr match {
      case Option.None => ()
      case Option.Some(elseExpr) =>
        writeWithColor(ColorPalette.Keyword, " else ")
        printExpression(elseExpr)
    }
  }
  def printIndexExpression(expr: BoundExpression.IndexExpression): unit = ???
  def printIntLiteral(expr: BoundExpression.IntLiteral): unit = {
    writeWithColor(ColorPalette.Number, string(expr.value))
  }
  def printMemberAccess(expr: BoundExpression.MemberAccess): unit = {
    printExpression(expr.left)
    writeWithColor(ColorPalette.Punctuation, ".")
    writeWithColor(ColorPalette.Identifier, expr.member.name)
  }
  def printNewExpression(expr: BoundExpression.NewExpression): unit = {
    writeWithColor(ColorPalette.Keyword, "new")
    print(" ")
    writeWithColor(ColorPalette.Identifier, expr.constructor.parent.get().name)
    writeWithColor(ColorPalette.Punctuation, "(")
    printExpressions(expr.arguments)
    writeWithColor(ColorPalette.Punctuation, ")")
  }

  def printExpressions(expressions: List[BoundExpression]): unit = {
    expressions match {
      case List.Nil =>
      case List.Cons(head, tail) =>
        printExpression(head)
        tail match {
          case List.Nil =>
          case _ =>
            writeWithColor(ColorPalette.Punctuation, ", ")
            printExpressions(tail)
        }
    }
  }

  def printStringLiteral(expr: BoundExpression.StringLiteral): unit = {
    writeWithColor(ColorPalette.String, "\"" + expr.value + "\"")
  }

  def printUnaryExpression(expr: BoundExpression.UnaryExpression): unit = {
    writeWithColor(
      ColorPalette.Punctuation,
      SyntaxFacts.getUnaryOperatorText(expr.operator)
    )
    printExpression(expr.operand)
  }

  def printUnitExpression(expr: BoundExpression.UnitExpression): unit =
    writeWithColor(ColorPalette.Punctuation, "()")

  def printVariable(expr: BoundExpression.Variable): unit = {
    writeWithColor(ColorPalette.Identifier, expr.symbol.name)
  }

  def printWhileExpression(expr: BoundExpression.WhileExpression): unit = {
    writeWithColor(ColorPalette.Keyword, "while")
    writeWithColor(ColorPalette.Punctuation, " (")
    printExpression(expr.condition)
    writeWithColor(ColorPalette.Punctuation, ") ")

    printExpression(expr.body)
  }

  def printStatements(value: List[BoundStatement]): unit = {
    value match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        printBoundStatement(head)
        printStatements(tail)
    }
  }

  def printBoundStatement(statement: BoundStatement): unit = {
    writeIndent()
    statement match {
      case BoundStatement.Error => printErrorStatement()
      case stmt: BoundStatement.ExpressionStatement =>
        printExpressionStatement(stmt)
      case stmt: BoundStatement.VariableDeclaration =>
        printVariableDeclaration(stmt)
    }
  }

  def printErrorStatement(): unit = {
    writeIndent()
    writeWithColor(ColorPalette.Error, "Error")
    println()
  }

  def printExpressionStatement(
      statement: BoundStatement.ExpressionStatement
  ): unit = {
    writeIndent()
    printExpression(statement.expression)
    println()
  }

  def printVariableDeclaration(
      statement: BoundStatement.VariableDeclaration
  ): unit = {
    writeIndent()
    writeWithColor(ColorPalette.Keyword, "var")
    print(" ")
    writeWithColor(ColorPalette.Identifier, statement.variable.name)

    writeWithColor(ColorPalette.Punctuation, " = ")
    printExpression(statement.initializer)
    println()
  }
}
