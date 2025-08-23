import panther._

class BoundAssemblyPrinter(
    binder: Binder,
    ast: AstPrinter,
    symbolPrinter: SymbolPrinter
) {

  def writeWithColor(color: string, text: string): unit = {
    ast.writeWithColor(color, text)
  }

  def printAssembly(assembly: BoundAssembly): unit = {
    ast.writeWithColor(ColorPalette.Brackets(0), "Bound Assembly")
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
      functionBodies: List[KeyValue[Symbol, BoundExpression]]
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
      body: BoundExpression
  ): unit = {
    symbolPrinter.printSimpleSymbol(symbol)
    writeWithColor(ColorPalette.Punctuation, " = ")
    printExpression(body)
    ast.appendLine("")
  }

  def printExpression(
      expression: BoundExpression
  ): unit = {
    expression match {
      case expr: BoundExpression.Error            => printError(expr)
      case expr: BoundExpression.Assignment       => printAssignment(expr)
      case expr: BoundExpression.BinaryExpression => printBinaryExpression(expr)
      case expr: BoundExpression.Block            => printBlock(expr)
      case expr: BoundExpression.Boolean          => printBooleanLiteral(expr)
      case expr: BoundExpression.CallExpression   => printCallExpression(expr)
      case expr: BoundExpression.CastExpression   => printCastExpression(expr)
      case expr: BoundExpression.Character        => printCharacterLiteral(expr)
      case expr: BoundExpression.ForExpression    => printForExpression(expr)
      case expr: BoundExpression.IfExpression     => printIfExpression(expr)
      case expr: BoundExpression.IndexExpression  => printIndexExpression(expr)
      case expr: BoundExpression.Int              => printIntLiteral(expr)
      case expr: BoundExpression.IsExpression     => printIsExpression(expr)
      case expr: BoundExpression.MemberAccess     => printMemberAccess(expr)
      case expr: BoundExpression.NewExpression    => printNewExpression(expr)
      case expr: BoundExpression.String           => printStringLiteral(expr)
      case expr: BoundExpression.UnaryExpression  => printUnaryExpression(expr)
      case expr: BoundExpression.UnitExpression   => printUnitExpression(expr)
      case expr: BoundExpression.Variable         => printVariable(expr)
      case expr: BoundExpression.WhileExpression  => printWhileExpression(expr)
      case expr: BoundExpression.MatchExpression  => printMatchExpression(expr)
    }
  }

  def printError(expr: BoundExpression.Error): unit = ???
  def printAssignment(expr: BoundExpression.Assignment): unit = {
    printLeftHandSide(expr.receiver)
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

  def printLeftHandSide(lhs: BoundLeftHandSide): unit = {
    lhs match {
      case BoundLeftHandSide.Index(expression) =>
        printIndexExpression(expression)
      case BoundLeftHandSide.MemberAccess(memberAccess) =>
        printMemberAccess(memberAccess)
      case BoundLeftHandSide.Variable(location, variable) =>
        if (variable.isStatic()) {
          writeWithColor(ColorPalette.Identifier, variable.qualifiedName())
        } else {
          writeWithColor(ColorPalette.Identifier, variable.name)
        }
      case BoundLeftHandSide.Call(expression) =>
        printCallExpression(expression)
      case BoundLeftHandSide.New(expression) =>
        printNewExpression(expression)
    }
  }

  def printBlock(expr: BoundExpression.Block): unit = {
    writeWithColor(ColorPalette.Punctuation, "{")
    ast.indent()
    ast.appendLine("")

    printStatements(expr.statements)

    ast.unindent()
    writeWithColor(ColorPalette.Punctuation, "}")
    ast.appendLine("")
  }

  def printBooleanLiteral(expr: BoundExpression.Boolean): unit =
    writeWithColor(
      ColorPalette.Keyword,
      if (expr.value) "true" else "false"
    )
  def printCallExpression(expr: BoundExpression.CallExpression): unit = {
    expr.receiver match {
      case Option.None =>
        writeWithColor(ColorPalette.Identifier, expr.method.name)

      case Option.Some(receiver) =>
        printLeftHandSide(receiver)
        writeWithColor(ColorPalette.Punctuation, ".")
        writeWithColor(ColorPalette.Identifier, expr.method.name)
    }

    if (!expr.genericArguments.isEmpty) {
      writeWithColor(ColorPalette.Punctuation, "<")
      ast._printTypes(expr.genericArguments, ", ", true)
      writeWithColor(ColorPalette.Punctuation, ">")
    }

    writeWithColor(ColorPalette.Punctuation, "(")
    printExpressions(expr.arguments)
    writeWithColor(ColorPalette.Punctuation, ")")
  }
  def printCastExpression(expr: BoundExpression.CastExpression): unit = {
    ast._printType(expr.targetType, true)
    writeWithColor(ColorPalette.Punctuation, "(")
    printExpression(expr.expression)
    writeWithColor(ColorPalette.Punctuation, ")")
  }
  def printCharacterLiteral(expr: BoundExpression.Character): unit =
    writeWithColor(ColorPalette.String, "'" + expr.value + "'")
  def printIsExpression(expr: BoundExpression.IsExpression): unit = {
    printExpression(expr.expression)
    writeWithColor(ColorPalette.Punctuation, " ")
    writeWithColor(ColorPalette.Keyword, "is")
    writeWithColor(ColorPalette.Punctuation, " ")
    ast._printType(expr.targetType, true)
  }
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
  def printIntLiteral(expr: BoundExpression.Int): unit = {
    writeWithColor(ColorPalette.Number, string(expr.value))
  }
  def printMemberAccess(expr: BoundExpression.MemberAccess): unit = {
    printLeftHandSide(expr.receiver)
    writeWithColor(ColorPalette.Punctuation, ".")
    writeWithColor(ColorPalette.Identifier, expr.member.name)
  }
  def printNewExpression(expr: BoundExpression.NewExpression): unit = {
    writeWithColor(ColorPalette.Keyword, "new")
    ast.append(" ")
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

  def printStringLiteral(expr: BoundExpression.String): unit = {
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
    statement match {
      case BoundStatement.Error => printErrorStatement()
      case stmt: BoundStatement.ExpressionStatement =>
        printExpressionStatement(stmt)
      case stmt: BoundStatement.VariableDeclaration =>
        printVariableDeclaration(stmt)
    }
  }

  def printErrorStatement(): unit = {
    writeWithColor(ColorPalette.Error, "Error")
    ast.appendLine("")
  }

  def printExpressionStatement(
      statement: BoundStatement.ExpressionStatement
  ): unit = {
    printExpression(statement.expression)
    ast.appendLine("")
  }

  def printVariableDeclaration(
      statement: BoundStatement.VariableDeclaration
  ): unit = {
    writeWithColor(ColorPalette.Keyword, "var")
    ast.append(" ")
    writeWithColor(ColorPalette.Identifier, statement.variable.name)

    writeWithColor(ColorPalette.Punctuation, " = ")
    printExpression(statement.initializer)
    ast.appendLine("")
  }

  def printMatchExpression(expr: BoundExpression.MatchExpression): unit = {
    printExpression(expr.expression)
    writeWithColor(ColorPalette.Keyword, "match ")
    writeWithColor(ColorPalette.Punctuation, " {")
    ast.appendLine("")
    ast.indent()
    printMatchCase(expr.cases.head)
    printMatchCases(expr.cases.tail)
    ast.unindent()
    writeWithColor(ColorPalette.Punctuation, "}")
  }

  def printMatchCases(value: List[BoundMatchCase]): unit = {
    value match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        printMatchCase(head)
        printMatchCases(tail)
    }
  }

  def printMatchCase(matchCase: BoundMatchCase): unit = {
    writeWithColor(ColorPalette.Keyword, "case ")
    printPattern(matchCase.pattern)
    writeWithColor(ColorPalette.Punctuation, " => ")
    printExpression(matchCase.result)
    ast.appendLine("")
  }

  def printPattern(pattern: BoundPattern): unit = pattern match {
    case BoundPattern.Literal(lit) => printBoundLiteral(lit)
    case BoundPattern.Variable(symbol) =>
      writeWithColor(ColorPalette.Identifier, symbol.name)
    case BoundPattern.Discard => writeWithColor(ColorPalette.Punctuation, "_")
  }

  def printBoundLiteral(lit: BoundLiteral): unit = lit match {
    case BoundLiteral.Int(location, value) =>
      writeWithColor(ColorPalette.Number, value.toString())
    case BoundLiteral.String(location, value) =>
      writeWithColor(ColorPalette.String, "\"" + value + "\"")
    case BoundLiteral.Bool(location, value) =>
      writeWithColor(ColorPalette.Keyword, value.toString())
    case BoundLiteral.Char(location, value) =>
      writeWithColor(ColorPalette.String, "'" + value.toString() + "'")
  }
}
