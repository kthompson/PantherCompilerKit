import panther._

case class LoweredAssembly(
    diagnostics: Diagnostics,
    functionBodies: Dictionary[Symbol, LoweredBlock],
    entryPoint: Option[Symbol]
)

case class Label(name: String)

enum LoweredStatement {
  case Error
  case Assignment(
      location: TextLocation,
      variable: Symbol,
      expression: LoweredExpression
  )
  case ConditionalGoto(
      location: TextLocation,
      label: Label,
      condition: LoweredExpression
  )

  // is this needed?
  case ExpressionStatement(
      location: TextLocation,
      expression: LoweredExpression
  )
  case Goto(location: TextLocation, label: Label)
  case LabelDeclaration(location: TextLocation, label: Label)
  case Return(location: TextLocation, expression: LoweredExpression)
  case VariableDeclaration(
      location: TextLocation,
      variable: Symbol,
      isReadOnly: Boolean,
      typ: Type,
      initializer: LoweredExpression
  )
}

enum LoweredExpression {
  case Error
  case BinaryExpression(
      location: TextLocation,
      left: LoweredExpression,
      operator: BinaryOperatorKind,
      right: LoweredExpression,
      resultType: Type
  )
  case BooleanLiteral(location: TextLocation, value: bool)
  case Cast(
      location: TextLocation,
      operand: LoweredExpression,
      resultType: Type
  )
  case CharacterLiteral(location: TextLocation, value: char)
  case IntegerLiteral(location: TextLocation, value: int)
  case Variable(location: TextLocation, variable: Symbol)
  case StringLiteral(location: TextLocation, value: string)
  case Unary(
      location: TextLocation,
      operand: LoweredExpression,
      operator: UnaryOperatorKind,
      resultType: Type
  )
  case Unit
}

case class LoweredBlock(
    statements: Chain[LoweredStatement],
    expression: LoweredExpression
)

object Lower {
  def lower(boundAssembly: BoundAssembly): LoweredAssembly = {
    val diagnostics = boundAssembly.diagnostics

    val entryPoint = boundAssembly.entryPoint

    val functionBodies = lowerFunctionBodies(
      boundAssembly.functionBodies,
      DictionaryModule.empty[Symbol, LoweredBlock]()
    )

    LoweredAssembly(diagnostics, functionBodies, entryPoint)
  }

  def lowerFunctionBodies(
      bodies: Dictionary[Symbol, BoundExpression],
      lowered: Dictionary[Symbol, LoweredBlock]
  ): Dictionary[Symbol, LoweredBlock] = {
    bodies.list match {
      case List.Nil => lowered
      case List.Cons(KeyValue(symbol, expr), tail) =>
        val loweredBody = lowerFunctionBody(symbol, expr)
        val updatedLowered = lowered.put(symbol, loweredBody)
        lowerFunctionBodies(Dictionary(tail), updatedLowered)
    }
  }

  def lowerFunctionBody(
      symbol: Symbol,
      body: BoundExpression
  ): LoweredBlock = {
    val context = LoweredBlock(Chain.Empty, LoweredExpression.Unit)
    val lowerer = new ExpressionLowerer(symbol)
    lowerer.lowerExpression(body, context)
  }

}

class ExpressionLowerer(symbol: Symbol) {

  def lowerExpression(
      expression: BoundExpression,
      context: LoweredBlock
  ): LoweredBlock = {
    expression match {
      case BoundExpression.Error            => ???
      case expr: BoundExpression.Assignment => lowerAssignment(expr, context)
      case expr: BoundExpression.BinaryExpression =>
        lowerBinaryExpression(expr, context)
      case expr: BoundExpression.Block => lowerBlock(expr, context)
      case expr: BoundExpression.BooleanLiteral =>
        lowerBooleanLiteral(expr, context)
      case expr: BoundExpression.CallExpression =>
        lowerCallExpression(expr, context)
      case expr: BoundExpression.CastExpression =>
        lowerCastExpression(expr, context)
      case expr: BoundExpression.CharacterLiteral =>
        lowerCharacterLiteral(expr, context)
      case expr: BoundExpression.ForExpression =>
        lowerForExpression(expr, context)
      case expr: BoundExpression.IfExpression =>
        lowerIfExpression(expr, context)
      case expr: BoundExpression.IndexExpression =>
        lowerIndexExpression(expr, context)
      case expr: BoundExpression.IntLiteral => lowerIntLiteral(expr, context)
      case expr: BoundExpression.MemberAccess =>
        lowerMemberAccess(expr, context)
      case expr: BoundExpression.NewExpression =>
        lowerNewExpression(expr, context)
      case expr: BoundExpression.StringLiteral =>
        lowerStringLiteral(expr, context)
      case expr: BoundExpression.UnaryExpression =>
        lowerUnaryExpression(expr, context)
      case expr: BoundExpression.UnitExpression =>
        lowerUnitExpression(expr, context)
      case expr: BoundExpression.Variable => lowerVariable(expr, context)
      case expr: BoundExpression.WhileExpression =>
        lowerWhileExpression(expr, context)
    }
  }

  def lowerAssignment(
      expr: BoundExpression.Assignment,
      context: LoweredBlock
  ): LoweredBlock = {

    val variable = expr.variable
    val location = expr.location
    val block = lowerExpression(expr.expression, context)

    val assignmentStatement = LoweredStatement.Assignment(
      location,
      variable,
      block.expression
    )

    val newStatements =
      ChainModule.append(block.statements, assignmentStatement)

    LoweredBlock(newStatements, LoweredExpression.Unit)
  }

  def lowerBinaryExpression(
      expr: BoundExpression.BinaryExpression,
      context: LoweredBlock
  ): LoweredBlock = {
    consumeExpr(
      lowerExpression(expr.left, context)
    ) match {
      case Tuple2(left, leftBlock) =>
        val right = lowerExpression(expr.right, leftBlock)

        val binary = LoweredExpression.BinaryExpression(
          expr.location,
          left,
          expr.operator,
          right.expression,
          expr.resultType
        )

        LoweredBlock(right.statements, binary)
    }
  }

  def lowerBlock(
      expr: BoundExpression.Block,
      context: LoweredBlock
  ): LoweredBlock = {
    val block = lowerStatements(expr.statements, context)
    lowerExpression(expr.expression, block)
  }

  def lowerBooleanLiteral(
      expr: BoundExpression.BooleanLiteral,
      context: LoweredBlock
  ): LoweredBlock = {
    val lowered = LoweredExpression.BooleanLiteral(
      expr.location,
      expr.value
    )

    checkUnusedExpr(context)

    LoweredBlock(context.statements, lowered)
  }

  def lowerCallExpression(
      expr: BoundExpression.CallExpression,
      context: LoweredBlock
  ): LoweredBlock = ???

  def lowerCastExpression(
      expr: BoundExpression.CastExpression,
      context: LoweredBlock
  ): LoweredBlock = {

    val block = lowerExpression(expr.expression, context)

    // TODO: probably should improve this more so we know the exact cast from and to
    val casted = LoweredExpression.Cast(
      expr.location,
      block.expression,
      expr.targetType
    )

    LoweredBlock(block.statements, casted)
  }

  def lowerCharacterLiteral(
      expr: BoundExpression.CharacterLiteral,
      context: LoweredBlock
  ): LoweredBlock = {
    val lowered = LoweredExpression.CharacterLiteral(
      expr.location,
      expr.value
    )

    checkUnusedExpr(context)

    LoweredBlock(context.statements, lowered)
  }

  def lowerForExpression(
      expr: BoundExpression.ForExpression,
      context: LoweredBlock
  ): LoweredBlock = ???

  def lowerIfExpression(
      expr: BoundExpression.IfExpression,
      context: LoweredBlock
  ): LoweredBlock = ???

  def lowerIndexExpression(
      expr: BoundExpression.IndexExpression,
      context: LoweredBlock
  ): LoweredBlock = ???

  def lowerIntLiteral(
      expr: BoundExpression.IntLiteral,
      context: LoweredBlock
  ): LoweredBlock = {
    val lowered = LoweredExpression.IntegerLiteral(
      expr.location,
      expr.value
    )

    checkUnusedExpr(context)

    LoweredBlock(context.statements, lowered)
  }

  def lowerMemberAccess(
      expr: BoundExpression.MemberAccess,
      context: LoweredBlock
  ): LoweredBlock = ???

  def lowerNewExpression(
      expr: BoundExpression.NewExpression,
      context: LoweredBlock
  ): LoweredBlock = ???

  def lowerStringLiteral(
      expr: BoundExpression.StringLiteral,
      context: LoweredBlock
  ): LoweredBlock = {
    val lowered = LoweredExpression.StringLiteral(
      expr.location,
      expr.value
    )

    checkUnusedExpr(context)

    LoweredBlock(context.statements, lowered)
  }

  def lowerUnaryExpression(
      expr: BoundExpression.UnaryExpression,
      context: LoweredBlock
  ): LoweredBlock = {
    val block = lowerExpression(expr.operand, context)

    val lowered = LoweredExpression.Unary(
      expr.location,
      block.expression,
      expr.operator,
      expr.resultType
    )

    LoweredBlock(block.statements, lowered)
  }

  def lowerUnitExpression(
      expr: BoundExpression.UnitExpression,
      context: LoweredBlock
  ): LoweredBlock = {
    checkUnusedExpr(context)
    context
  }

  def lowerVariable(
      expr: BoundExpression.Variable,
      context: LoweredBlock
  ): LoweredBlock = {
    checkUnusedExpr(context)

    val lowered = LoweredExpression.Variable(expr.location, expr.symbol)
    LoweredBlock(context.statements, lowered)
  }

  def lowerWhileExpression(
      expr: BoundExpression.WhileExpression,
      context: LoweredBlock
  ): LoweredBlock = ???

  def lowerStatements(
      statements: List[BoundStatement],
      context: LoweredBlock
  ): LoweredBlock = {
    statements match {
      case List.Nil => context
      case List.Cons(head, tail) =>
        val block = lowerStatement(head, context)
        lowerStatements(tail, block)
    }
  }

  def lowerStatement(
      statement: BoundStatement,
      context: LoweredBlock
  ): LoweredBlock = {
    statement match {
      case BoundStatement.Error => ???
      case statement: BoundStatement.ExpressionStatement =>
        lowerExpressionStatement(statement, context)
      case statement: BoundStatement.VariableDeclaration =>
        lowerVariableDeclaration(statement, context)
    }
  }

  def lowerExpressionStatement(
      statement: BoundStatement.ExpressionStatement,
      context: LoweredBlock
  ): LoweredBlock =
    lowerExpression(statement.expression, context)

  def lowerVariableDeclaration(
      statement: BoundStatement.VariableDeclaration,
      context: LoweredBlock
  ): LoweredBlock = ???

  def checkUnusedExpr(context: LoweredBlock): unit = {
    if (context.expression != LoweredExpression.Unit) {
      panic("Unused expression in lowered block")
    }
  }

  def consumeExpr(
      context: LoweredBlock
  ): Tuple2[LoweredExpression, LoweredBlock] = {
    val expr = context.expression
    Tuple2(expr, LoweredBlock(context.statements, LoweredExpression.Unit))
  }
}
