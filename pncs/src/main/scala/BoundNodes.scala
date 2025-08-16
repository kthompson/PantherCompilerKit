import panther._

case class BoundAssembly(
    diagnostics: Diagnostics,
    functionBodies: Dictionary[Symbol, BoundExpression],
    entryPoint: Option[Symbol]
)

enum BoundDefinition {
  case Object(symbol: Symbol, members: List[BoundMember])
  case Class(symbol: Symbol, members: List[BoundMember])
  //  case Enum(symbol: string, members: Array[String])
}

enum BoundMember {
  case Field(symbol: Symbol, typ: Type)
  case Method(
      symbol: Symbol,
      parameters: List[BoundParameter],
      returnType: Type,
      body: BoundExpression
  )
}

case class BoundParameter(symbol: Symbol, typ: Type)

enum BoundStatement {
  case Error
  case ExpressionStatement(expression: BoundExpression)
  case VariableDeclaration(
      variable: Symbol,
      isReadOnly: bool,
      typ: Type,
      initializer: BoundExpression
  )
}

enum BoundLeftHandSide {
  case Call(expression: BoundExpression.CallExpression)
  case Index(expression: BoundExpression.IndexExpression)
  case MemberAccess(expression: BoundExpression.MemberAccess)
  case New(expression: BoundExpression.NewExpression)
  case Variable(location: TextLocation, symbol: Symbol)
}

enum BoundLiteral {
  case IntLiteral(location: TextLocation, value: int)
  case StringLiteral(location: TextLocation, value: string)
  case BoolLiteral(location: TextLocation, value: bool)
  case CharLiteral(location: TextLocation, value: char)
}

enum BoundPattern {
  case Literal(literal: BoundLiteral)
  case Variable(symbol: Symbol)
  case Discard
}

case class BoundMatchCase(
    location: TextLocation,
    pattern: BoundPattern,
    result: BoundExpression
)

enum BoundExpression {
  case Error(message: string)
  case Assignment(
      location: TextLocation,
      receiver: BoundLeftHandSide,
      expression: BoundExpression
  )
  case BinaryExpression(
      location: TextLocation,
      left: BoundExpression,
      operator: BinaryOperatorKind,
      right: BoundExpression,
      resultType: Type
  )
  case Block(statements: List[BoundStatement], expression: BoundExpression)
  case BooleanLiteral(location: TextLocation, value: bool)
  case CallExpression(
      location: TextLocation,
      receiver: Option[BoundLeftHandSide],
      method: Symbol,
      genericArguments: List[Type],
      arguments: List[BoundExpression],
      resultType: Type
  )
  case CastExpression(
      location: TextLocation,
      expression: BoundExpression,
      targetType: Type
  )
  case CharacterLiteral(location: TextLocation, value: char)
  case ForExpression(
      location: TextLocation,
      variable: Symbol,
      lowerBound: BoundExpression,
      upperBound: BoundExpression,
      body: BoundExpression
  )
  case IfExpression(
      location: TextLocation,
      cond: BoundExpression,
      thenExpr: BoundExpression,
      elseExpr: Option[BoundExpression],
      resultType: Type
  )
  case IndexExpression(
      location: TextLocation,
      array: BoundExpression,
      index: BoundExpression,
      resultType: Type
  )
  case IntLiteral(location: TextLocation, value: int)
  case MemberAccess(
      location: TextLocation,
      receiver: BoundLeftHandSide,
      member: Symbol,
      genericArguments: List[Type],
      resultType: Type
  )
  case NewExpression(
      location: TextLocation,
      constructor: Symbol,
      genericArguments: List[Type],
      arguments: List[BoundExpression],
      resultType: Type
  )
  case StringLiteral(location: TextLocation, value: string)
  case UnaryExpression(
      location: TextLocation,
      operator: UnaryOperatorKind,
      operand: BoundExpression,
      resultType: Type
  )
  case UnitExpression(location: TextLocation)
  case Variable(
      location: TextLocation,
      symbol: Symbol,
      resultType: Option[Type]
  )
  case WhileExpression(
      location: TextLocation,
      condition: BoundExpression,
      body: BoundExpression
  )
  case MatchExpression(
      location: TextLocation,
      resultType: Type,
      expression: BoundExpression,
      cases: NonEmptyList[BoundMatchCase]
  )
}
