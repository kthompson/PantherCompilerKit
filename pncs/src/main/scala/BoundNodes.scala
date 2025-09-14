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
  case ArrayCreation(expression: BoundExpression.ArrayCreation)
  case Call(expression: BoundExpression.Call)
  case Index(expression: BoundExpression.Index)
  case MemberAccess(expression: BoundExpression.MemberAccess)
  case New(expression: BoundExpression.New)
  case Variable(location: TextLocation, symbol: Symbol)
}

enum BoundLiteral {
  case Int(location: TextLocation, value: int)
  case String(location: TextLocation, value: string)
  case Bool(location: TextLocation, value: bool)
  case Char(location: TextLocation, value: char)
}

enum BoundPattern {
  case Literal(literal: BoundLiteral)
  case Variable(symbol: Symbol)
  case Extract(
      constructor: Symbol,
      patterns: Array[BoundPattern]
  )
  case Discard
}

case class BoundMatchCase(
    location: TextLocation,
    pattern: BoundPattern,
    result: BoundExpression
)

enum BoundExpression {
  case Error(message: string)
  case ArrayCreation(
      location: TextLocation,
      elementType: Type,
      sizeExpression: BoundExpression,
      resultType: Type
  )
  case Assignment(
      location: TextLocation,
      receiver: BoundLeftHandSide,
      expression: BoundExpression
  )
  case Binary(
      location: TextLocation,
      left: BoundExpression,
      operator: BinaryOperatorKind,
      right: BoundExpression,
      resultType: Type
  )
  case Block(statements: List[BoundStatement], expression: BoundExpression)
  case Boolean(location: TextLocation, value: bool)
  case Call(
      location: TextLocation,
      receiver: Option[BoundLeftHandSide],
      method: Symbol,
      genericArguments: List[Type],
      arguments: List[BoundExpression],
      resultType: Type
  )
  case Cast(
      location: TextLocation,
      expression: BoundExpression,
      targetType: Type
  )
  case Character(location: TextLocation, value: char)
  case For(
      location: TextLocation,
      variable: Symbol,
      lowerBound: BoundExpression,
      upperBound: BoundExpression,
      body: BoundExpression
  )
  case If(
      location: TextLocation,
      cond: BoundExpression,
      thenExpr: BoundExpression,
      elseExpr: Option[BoundExpression],
      resultType: Type
  )
  case Index(
      location: TextLocation,
      array: BoundExpression,
      index: BoundExpression,
      resultType: Type
  )
  case Int(location: TextLocation, value: int)
  case Is(
      location: TextLocation,
      expression: BoundExpression,
      targetType: Type
  )
  case Match(
      location: TextLocation,
      resultType: Type,
      expression: BoundExpression,
      cases: NonEmptyList[BoundMatchCase]
  )
  case MemberAccess(
      location: TextLocation,
      receiver: BoundLeftHandSide,
      member: Symbol,
      genericArguments: List[Type],
      resultType: Type
  )
  case New(
      location: TextLocation,
      constructor: Symbol,
      genericArguments: List[Type],
      arguments: List[BoundExpression],
      resultType: Type
  )
  case String(location: TextLocation, value: string)
  case Unary(
      location: TextLocation,
      operator: UnaryOperatorKind,
      operand: BoundExpression,
      resultType: Type
  )
  case Unit(location: TextLocation)
  case Variable(
      location: TextLocation,
      symbol: Symbol,
      resultType: Option[Type]
  )
  case While(
      location: TextLocation,
      condition: BoundExpression,
      body: BoundExpression
  )
}
