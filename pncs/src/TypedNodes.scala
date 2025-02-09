import panther._




case class BoundAssembly(definitions: List[BoundDefinition], diagnostics: Diagnostics, entryPoint: Option[BoundEntry])

case class BoundEntry(
                       program: BoundDefinition.Object,
                       main: BoundMember.Method,
                       extraStatements: List[MemberSyntax.GlobalStatementSyntax]
                     )

enum BoundDefinition {
  case Object(symbol: Symbol, members: List[BoundMember])
  case Class(symbol: Symbol, members: List[BoundMember])
  //  case Enum(symbol: string, members: Array[String])
}

enum BoundMember {
  case Field(symbol: Symbol, typ: Type)
  case Method(name: string, parameters: List[BoundParameter], returnType: Type, body: BoundExpression)
}

case class BoundParameter(name: string, typ: Type)

enum BoundStatement {
    case Error
    case ExpressionStatement(expression: BoundExpression)
    case VariableDeclaration(variable: Symbol, isReadOnly: bool, typ: Type, initializer: BoundExpression)
}

enum BoundExpression {
  case Error
  case Assignment(location: TextLocation, variable: Symbol, expression: BoundExpression)
  case BinaryExpression(location: TextLocation, left: BoundExpression, operator: BinaryOperatorKind, right: BoundExpression, resultType: Type)
  case Block(statements: List[BoundStatement], expression: BoundExpression)
  case BooleanLiteral(location: TextLocation, value: bool)
  case CallExpression(location: TextLocation, method: BoundExpression, genericArguments: List[Type], arguments: List[BoundExpression], resultType: Type)
  case CastExpression(location: TextLocation, expression: BoundExpression, targetType: Type)
  case CharacterLiteral(location: TextLocation, value: char)
  case ForExpression(location: TextLocation, variable: Symbol, lowerBound: BoundExpression, upperBound: BoundExpression, body: BoundExpression)
  case IfExpression(location: TextLocation, cond: BoundExpression, thenExpr: BoundExpression, elseExpr: Option[BoundExpression], resultType: Type)
  case IndexExpression(location: TextLocation, array: BoundExpression, index: BoundExpression, resultType: Type)
  case IntLiteral(location: TextLocation, value: int)
  case MemberAccess(location: TextLocation, left: BoundExpression, member: Symbol, resultType: Type)
  case NewExpression(location: TextLocation, constructor: Symbol, genericArguments: List[Type], arguments: List[BoundExpression], resultType: Type)
  case StringLiteral(location: TextLocation, value: string)
  case UnaryExpression(location: TextLocation, operator: UnaryOperatorKind, operand: BoundExpression, resultType: Type)
  case UnitExpression(location: TextLocation)
  case Variable(location: TextLocation, symbol: Symbol, resultType: Option[Type])
  case WhileExpression(location: TextLocation, condition: BoundExpression, body: BoundExpression)

  //
  //  case ArrayCreationExpression(typ: Type,
  //                               newKeyword: SyntaxToken,
  //                               name: NameSyntax,
  //                               openBracket: SyntaxToken,
  //                               arrayRank: Option[TypedExpression],
  //                               closeBracket: SyntaxToken,
  //                               initializer: Option[ArrayInitializerExpressionSyntax])

  //  case AssignmentExpression(location: TextLocation, expression: Expression.AssignmentExpression)

  //  case BlockExpression(location: TextLocation, expression: Expression.BlockExpression)
  //  case ForExpression(location: TextLocation, expression: Expression.ForExpression)
  //  case GroupExpression(location: TextLocation, expression: Expression.GroupExpression)
  //  case IdentifierName(location: TextLocation, expression: Expression.IdentifierName)
  //  case IndexExpression(location: TextLocation, expression: Expression.IndexExpression)
  //  case LiteralExpression(location: TextLocation, expression: Expression.LiteralExpression)
  //  case MemberAccessExpression(location: TextLocation, expression: Expression.MemberAccessExpression)
  //  case MatchExpression(location: TextLocation, expression: Expression.MatchExpression)
  //  case NewExpression(location: TextLocation, expression: Expression.NewExpression)
  //  case UnitExpression(location: TextLocation, expression: Expression.UnitExpression)

}