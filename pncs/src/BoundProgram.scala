//import StatementSyntax.ExpressionStatement
//import panther._
//
//case class BoundProgram(root: Symbol,
//                        diagnostics: Diagnostics,
//                        definitions: BoundDefinitions)
//
//enum BoundDefinitions {
//  case Empty
//  case Cons(definition: BoundDefinition, tail: BoundDefinitions)
//}
//
//enum BoundDefinition {
//  case Object(symbol: Symbol, members: BoundDefinitions)
//  case Class(symbol: Symbol, fields: BoundFields, members: BoundDefinitions)
//  case Enum(symbol: Symbol, cases: Array[BoundEnumCase], members: BoundDefinitions)
//  case Function(symbol: Symbol, function: MemberSyntax.FunctionDeclarationSyntax, parameters: Array[BoundParameter], body: BoundExpression)
//  case Constructor(symbol: Symbol, node: MemberSyntax.ClassDeclarationSyntax, parameters: Array[BoundParameter])
//  case Field(symbol: Symbol, typeAnnotation: Option[TypeAnnotationSyntax], expression: BoundExpression)
//  case GlobalStatement(statement: StatementSyntax)
//}
//
//enum BoundFields {
//  case Empty
//  case Cons(field: BoundField, tail: BoundFields)
//}
//
//case class BoundField(symbol: Symbol, typeAnnotation: TypeAnnotationSyntax)
//
//case class BoundEnumCase(symbol: Symbol, fields: Array[BoundParameter])
//
//case class BoundParameter(symbol: Symbol, parameter: ParameterSyntax)
//
//enum BoundExpression {
//  // Special cases
//  case None
//  case Error
//  case Delayed(expression: Expression, scope: Scope)
//
////  Literals
//  case Boolean(value: bool)
//  case String(value: string)
//  case Character(value: char)
//  case Number(value: int)
//
//  // compound expressions
//  case Assign(lhs: BoundAssignable, expression: BoundExpression)
//  case Binary(left: BoundExpression, operator: BinaryOperatorKind, right: BoundExpression)
//  case MemberAccess(left: BoundExpression, right: string)
//  //
//  case Block(statements: Array[BoundStatement], expression: BoundExpression)
//
//  // control flow
//  case If(condition: BoundExpression, thenExpression: BoundExpression, elseExpression: BoundExpression)
//  case While(condition: BoundExpression, body: BoundExpression)
//  case For(variable: Symbol, lowerBound: BoundExpression, upperBound: BoundExpression, body: BoundExpression)
//
//  case Call(target: BoundExpression, arguments: Array[BoundExpression])
//  case New(symbol: Symbol, arguments: Array[BoundExpression])
//
//  case Assignable(assignable: BoundAssignable)
//}
//
//enum BoundAssignable {
//  case Index(symbol: Symbol, index: BoundExpression)
//  case Reference(symbol: Symbol)
//}
//
//enum BoundStatement {
//  case ExpressionStatement(expression: BoundExpression)
//  case VariableDeclarationStatement(symbol: Symbol, typeAnnotation: Option[TypeAnnotationSyntax], expression: BoundExpression)
//}
//
