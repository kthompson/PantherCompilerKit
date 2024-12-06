import panther._

case class BoundField(symbol: Symbol, typeAnnotation: Option[TypeAnnotationSyntax], expression: Option[Expression])

case class BoundParameter(symbol: Symbol, parameter: ParameterSyntax)

case class BoundEnumCase(symbol: Symbol, fields: Array[BoundParameter])

enum BoundDefinition {
  case Object(symbol: Symbol, members: BoundDefinitions)
  case Class(symbol: Symbol, fields: Array[BoundField], members: BoundDefinitions)
  case Enum(symbol: Symbol, cases: Array[BoundEnumCase], members: BoundDefinitions)
  case Function(symbol: Symbol, function: MemberSyntax.FunctionDeclarationSyntax, parameters: Array[BoundParameter], body: Option[Expression])
  case Method(symbol: Symbol, function: MemberSyntax.FunctionDeclarationSyntax, parameters: Array[BoundParameter], body: Option[Expression])
  case Field(symbol: Symbol, typeAnnotation: Option[TypeAnnotationSyntax], expression: Option[Expression])
  case GlobalStatement(statement: StatementSyntax)
}

enum BoundDefinitions {
  case Empty
  case Cons(definition: BoundDefinition, tail: BoundDefinitions)
}

enum BoundFields {
  case Empty
  case Cons(field: BoundField, tail: BoundFields)
}

case class BoundTree(root: Symbol,
                     diagnostics: Diagnostics,
                     definitions: BoundDefinitions)
