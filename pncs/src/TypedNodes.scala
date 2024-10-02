enum Type {
  case Function(parameters: List[TypedParameter], returnType: Type)
  case Array(inner: Type)
  case Named(symbol: Symbol)
  case Option(inner: Type)
  case Any
  case Boolean
  case Int
  case Never
  case String
  case Unit
  
  case Error
}

enum TypedUnaryOperator {
  case Negate
  case Not
}

enum TypedBinaryOperator {
  case Add
  case And
  case Divide
  case Equal
  case Greater
  case GreaterOrEqual
  case Less
  case LessOrEqual
  case Multiply
  case NotEqual
  case Or
  case Subtract
}

enum TypedExpression {
  case TypedLiteral(typ: Type, value: String)
  case TypedVariable(typ: Type, name: String)
  case TypedAssignment(name: String, value: TypedExpression)
  case TypedBinary(typ: Type, left: TypedExpression, op: TypedBinaryOperator, right: TypedExpression)
  case TypedCall(typ: Type, callee: TypedExpression, args: List[TypedExpression])
  case TypedIf(typ: Type, condition: TypedExpression, thenBranch: TypedExpression, elseBranch: TypedExpression)
  case TypedIndex(typ: Type, array: TypedExpression, index: TypedExpression)
  case TypedBlock(typ: Type, statements: List[TypedExpression])
  case TypedUnary(typ: Type, op: TypedUnaryOperator, operand: TypedExpression)
}

case class TypedParameter(name: String, typ: Type)

enum TypedMember {
  case TypedField(name: String, typeStr: String)
  case TypedMethod(name: String, returnType: String, args: List[TypedParameter])
}

enum TypedDefinition {
  case TypedClass(namespace: String, name: String, members: List[TypedMember])
  case TypedEnum(namespace: String, name: String, members: List[String])
}

case class TypedAssembly(classes: List[TypedDefinition])
