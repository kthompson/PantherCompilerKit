import scala.{Array => SArray}
import scala.{Option => SOption}

enum Type {
  case Function(parameters: SArray[TypedParameter], returnType: Type)
  case Array(inner: Type)
  case Reference(symbol: Symbol, baseType: SOption[Type])
  case Option(inner: Type)
  case Any
  case Never
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
  case TypedCall(typ: Type, callee: TypedExpression, args: Array[TypedExpression])
  case TypedIf(typ: Type, condition: TypedExpression, thenBranch: TypedExpression, elseBranch: TypedExpression)
  case TypedIndex(typ: Type, array: TypedExpression, index: TypedExpression)
  case TypedBlock(typ: Type, statements: Array[TypedExpression])
  case TypedUnary(typ: Type, op: TypedUnaryOperator, operand: TypedExpression)
}

case class TypedParameter(name: String, typ: Type)

enum TypedMember {
  case TypedField(name: String, typeStr: Type)
  case TypedMethod(name: String, returnType: Type, args: Array[TypedParameter])
}

enum TypedDefinition {
  case TypedClass(namespace: String, name: String, members: Array[TypedMember])
  case TypedEnum(namespace: String, name: String, members: Array[String])
}

case class TypedAssembly(classes: Array[TypedDefinition])
