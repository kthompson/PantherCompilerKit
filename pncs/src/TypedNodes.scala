import scala.{Array => SArray}
import scala.{Option => SOption}

enum Type {
  case Function(parameters: SArray[TypedParameter], returnType: Type)
  case Array(inner: Type)
  case Reference(symbol: Symbol, baseType: SOption[Type])
  case Option(inner: Type)
  case Int
  case Bool
  case Class(name: ClassName)
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

case class VariableName(name: String)

case class ClassName(name: String)

case class FieldName(name: String)

case class MethodName(name: String)
case class FunctionName(name: String)

case class TypeBinding(name: VariableName, typ: Type)

case class TypeEnvironment(bindings: Array[TypeBinding])

enum TypedExpression {
  case ArrayCreationExpression(typ: Type, expression: Expression.ArrayCreationExpression)
  case AssignmentExpression(typ: Type, expression: Expression.AssignmentExpression)
  case BinaryExpression(typ: Type, expression: Expression.BinaryExpression)
  case BlockExpression(typ: Type, expression: Expression.BlockExpression)
  case CallExpression(typ: Type, expression: Expression.CallExpression)
  case ForExpression(typ: Type, expression: Expression.ForExpression)
  case GroupExpression(typ: Type, expression: Expression.GroupExpression)
  case IdentifierName(typ: Type, expression: Expression.IdentifierName)
  case IfExpression(typ: Type, expression: Expression.IfExpression)
  case IndexExpression(typ: Type, expression: Expression.IndexExpression)
  case LiteralExpression(typ: Type, expression: Expression.LiteralExpression)
  case MemberAccessExpression(typ: Type, expression: Expression.MemberAccessExpression)
  case MatchExpression(typ: Type, expression: Expression.MatchExpression)
  case NewExpression(typ: Type, expression: Expression.NewExpression)
  case UnaryExpression(typ: Type, expression: Expression.UnaryExpression)
  case UnitExpression(typ: Type, expression: Expression.UnitExpression)
  case WhileExpression(typ: Type, expression: Expression.WhileExpression)
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

//case class FunctionDefinition(name: FunctionName, )
//(* Function defn consists of the function name, return type, the list of params, and the
//  body expr of the function *)
//type function_defn =
//  | TFunction of
//Function_name.t * borrowed_ref option * type_expr * param list * block_expr
//
//(* Method defn consists the method name, return type (and whether it returns a borrowed
//  ref), the list of params, the capabilities used and the body expr of the function *)
//type method_defn =
//  | TMethod of
//Method_name.t
//  * borrowed_ref option
//  * type_expr
//  * param list
//  * Capability_name.t list
//  * block_expr
//
//(* Class definitions consist of the class name, its capabilities and the fields and
//  methods in the class *)
//type class_defn =
//  | TClass of Class_name.t * capability list * field_defn list * method_defn list
//
//(* Each bolt program defines the classes,followed by functions, followed by the main
//  expression to execute. *)
//type program = Prog of class_defn list * function_defn list * block_expr