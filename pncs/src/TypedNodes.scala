import panther._
enum Variance {
  case Invariant
  case Covariant // "out"
  case Contravariant // "in"
}

enum Type {

  /** A named type can have zero or more type arguments.
   *  E.g. `List[Int]` -> Named("panther", "List", Types.Cons(Named("", "Int"), Types.Empty))
   */
  case Named(ns: string, name: string, args: List[Type])

  /** A function type takes a list of parameters and a return type.
   *  E.g. `(x: Int, y: Int) => Int`
   */
  case Function(genericTypeParameters: List[Type], parameters: List[BoundParameter], returnType: Type)

  /** A type variable with optional upper-bound.
   *  e.g., `T <: SomeBase`.
   *
   */
  case Variable(name: string, variance: Variance, upperBound: Option[Type])

  /** Built-in "top" type */
  case Any

  /** Built-in "bottom" type */
  case Never

  case Error

  def _params(paramsStr: string, parameters: List[BoundParameter]): string = {
    parameters match {
      case List.Nil => paramsStr
      case List.Cons(param, tail) =>
        val sep = if (paramsStr == "") "" else ", "
        _params(paramsStr + sep + param.name + ": " + param.typ.toString, tail)
    }
  }

  def _args(str: string, args: List[Type]): string =
    args match {
      case List.Nil => str
      case List.Cons(typ, tail) =>
        val sep = if (str == "") "" else ", "
        _args(str + sep + typ.toString, tail)
    }

  override def toString(): string = {
    this match {
      case Type.Function(genTypeParams, parameters, returnType) =>
        val genTypeStr = _args("", genTypeParams)
        val paramStr = _params("", parameters)
        if(genTypeStr.isEmpty) "(" + paramStr + ") -> " + returnType.toString
        else "<" + genTypeStr + ">" + "(" + paramStr + ") -> " + returnType.toString
      case Type.Named(ns, name, args) =>
        val argStr = _args("", args)
        val tail = if (argStr.isEmpty) "" else "[" + argStr + "]"
        if (ns == "") name + tail
        else ns + "." + name + tail
      case Type.Variable(name, variance, upperBound) =>
        val varianceStr = variance match {
          case Variance.Invariant =>
            ""
          case Variance.Covariant =>
            "+"
          case Variance.Contravariant =>
            "-"
        }
        upperBound match {
          case Option.Some(value) => varianceStr + name + " : " + value.toString
          case Option.None => varianceStr + name
        }
      case Type.Any =>
        "Any"
      case Type.Never =>
        "Never"
      case Type.Error =>
        "Error"
    }
  }
}

enum BoundUnaryOperator {
  case Negate
  case Not
}

enum BoundBinaryOperator {
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

case class MemberName(name: String)

case class NamespaceName(name: String)

case class FunctionName(name: String)

case class TypeBinding(name: VariableName, typ: Type)

enum TypeEnvironment {
  case Empty
  case Cons(binding: TypeBinding, tail: TypeEnvironment)
}

object Environment {
  def getVarType(variable: VariableName, env: TypeEnvironment): Option[Type] = {
    env match {
      case TypeEnvironment.Empty => None
      case TypeEnvironment.Cons(binding, tail) =>
        if (binding.name == variable) {
          Some(binding.typ)
        } else {
          getVarType(variable, tail)
        }
    }
  }
}



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
  case Field(name: string, typ: Type)
  case Method(name: string, parameters: List[BoundParameter], returnType: Type, body: BoundExpression)
}

case class BoundParameter(name: string, typ: Type)

enum BoundExpression {
  case IntLiteral(typ: Type, location: TextLocation, value: int)
  //
  //  case ArrayCreationExpression(typ: Type,
  //                               newKeyword: SyntaxToken,
  //                               name: NameSyntax,
  //                               openBracket: SyntaxToken,
  //                               arrayRank: Option[TypedExpression],
  //                               closeBracket: SyntaxToken,
  //                               initializer: Option[ArrayInitializerExpressionSyntax])

  //  case AssignmentExpression(typ: Type, location: TextLocation, expression: Expression.AssignmentExpression)
  //  case BinaryExpression(typ: Type, location: TextLocation, expression: Expression.BinaryExpression)
  //  case BlockExpression(typ: Type, location: TextLocation, expression: Expression.BlockExpression)
  //  case CallExpression(typ: Type, location: TextLocation, expression: Expression.CallExpression)
  //  case ForExpression(typ: Type, location: TextLocation, expression: Expression.ForExpression)
  //  case GroupExpression(typ: Type, location: TextLocation, expression: Expression.GroupExpression)
  //  case IdentifierName(typ: Type, location: TextLocation, expression: Expression.IdentifierName)
  //  case IfExpression(typ: Type, location: TextLocation, expression: Expression.If)
  //  case IndexExpression(typ: Type, location: TextLocation, expression: Expression.IndexExpression)
  //  case LiteralExpression(typ: Type, location: TextLocation, expression: Expression.LiteralExpression)
  //  case MemberAccessExpression(typ: Type, location: TextLocation, expression: Expression.MemberAccessExpression)
  //  case MatchExpression(typ: Type, location: TextLocation, expression: Expression.MatchExpression)
  //  case NewExpression(typ: Type, location: TextLocation, expression: Expression.NewExpression)
  //  case UnaryExpression(typ: Type, location: TextLocation, expression: Expression.UnaryExpression)
  //  case UnitExpression(typ: Type, location: TextLocation, expression: Expression.UnitExpression)
  //  case WhileExpression(typ: Type, location: TextLocation, expression: Expression.WhileExpression)
}


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