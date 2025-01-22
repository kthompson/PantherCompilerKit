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
  case Named(ns: List[string], name: string, args: List[Type])

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

  def _name(list: List[string], name: string): string = {
    list match {
      case List.Nil => name
      case List.Cons(head, tail) => _name(tail, if (head == "") name else head + "." + name)
    }
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
        _name(ns, name) + tail
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

case class VariableName(name: String)

case class ClassName(name: String)

case class MemberName(name: String)

case class NamespaceName(name: String)

case class FunctionName(name: String)

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
  case NewExpression(location: TextLocation, constructor: Symbol, genericArguments: List[Type], arguments: List[BoundExpression], resultType: Type)
  case StringLiteral(location: TextLocation, value: string)
  case UnaryExpression(location: TextLocation, operator: UnaryOperatorKind, operand: BoundExpression, resultType: Type)
  case UnitExpression(location: TextLocation)
  case Variable(location: TextLocation, symbol: Symbol)
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