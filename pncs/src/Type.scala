import panther._

/** A type variable with optional upper-bound. e.g., `T <: SomeBase`.
  *
  * @param name
  * @param variance
  * @param upperBound
  */
case class GenericTypeParameter(
    location: TextLocation,
    name: string,
    variance: Variance,
    upperBound: Option[Type]
)

enum Type {

  /** A named type can have zero or more type arguments. E.g. `List[Int]` ->
    * Named("panther", "List", Types.Cons(Named("", "Int"), Types.Empty))
    */
  case Class(
      location: TextLocation,
      ns: List[string],
      name: string,
      args: List[Type],
      superClass: Option[Type]
  )

  case GenericClass(
      location: TextLocation,
      ns: List[string],
      name: string,
      args: List[GenericTypeParameter],
      superClass: Option[Type]
  )

  /** A generic function is a type that has type parameters. this gets converted
    * to a regular function
    */
  case GenericFunction(
      location: TextLocation,
      generics: List[GenericTypeParameter],
      traits: List[Type],
      parameters: List[BoundParameter],
      returnType: Type
  )

  /** A function type takes a list of parameters and a return type. E.g.
    * `(x: Int, y: Int) => Int`
    */
  case Function(
      location: TextLocation,
      parameters: List[BoundParameter],
      returnType: Type
  )

//  /** A type variable with optional upper-bound.
//   *  e.g., `T <: SomeBase`.
//   *
//   */
//  case Generic(location: TextLocation, name: string, variance: Variance, upperBound: Option[Type])

  /** A type variable used in type inference.
    */
  case Variable(location: TextLocation, id: int)

  /** Built-in "top" type */
  case Any

  /** Built-in "bottom" type */
  case Never

  case Error

  def getLocation(): Option[TextLocation] = {
    this match {
      case Class(location, _, _, _, _)           => Option.Some(location)
      case Function(location, _, _)              => Option.Some(location)
      case GenericClass(location, _, _, _, _)    => Option.Some(location)
      case GenericFunction(location, _, _, _, _) => Option.Some(location)
      case Variable(location, _)                 => Option.Some(location)
      case Any                                   => Option.None
      case Never                                 => Option.None
      case Error                                 => Option.None
    }
  }

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
      case List.Cons(head, tail) =>
        _name(tail, if (head == "") name else head + "." + name)
    }
  }

  def _genArgs(str: string, items: List[GenericTypeParameter]): string = {
    items match {
      case List.Nil => str
      case List.Cons(item, tail) =>
        val sep = if (str == "") "" else ", "
        val varianceStr = item.variance match {
          case Variance.Invariant =>
            ""
          case Variance.Covariant =>
            "+"
          case Variance.Contravariant =>
            "-"
        }
        val upperBoundStr = item.upperBound match {
          case Option.Some(value) => " <: " + value.toString
          case Option.None        => ""
        }
        _genArgs(str + sep + varianceStr + item.name + upperBoundStr, tail)
    }
  }

  override def toString(): string = {
    this match {
      case Type.Function(_, parameters, returnType) =>
        val paramStr = _params("", parameters)
        "(" + paramStr + ") -> " + returnType.toString
      case Type.Class(_, ns, name, args, _) =>
        val argStr = _args("", args)
        val tail = if (argStr.isEmpty) "" else "<" + argStr + ">"
        _name(ns, name) + tail

      case Type.GenericClass(_, ns, name, generics, _) =>
        val argStr = _genArgs("", generics)
        val tail = if (argStr.isEmpty) "" else "<" + argStr + ">"
        _name(ns, name) + tail

      case Type.GenericFunction(_, generics, traits, parameters, returnType) =>
        val paramStr = _params("", parameters)
        "<" + _genArgs(
          "",
          generics
        ) + ">" + "(" + paramStr + ") -> " + returnType.toString

//      case Type.Generic(_, name, variance, upperBound) =>
//        val varianceStr = variance match {
//          case Variance.Invariant =>
//            ""
//          case Variance.Covariant =>
//            "+"
//          case Variance.Contravariant =>
//            "-"
//        }
//        upperBound match {
//          case Option.Some(value) => varianceStr + name + " : " + value.toString
//          case Option.None => varianceStr + name
//        }
      case Type.Variable(_, i) => "$" + string(i)
      case Type.Any            => "any"
      case Type.Never          => "Never"
      case Type.Error          => "Error"
    }
  }
}
