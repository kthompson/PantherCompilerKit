import panther._

enum Type {

  /** A type variable with optional upper-bound. e.g., `T <: SomeBase`.
    *
    * @param name
    * @param variance
    * @param upperBound
    */
  case GenericTypeParameter(
      location: TextLocation,
      name: string,
      variance: Variance,
      upperBound: Option[Type]
  )

  /** A named type can have zero or more type arguments. E.g. `List[Int]` ->
    * Named("panther", "List", List.Cons(Named("", "Int"), List.Nil))
    */
  case Class(
      location: TextLocation,
      ns: List[string],
      name: string,
      symbol: Symbol
  )

  case GenericClass(
      location: TextLocation,
      ns: List[string],
      name: string,
      typeParameters: List[GenericTypeParameter],
      symbol: Symbol // may refer to type parameters
  )

  case InstantiatedGenericClass(
      genericClass: GenericClass,
      typeArguments: List[Type],
      instantiatedSymbol: Symbol
  )

  // TODO: this should probably have generic and instantiated variants too
  case Alias(
      location: TextLocation,
      ns: List[string],
      name: string,
      value: Type,
      symbol: Symbol
  )

  case GenericAlias(
      location: TextLocation,
      ns: List[string],
      name: string,
      typeParameters: List[GenericTypeParameter],
      value: Type, // may refer to type parameters
      symbol: Symbol // may refer to type parameters
  )

  case InstantiatedGenericAlias(
      genericAlias: GenericAlias,
      typeArguments: List[Type],
      value: Type, // instantiated value type
      instantiatedSymbol: Symbol
  )

  case Union(
      location: TextLocation,
      cases: List[Type]
  )

  /** A function type takes a list of parameters and a return type. E.g.
    * `(x: Int, y: Int) => Int`
    */
  case Function(
      location: TextLocation,
      parameters: List[BoundParameter],
      returnType: Type
  )

  /** A generic function is a type that has type parameters. this gets
    * instantiated via InstantiatedGenericFunction
    */
  case GenericFunction(
      location: TextLocation,
      generics: List[GenericTypeParameter],
      traits: List[Type],
      parameters: List[BoundParameter], // May refer to generics
      returnType: Type // May refer to generics
  )

  /** Apply typeArguments A=Int, B=Bool to <A, B>(x: A) => B to get (x: int) =>
    * bool
    */
  case InstantiatedGenericFunction(
      genericFunction: GenericFunction,
      typeArguments: List[Type],
      parameters: List[BoundParameter],
      returnType: Type
  )

  /** Built-in "top" type */
  case Any

  /** Built-in "bottom" type */
  case Never

  case Error(message: string)

  def getLocation(): Option[TextLocation] = {
    this match {
      case Class(location, _, _, _)           => Option.Some(location)
      case GenericClass(location, _, _, _, _) => Option.Some(location)
      case InstantiatedGenericClass(genericClass, _, _) =>
        Option.Some(genericClass.location)

      case Function(location, _, _)              => Option.Some(location)
      case GenericFunction(location, _, _, _, _) => Option.Some(location)
      case InstantiatedGenericFunction(genericFunction, _, _, _) =>
        Option.Some(genericFunction.location)

      case Alias(location, _, _, _, _)           => Option.Some(location)
      case GenericAlias(location, _, _, _, _, _) => Option.Some(location)
      case InstantiatedGenericAlias(genericAlias, _, _, _) =>
        Option.Some(genericAlias.location)

      case Type.GenericTypeParameter(location, _, _, _) => Option.Some(location)

      case Union(location, _) => Option.Some(location)
      case Any                => Option.None
      case Never              => Option.None
      case Error(_)           => Option.None
    }
  }

  def _params(paramsStr: string, parameters: List[BoundParameter]): string = {
    parameters match {
      case List.Nil => paramsStr
      case List.Cons(param, tail) =>
        val sep = if (paramsStr == "") "" else ", "
        _params(
          paramsStr + sep + param.symbol.name + ": " + param.typ.toString,
          tail
        )
    }
  }

  def _args(str: string, separator: string, args: List[Type]): string =
    args match {
      case List.Nil => str
      case List.Cons(typ, tail) =>
        val sep = if (str == "") "" else separator
        _args(str + sep + typ.toString, separator, tail)
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

      case Type.GenericFunction(_, generics, traits, parameters, returnType) =>
        val paramStr = _params("", parameters)
        "<" + _genArgs(
          "",
          generics
        ) + ">" + "(" + paramStr + ") -> " + returnType.toString

      case Type.InstantiatedGenericFunction(
            genericFunction,
            typeArguments,
            parameters,
            returnType
          ) =>

        val paramStr = _params("", parameters)
        "<" + _args(
          "",
          ",",
          typeArguments
        ) + ">" + "(" + paramStr + ") -> " + returnType.toString

      case Type.Alias(_, ns, name, value, _) =>
        _name(ns, name)

      case Type.GenericAlias(_, ns, name, generics, value, _) =>
        val argStr = _genArgs("", generics)
        val tail = if (argStr.isEmpty) "" else "<" + argStr + ">"
        _name(ns, name) + tail

      case Type.InstantiatedGenericAlias(
            genericAlias,
            typeArguments,
            value,
            _
          ) =>
        val argStr = _args("", ", ", typeArguments)
        val tail = if (argStr.isEmpty) "" else "<" + argStr + ">"
        _name(genericAlias.ns, genericAlias.name) + tail

      case Type.GenericTypeParameter(_, name, _, _) => name

      case Type.Union(_, cases) =>
        _args("", " | ", cases)

      case Type.Class(_, ns, name, _) =>
        _name(ns, name)

      case Type.GenericClass(_, ns, name, generics, _) =>
        val argStr = _genArgs("", generics)
        val tail = if (argStr.isEmpty) "" else "<" + argStr + ">"
        _name(ns, name) + tail

      case Type.InstantiatedGenericClass(genericClass, typeArguments, _) =>
        val argStr = _args("", ", ", typeArguments)
        val tail = if (argStr.isEmpty) "" else "<" + argStr + ">"
        _name(genericClass.ns, genericClass.name) + tail

      case Type.Any      => "any"
      case Type.Never    => "Never"
      case Type.Error(_) => "Error"
    }
  }
}

object Types {

  // TODO: skip redundant types in the union
  def union(t1: Type, t2: Type): Type = {
    if (t1 == t2) t1
    else {
      Tuple2(t1, t2) match {
        // Never is the identity element for union: Never ∪ T = T
        case Tuple2(Type.Never, _) => t2
        case Tuple2(_, Type.Never) => t1
        case Tuple2(Type.Union(location, cases1), Type.Union(_, cases2)) =>
          Type.Union(location, ListModule.concat(cases1, cases2))
        case Tuple2(Type.Union(location, cases), _) =>
          Type.Union(
            location,
            List.Cons(t2, cases)
          )
        case Tuple2(_, Type.Union(location, cases)) =>
          Type.Union(
            location,
            List.Cons(t1, cases)
          )
        case _ =>
          Type.Union(
            t1.getLocation() match {
              case Option.Some(location) => location
              case Option.None           => TextLocationFactory.empty()
            },
            List.Cons(t1, List.Cons(t2, List.Nil))
          )
      }
    }
  }
}
