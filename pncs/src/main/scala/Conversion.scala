import panther._

enum Conversion {
  case Implicit
  case Explicit
  case Identity
  case None

  val isImplicit: bool = this == Implicit
  val isExplicit: bool = this == Explicit
  val isIdentity: bool = this == Identity
  val Exists: bool = this != None
}

class ConversionClassifier(binder: Binder) {
  def classify(from: Type, toType: Type): Conversion = {
    if (from == toType || semanticTypeEquals(from, toType)) {
      Conversion.Identity
    } else if (toType == binder.anyType || toType == binder.unitType) {
      Conversion.Implicit
    } else if (from == binder.neverType) {
      Conversion.Implicit
    } else if (from == binder.anyType) {
      Conversion.Explicit
    } else if (toType == binder.stringType) {
      Conversion.Explicit
    } else if (from == binder.intType && toType == binder.charType) {
      Conversion.Explicit
    } else if (from == binder.charType && toType == binder.intType) {
      Conversion.Implicit
    } else {
      Tuple2(from, toType) match {
        // Handle conversion from union to alias (e.g., Enum.Case1 | Enum.Case2 -> Enum)
        case Tuple2(
              Type.Union(_, fromCases),
              Type.Alias(_, _, _, aliasValue, _)
            ) =>
          aliasValue match {
            case Type.Union(_, aliasCases) =>
              if (unionSubsetOf(fromCases, aliasCases)) {
                Conversion.Identity
              } else {
                Conversion.None
              }
            case _ =>
              Conversion.None
          }
        case _ =>
          toType match {
            case Type.Alias(location, _, _, value, _) =>
              classify(from, value)
            case Type.Union(location, cases) =>
              cases match {
                case List.Nil => Conversion.None
                case List.Cons(head, tail) =>
                  classify(from, head) match {
                    case Conversion.None =>
                      classify(from, Type.Union(location, tail))
                    case conversion => conversion
                  }
              }
            case _ =>
              Conversion.None
          }
      }
    }
  }

  def semanticTypeEquals(type1: Type, type2: Type): bool = {
    Tuple2(type1, type2) match {
      case Tuple2(
            Type.Class(_, ns1, name1, sym1),
            Type.Class(_, ns2, name2, sym2)
          ) =>
        sym1 == sym2 && name1 == name2 && ns1 == ns2
      case Tuple2(Type.Union(_, cases1), Type.Union(_, cases2)) =>
        semanticTypeListEquals(cases1, cases2)
      case Tuple2(
            Type.Alias(_, ns1, name1, value1, sym1),
            Type.Alias(_, ns2, name2, value2, sym2)
          ) =>
        sym1 == sym2 && name1 == name2 && ns1 == ns2 && semanticTypeEquals(
          value1,
          value2
        )
      case Tuple2(
            Type.Function(_, params1, ret1),
            Type.Function(_, params2, ret2)
          ) =>
        semanticTypeEquals(ret1, ret2) && semanticBoundParameterListEquals(
          params1,
          params2
        )
      case Tuple2(
            Type.GenericClass(_, ns1, name1, args1, sym1),
            Type.GenericClass(_, ns2, name2, args2, sym2)
          ) =>
        sym1 == sym2 && name1 == name2 && ns1 == ns2 && semanticGenericParameterListEquals(
          args1,
          args2
        )
      case Tuple2(Type.Any, Type.Any)                 => true
      case Tuple2(Type.Never, Type.Never)             => true
      case Tuple2(Type.Error(msg1), Type.Error(msg2)) => msg1 == msg2
      case _                                          => false
    }
  }

  def semanticTypeListEquals(list1: List[Type], list2: List[Type]): bool = {
    Tuple2(list1, list2) match {
      case Tuple2(List.Nil, List.Nil) => true
      case Tuple2(List.Cons(head1, tail1), List.Cons(head2, tail2)) =>
        semanticTypeEquals(head1, head2) && semanticTypeListEquals(tail1, tail2)
      case _ => false
    }
  }

  def semanticBoundParameterListEquals(
      params1: List[BoundParameter],
      params2: List[BoundParameter]
  ): bool = {
    Tuple2(params1, params2) match {
      case Tuple2(List.Nil, List.Nil) => true
      case Tuple2(List.Cons(param1, tail1), List.Cons(param2, tail2)) =>
        semanticTypeEquals(
          param1.typ,
          param2.typ
        ) && semanticBoundParameterListEquals(tail1, tail2)
      case _ => false
    }
  }

  def semanticGenericParameterListEquals(
      params1: List[Type.GenericTypeParameter],
      params2: List[Type.GenericTypeParameter]
  ): bool = {
    Tuple2(params1, params2) match {
      case Tuple2(List.Nil, List.Nil) => true
      case Tuple2(List.Cons(param1, tail1), List.Cons(param2, tail2)) =>
        val upperBoundsEqual =
          Tuple2(param1.upperBound, param2.upperBound) match {
            case Tuple2(Option.Some(bound1), Option.Some(bound2)) =>
              semanticTypeEquals(bound1, bound2)
            case Tuple2(Option.None, Option.None) => true
            case _                                => false
          }
        param1.name == param2.name && param1.variance == param2.variance &&
        semanticGenericParameterListEquals(tail1, tail2) && upperBoundsEqual
      case _ => false
    }
  }

  def unionSubsetOf(subset: List[Type], superset: List[Type]): bool = {
    subset match {
      case List.Nil => true
      case List.Cons(head, tail) =>
        typeInList(head, superset) && unionSubsetOf(tail, superset)
    }
  }

  def typeInList(typ: Type, list: List[Type]): bool = {
    list match {
      case List.Nil => false
      case List.Cons(head, tail) =>
        if (semanticTypeEquals(typ, head)) {
          true
        } else {
          typeInList(typ, tail)
        }
    }
  }
}
