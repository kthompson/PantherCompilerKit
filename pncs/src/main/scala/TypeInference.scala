import panther.*

case class TypeInference(binder: Binder) {

  /** Infers type arguments for a generic function call based on parameter and
    * argument types
    */
  def inferTypeArgumentsFromCall(
      genericParams: List[GenericTypeParameter],
      parameterTypes: List[Type],
      argumentTypes: List[Type]
  ): List[Type] = {
    // Try to infer from parameter/argument pairs
    val typeMap = inferFromParameterArgumentPairs(
      parameterTypes,
      argumentTypes,
      emptyTypeMap()
    )

    // Convert to list of types in the order of generic parameters
    mapGenericParamsToTypes(genericParams, typeMap)
  }

  /** Infers type arguments for a generic function call using both argument
    * types AND the expected return type (bidirectional inference)
    */
  def checkTypeArgumentsFromCall(
      genericParams: List[GenericTypeParameter],
      parameterTypes: List[Type],
      argumentTypes: List[Type],
      returnType: Type,
      expectedType: Type
  ): List[Type] = {
    // The expected type goes first, and first binding wins: a call in check
    // position is being asked for that type, so it fixes what it can and the
    // arguments fill in the rest. Reading the arguments first would let
    // `Tuple2(value, Chain.Empty())` answer `Tuple2<T, Chain.Empty<T>>` where
    // the context asked for `Tuple2<T, Chain<T>>` — an argument's type is a
    // lower bound on its parameter, not the parameter itself.
    val fromExpected =
      inferTypeFromPair(returnType, expectedType, emptyTypeMap())

    val typeMap = inferFromParameterArgumentPairs(
      parameterTypes,
      argumentTypes,
      fromExpected
    )

    // Convert to list of types in the order of generic parameters
    mapGenericParamsToTypes(genericParams, typeMap)
  }

  /** The type arguments the expected type alone fixes, or `Option.None` when it
    * leaves any of them open.
    *
    * Runs before the arguments are bound, so that a parameter type can be the
    * expected type of the argument written in that position. A partial solution
    * is no use there: an unsolved slot stays a `Type.Variable`, and a variable
    * of the callee cannot be told from one the enclosing method declared,
    * because both are numbered by position (ADR 0001).
    */
  def solveTypeArgumentsFromExpected(
      genericParams: List[GenericTypeParameter],
      resultType: Type,
      expectedType: Type
  ): Option[List[Type]] = {
    resultType match {
      case Type.Variable(_, _) =>
        // A bare variable matches whatever it is paired with, so the expected
        // type would fix the callee's parameter without saying anything about
        // it. Only a result with structure is evidence.
        Option.None
      case _ =>
        val typeMap =
          inferTypeFromPair(resultType, expectedType, emptyTypeMap())
        if (allSolved(genericParams, typeMap, 0)) {
          Option.Some(mapGenericParamsToTypes(genericParams, typeMap))
        } else {
          Option.None
        }
    }
  }

  def allSolved(
      genericParams: List[GenericTypeParameter],
      typeMap: Dictionary[int, Type],
      index: int
  ): bool =
    genericParams match {
      case List.Nil => true
      case List.Cons(_, tail) =>
        if (typeMap.contains(index)) allSolved(tail, typeMap, index + 1)
        else false
    }

  /** What each type variable has been inferred to be so far, keyed by the
    * variable's id.
    *
    * `Dictionary` is immutable, so every method that learns something returns a
    * new map rather than updating this one in place, and callers thread it
    * through. Inference visits a handful of parameters at a time, so the linear
    * `get` costs nothing here.
    */
  def emptyTypeMap(): Dictionary[int, Type] =
    DictionaryModule.empty[int, Type]()

  def mapGenericParamsToTypes(
      genericParams: List[GenericTypeParameter],
      typeMap: Dictionary[int, Type]
  ): List[Type] = {
    mapGenericParamsToTypesWithIndex(genericParams, typeMap, 0)
  }

  def mapGenericParamsToTypesWithIndex(
      genericParams: List[GenericTypeParameter],
      typeMap: Dictionary[int, Type],
      index: int
  ): List[Type] = {
    genericParams match {
      case List.Nil => List.Nil
      case List.Cons(param, tail) =>
        val inferredType = typeMap.get(index) match {
          case Option.Some(typ) => typ
          case Option.None      => unsolvedDefault(param)
        }
        List.Cons(
          inferredType,
          mapGenericParamsToTypesWithIndex(tail, typeMap, index + 1)
        )
    }
  }

  /** What an unsolved parameter becomes. A covariant slot filled with never
    * converts to any instantiation, so Result.Error(e) satisfies every
    * Result[E, B]. Anything else has nothing safer than any.
    */
  def unsolvedDefault(param: GenericTypeParameter): Type = {
    param.variance match {
      case Variance.Covariant => binder.neverType
      case _                  => binder.anyType
    }
  }

  def inferFromParameterArgumentPairs(
      paramTypes: List[Type],
      argTypes: List[Type],
      typeMap: Dictionary[int, Type]
  ): Dictionary[int, Type] = {
    Tuple2(paramTypes, argTypes) match {
      case Tuple2(
            List.Cons(paramType, paramTail),
            List.Cons(argType, argTail)
          ) =>
        inferFromParameterArgumentPairs(
          paramTail,
          argTail,
          inferTypeFromPair(paramType, argType, typeMap)
        )
      case _ => typeMap // Different lengths or empty lists
    }
  }

  def inferTypeFromPair(
      paramType: Type,
      argType: Type,
      typeMap: Dictionary[int, Type]
  ): Dictionary[int, Type] = {
    Tuple2(paramType, argType) match {
      case Tuple2(Type.Variable(_, id), _) =>
        // First binding wins; a later, conflicting argument is caught when
        // the arguments are converted to the instantiated parameter types.
        if (typeMap.contains(id)) typeMap
        else typeMap.put(id, argType)
      case Tuple2(
            Type.Class(_, _, _, paramArgs, _),
            Type.Class(_, _, _, argArgs, _)
          ) =>
        inferFromParameterArgumentPairs(paramArgs, argArgs, typeMap)
      case Tuple2(
            Type.Alias(_, _, _, paramArgs, _, _),
            Type.Alias(_, _, _, argArgs, _, _)
          ) =>
        inferFromParameterArgumentPairs(paramArgs, argArgs, typeMap)
      case Tuple2(
            Type.Alias(_, _, _, paramArgs, _, aliasSymbol),
            Type.Class(_, _, _, argArgs, caseSymbol)
          ) =>
        // A case against its enum, e.g. List.Cons<int> against List<T>. The
        // case's parameter list is the enum's, so the arguments line up.
        if (isCaseOf(caseSymbol, aliasSymbol)) {
          inferFromParameterArgumentPairs(paramArgs, argArgs, typeMap)
        } else typeMap
      case Tuple2(
            Type.Class(_, _, _, paramArgs, caseSymbol),
            Type.Alias(_, _, _, argArgs, _, aliasSymbol)
          ) =>
        if (isCaseOf(caseSymbol, aliasSymbol)) {
          inferFromParameterArgumentPairs(paramArgs, argArgs, typeMap)
        } else typeMap
      case Tuple2(
            Type.Function(_, paramParams, paramReturn),
            Type.Function(_, argParams, argReturn)
          ) =>
        inferTypeFromPair(
          paramReturn,
          argReturn,
          inferFromParameterArgumentPairs(
            parameterTypes(paramParams),
            parameterTypes(argParams),
            typeMap
          )
        )
      case _ =>
        // A union has no single argument list to learn from, and nothing
        // else carries type arguments.
        typeMap
    }
  }

  def isCaseOf(caseSymbol: Symbol, enumSymbol: Symbol): bool =
    caseSymbol.parent match {
      case Option.Some(parent) => parent == enumSymbol
      case Option.None         => false
    }

  def parameterTypes(params: List[BoundParameter]): List[Type] =
    params match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        List.Cons(head.typ, parameterTypes(tail))
    }
}
