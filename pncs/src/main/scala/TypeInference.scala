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
    // First, infer from parameter/argument pairs (same as before)
    val fromArguments = inferFromParameterArgumentPairs(
      parameterTypes,
      argumentTypes,
      emptyTypeMap()
    )

    // NEW: Also infer from return type vs expected type
    // This helps when the return type contains type variables
    // Example: identity<T>(x: T): T called with expected type string
    val typeMap = inferTypeFromPair(returnType, expectedType, fromArguments)

    // Convert to list of types in the order of generic parameters
    mapGenericParamsToTypes(genericParams, typeMap)
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
