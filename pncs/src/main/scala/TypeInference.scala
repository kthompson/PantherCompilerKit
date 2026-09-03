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
    val typeMap = new scala.collection.mutable.HashMap[String, Type]()

    // Try to infer from parameter/argument pairs
    inferFromParameterArgumentPairs(parameterTypes, argumentTypes, typeMap)

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
    val typeMap = new scala.collection.mutable.HashMap[String, Type]()

    // First, infer from parameter/argument pairs (same as before)
    inferFromParameterArgumentPairs(parameterTypes, argumentTypes, typeMap)

    // NEW: Also infer from return type vs expected type
    // This helps when the return type contains type variables
    // Example: identity<T>(x: T): T called with expected type string
    inferTypeFromPair(returnType, expectedType, typeMap)

    // Convert to list of types in the order of generic parameters
    mapGenericParamsToTypes(genericParams, typeMap)
  }

  def mapGenericParamsToTypes(
      genericParams: List[GenericTypeParameter],
      typeMap: scala.collection.mutable.HashMap[String, Type]
  ): List[Type] = {
    mapGenericParamsToTypesWithIndex(genericParams, typeMap, 0)
  }

  def mapGenericParamsToTypesWithIndex(
      genericParams: List[GenericTypeParameter],
      typeMap: scala.collection.mutable.HashMap[String, Type],
      index: int
  ): List[Type] = {
    genericParams match {
      case List.Nil => List.Nil
      case List.Cons(param, tail) =>
        val key = "var_" + string(index)
        val inferredType = typeMap.getOrElse(key, binder.anyType)
        List.Cons(
          inferredType,
          mapGenericParamsToTypesWithIndex(tail, typeMap, index + 1)
        )
    }
  }

  def inferFromParameterArgumentPairs(
      paramTypes: List[Type],
      argTypes: List[Type],
      typeMap: scala.collection.mutable.HashMap[String, Type]
  ): Unit = {
    Tuple2(paramTypes, argTypes) match {
      case Tuple2(
            List.Cons(paramType, paramTail),
            List.Cons(argType, argTail)
          ) =>
        inferTypeFromPair(paramType, argType, typeMap)
        inferFromParameterArgumentPairs(paramTail, argTail, typeMap)
      case _ => // Different lengths or empty lists
    }
  }

  def inferTypeFromPair(
      paramType: Type,
      argType: Type,
      typeMap: scala.collection.mutable.HashMap[String, Type]
  ): unit = {
    Tuple2(paramType, argType) match {
      case Tuple2(Type.Variable(_, id), _) =>
        // First binding wins; a later, conflicting argument is caught when
        // the arguments are converted to the instantiated parameter types.
        val key = "var_" + string(id)
        if (!typeMap.contains(key)) {
          typeMap.put(key, argType)
        }
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
        }
      case Tuple2(
            Type.Class(_, _, _, paramArgs, caseSymbol),
            Type.Alias(_, _, _, argArgs, _, aliasSymbol)
          ) =>
        if (isCaseOf(caseSymbol, aliasSymbol)) {
          inferFromParameterArgumentPairs(paramArgs, argArgs, typeMap)
        }
      case Tuple2(
            Type.Function(_, paramParams, paramReturn),
            Type.Function(_, argParams, argReturn)
          ) =>
        inferFromParameterArgumentPairs(
          parameterTypes(paramParams),
          parameterTypes(argParams),
          typeMap
        )
        inferTypeFromPair(paramReturn, argReturn, typeMap)
      case _ =>
        // A union has no single argument list to learn from, and nothing
        // else carries type arguments.
        ()
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

  /** Constructor-specific type inference methods
    */
  def inferTypeArgumentsFromConstructor(
      genericParams: List[GenericTypeParameter],
      constructor: Symbol,
      args: List[BoundExpression]
  ): List[Type] = {
    binder.tryGetSymbolType(constructor) match {
      case Option.Some(Type.GenericFunction(_, _, _, params, _)) =>
        inferFromGenericParameters(genericParams, params, args)
      case Option.Some(Type.Function(_, params, _)) =>
        basicInference(genericParams, args)
      case _ =>
        mapGenericParamsToAny(genericParams)
    }
  }

  def inferFromGenericParameters(
      genericParams: List[GenericTypeParameter],
      constructorParams: List[BoundParameter],
      args: List[BoundExpression]
  ): List[Type] = {
    genericParams match {
      case List.Cons(param, List.Nil) =>
        // Single type parameter - find first constructor argument that can determine its type
        val inferredType = inferSingleTypeParameter(0, constructorParams, args)
        List.Cons(inferredType, List.Nil)
      case _ =>
        // Multiple type parameters - more complex, fall back to Any for now
        mapGenericParamsToAny(genericParams)
    }
  }

  def inferSingleTypeParameter(
      parameterIndex: int,
      constructorParams: List[BoundParameter],
      args: List[BoundExpression]
  ): Type = {
    // Look through constructor parameters and arguments to find a concrete type
    Tuple2(constructorParams, args) match {
      case Tuple2(List.Cons(param, paramTail), List.Cons(arg, argTail)) =>
        param.typ match {
          case Type.Variable(_, 0) =>
            // This parameter has type T (type variable 0) - use the argument type
            val argType = binder.getType(arg)
            if (isConcreteType(argType)) {
              argType
            } else {
              // Argument type is polymorphic, try next parameter
              inferSingleTypeParameter(parameterIndex + 1, paramTail, argTail)
            }
          case _ =>
            // This parameter doesn't use our type variable, try next parameter
            inferSingleTypeParameter(parameterIndex + 1, paramTail, argTail)
        }
      case _ =>
        // No more parameters or arguments - fall back to Any
        binder.anyType
    }
  }

  def isConcreteType(typ: Type): bool = {
    typ match {
      case Type.Variable(_, _)              => false
      case Type.GenericClass(_, _, _, _, _) => false
      case Type.Any                         => false
      case Type.Never                       => false
      case Type.Error(_)                    => false
      case _                                => true
    }
  }

  def basicInference(
      genericParams: List[GenericTypeParameter],
      args: List[BoundExpression]
  ): List[Type] = {
    // Fallback to original simple logic for non-generic constructors
    genericParams match {
      case List.Cons(param, List.Nil) =>
        args match {
          case List.Cons(arg, List.Nil) =>
            // Single type parameter, single argument - infer from argument
            List.Cons(binder.getType(arg), List.Nil)
          case _ =>
            // Multiple or no arguments - for now, use Any as fallback
            List.Cons(binder.anyType, List.Nil)
        }
      case _ =>
        // Multiple type parameters - for now, use Any for all
        mapGenericParamsToAny(genericParams)
    }
  }

  def mapGenericParamsToAny(params: List[GenericTypeParameter]): List[Type] = {
    params match {
      case List.Nil => List.Nil
      case List.Cons(_, tail) =>
        List.Cons(binder.anyType, mapGenericParamsToAny(tail))
    }
  }
}
