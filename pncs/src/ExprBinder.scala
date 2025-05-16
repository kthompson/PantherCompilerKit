import panther.*
//import Type.Reference

// object TypeSchemeKind {
//     val Polymorphic = 1
//     val Simple = 2
// }

// class TypeScheme(kind: int, polymorphic: Array[PolymorphicType], simple: Array[SimpleType])

// class PolymorphicType(body: SimpleType)

// object SimpleTypeKind {
//     val Variable = 1
//     val Concrete = 2
// }
// class SimpleType(kind: int, variable: Array[Variable], concrete: Array[ConcreteType]) {
//     def is_any(): bool = kind == SimpleTypeKind.Concrete && concrete[0].kind == ConcreteTypeKind.Any
//     def is_nothing(): bool = kind == SimpleTypeKind.Concrete && concrete[0].kind == ConcreteTypeKind.Nothing
// }

// object ConcreteTypeKind {
//     val Any = 1 // Top
//     val Nothing = 2 // Bottom
//     val Function = 3
//     val Record = 4
//     val Primitive = 5
//     val Array = 5
// }

// enum ConcreteType {
//   case Function(params: Array[SimpleType], returnType: SimpleType)
//   case Array(typ: Type)
//   case Record(fields: Array[RecordField])
//   case Primitive(name: string)
//   case Variable(lower: ConcreteType, upper: ConcreteType)
// )

// Any is Top (super type of all types)
// Nothing is Bottom (sub type of all types)

// https://github.com/LPTK/simpler-sub/blob/simpler-sub/shared/src/main/scala/simplesub/Typer.scala

// ∩ == intersection
// ∪ == union
//
// https://www.youtube.com/watch?v=d10q-b8jNKg
// x gets type variable A
// y gets type variable B

// union of branches gets type variable C

// constraint: A <= int
// constraint: B <= int
// constraint: A, B <= C

// f: A -> B -> C where A <= int, B <= int, A ∪ B <= C
// then (coalesce bounds)
// f: A ∩ int -> B ∩ int -> A ∪ B ∪ C
// then simplify variables
// f: A ∩ int -> A ∩ int -> A

// will be connected to a Declaration or an Expression

case class TypePair(t1: Type, t2: Type)

case class ExprBinder(
    rootSymbol: Symbol,
    binder: Binder,
    conversionClassifier: ConversionClassifier,
    diagnosticBag: DiagnosticBag
) {

  val operators = new BinaryOperators(binder)

//  /**   1. Base rules:
//    *      - Never is a subtype of all types.
//    *      - All types are subtypes of Any.
//    *      - A type is a subtype of itself.
//    *        2. Function Subtyping:
//    *   - Function types are covariant in the return type and contravariant in
//    *     the parameter types:
//    *     - (A1, A2) => R1 is a subtype of (B1, B2) => R2 if:
//    *     - B1 is a subtype of A1 (contravariant in parameters),
//    *     - B2 is a subtype of A2 (contravariant in parameters), and
//    *     - R1 is a subtype of R2 (covariant in the return type).
//    *
//    *   3. Array Subtyping:
//    *      - ArrayType(T1) is a subtype of ArrayType(T2) if T1 is a subtype of
//    *        T2.
//    *        4. Option Subtyping:
//    *      - OptionType(T1) is a subtype of OptionType(T2) if T1 is a subtype of
//    *        T2.
//    *        5. Reference Types:
//    *      - Reference(S1) is a subtype of Reference(S2) if they refer to the
//    *        same symbol (assuming no polymorphism in this example).
//    */
//  def isSubtype(t1: Type, t2: Type): bool = {
//    // Reflexivity: a type is a subtype of itself
//    if (t1 == t2) true
//    else {
//      TypePair(t1, t2) match {
//        // `Never` is a subtype of everything
//        case TypePair(Type.Never, _) => true
//
//        // Everything is a subtype of `Any`
//        case TypePair(_, Type.Any) => true
//
//        // Function subtyping: contravariant in parameters, covariant in return type
//        case TypePair(
//              Type.Function(_, params1, return1),
//              Type.Function(_, params2, return2)
//            ) =>
//          isSubtype(return1, return2) && areParametersSubtype(params2, params1)
//
//        // Array subtyping: inner types must be subtypes
//        case TypePair(
//              Type.Class(_, ns1, name1, args1),
//              Type.Class(_, ns2, name2, args2)
//            ) =>
//          if (ns1 == ns2 && name1 == name2) {
//            areArgsSubtype(args1, args2)
//          } else {
//            false
//          }
//
//        // Error type: not a subtype of anything
//        case TypePair(Type.Error, _) => false
//
//        // Otherwise, not a subtype
//        case _ => false
//      }
//    }
//  }

//  def areArgsSubtype(args1: List[Type], args2: List[Type]): bool = {
//    args1 match {
//      case List.Nil =>
//        args2 match {
//          case List.Nil        => true
//          case List.Cons(_, _) => false
//        }
//      case List.Cons(head1, tail1) =>
//        args2 match {
//          case List.Nil => false
//          case List.Cons(head2, tail2) =>
//            if (isSubtype(head1, head2)) {
//              areArgsSubtype(tail1, tail2)
//            } else {
//              false
//            }
//        }
//    }
//  }

//  def areParametersSubtype(
//      params1: List[BoundParameter],
//      params2: List[BoundParameter]
//  ): bool = {
//    params1 match {
//      case List.Nil =>
//        params2 match {
//          case List.Nil        => true
//          case List.Cons(_, _) => false
//        }
//      case List.Cons(parameter, tail) =>
//        params2 match {
//          case List.Nil => false
//          case List.Cons(parameter2, tail2) =>
//            isSubtype(parameter.typ, parameter2.typ) && areParametersSubtype(
//              tail,
//              tail2
//            )
//        }
//    }
//  }

  def bindUnaryOperator(token: SyntaxToken): UnaryOperatorKind = {
    token.kind match {
      case SyntaxKind.BangToken  => UnaryOperatorKind.LogicalNegation
      case SyntaxKind.PlusToken  => UnaryOperatorKind.Identity
      case SyntaxKind.DashToken  => UnaryOperatorKind.Negation
      case SyntaxKind.TildeToken => UnaryOperatorKind.BitwiseNegation
      case _ =>
        diagnosticBag.reportInvalidOperator(token.location, token.text)
        UnaryOperatorKind.Error
    }
  }

  def bindBinaryOperator(token: SyntaxToken): BinaryOperatorKind = {
    token.kind match {
      case SyntaxKind.AmpersandAmpersandToken => BinaryOperatorKind.LogicalAnd
      case SyntaxKind.BangEqualsToken         => BinaryOperatorKind.NotEquals
      case SyntaxKind.DashToken               => BinaryOperatorKind.Minus
      case SyntaxKind.EqualsEqualsToken       => BinaryOperatorKind.Equals
      case SyntaxKind.GreaterThanEqualsToken =>
        BinaryOperatorKind.GreaterThanOrEqual
      case SyntaxKind.GreaterThanToken    => BinaryOperatorKind.GreaterThan
      case SyntaxKind.LessThanEqualsToken => BinaryOperatorKind.LessThanOrEqual
      case SyntaxKind.LessThanToken       => BinaryOperatorKind.LessThan
      case SyntaxKind.PercentToken        => BinaryOperatorKind.Modulus
      case SyntaxKind.PipePipeToken       => BinaryOperatorKind.LogicalOr
      case SyntaxKind.PlusToken           => BinaryOperatorKind.Plus
      case SyntaxKind.SlashToken          => BinaryOperatorKind.Divide
      case SyntaxKind.StarToken           => BinaryOperatorKind.Multiply
      case _ =>
        diagnosticBag.reportInvalidOperator(token.location, token.text)
        BinaryOperatorKind.Error
    }
  }

  def bind(expr: Expression, scope: Scope): BoundExpression = {
    expr match {
      case node: Expression.ArrayCreationExpression =>
        bindArrayCreationExpression(node, scope)
      case node: Expression.AssignmentExpression =>
        bindAssignmentExpression(node, scope)
      case node: Expression.BinaryExpression =>
        bindBinaryExpression(node, scope)
      case node: Expression.BlockExpression => bindBlockExpression(node, scope)
      case node: Expression.CallExpression  => bindCallExpression(node, scope)
      case node: Expression.ForExpression   => bindForExpression(node, scope)
      case node: Expression.GroupExpression => bindGroupExpression(node, scope)
      case node: Expression.IdentifierName  => bindIdentifierName(node, scope)
      case node: Expression.If              => bindIf(node, scope)
      case node: Expression.LiteralExpression =>
        bindLiteralExpression(node, scope)
      case node: Expression.MemberAccessExpression =>
        bindMemberAccessExpression(node, scope)
      case node: Expression.MatchExpression => bindMatchExpression(node, scope)
      case node: Expression.NewExpression   => bindNewExpression(node, scope)
      case node: Expression.UnaryExpression => bindUnaryExpression(node, scope)
      case node: Expression.UnitExpression  => bindUnitExpression(node, scope)
      case node: Expression.WhileExpression => bindWhileExpression(node, scope)
    }
  }

  def bindConversionExpr(
      expr: Expression,
      toType: Type,
      scope: Scope
  ): BoundExpression = {
    val bound = bind(expr, scope)
    if (bound == BoundExpression.Error) {
      bound
    } else {
      bindConversion(bound, toType, false)
    }
  }

  def bindConversion(
      expr: BoundExpression,
      toType: Type,
      allowExplicit: bool
  ): BoundExpression = {
    if (expr == BoundExpression.Error) {
      expr
    } else {
      val from = binder.getType(expr)
      if (from == Type.Error || toType == Type.Error) {
        BoundExpression.Error
      } else {
        conversionClassifier.classify(from, toType) match {
          case Conversion.Identity => expr
          case Conversion.Implicit =>
            val location = AstUtils.locationOfBoundExpression(expr)
            new BoundExpression.CastExpression(location, expr, toType)
          case Conversion.Explicit =>
            if (allowExplicit) {
              val location = AstUtils.locationOfBoundExpression(expr)
              new BoundExpression.CastExpression(location, expr, toType)
            } else {
              val location = AstUtils.locationOfBoundExpression(expr)
              diagnosticBag.reportCannotConvert(location, from, toType)
              BoundExpression.Error
            }
          case Conversion.None =>
            val location = AstUtils.locationOfBoundExpression(expr)
            diagnosticBag.reportCannotConvert(location, from, toType)
            BoundExpression.Error
        }
      }
    }
  }

  def boundErrorExpression(text: string): BoundExpression = {
    println("\nbinding error: " + text + "\n")
    BoundExpression.Error
  }

  def boundErrorStatement(text: string): BoundStatement = {
    panic("\nbinding error: " + text + "\n")
    BoundStatement.Error
  }

  def bindArrayCreationExpression(
      node: Expression.ArrayCreationExpression,
      scope: Scope
  ): BoundExpression =
    boundErrorExpression("bindArrayCreationExpression")

  def bindAssignmentExpression(
      node: Expression.AssignmentExpression,
      scope: Scope
  ): BoundExpression = {
    val lhs = bind(node.left, scope)
    val rhs = bind(node.right, scope)
    if (lhs == BoundExpression.Error || rhs == BoundExpression.Error) {
      BoundExpression.Error
    } else {
      lhs match {
        case BoundExpression.Variable(location, symbol, returnType) =>
          val lhsType = binder.getType(lhs)

          BoundExpression.Assignment(
            location,
            symbol,
            bindConversion(rhs, lhsType, false)
          )
        case _ =>
          diagnosticBag.reportExpressionIsNotAssignable(
            AstUtils.locationOfBoundExpression(lhs)
          )
          BoundExpression.Error
      }
    }
  }

  def bindBinaryExpression(
      node: Expression.BinaryExpression,
      scope: Scope
  ): BoundExpression = {
    val left = bind(node.left, scope)
    val right = bind(node.right, scope)

    if (left == BoundExpression.Error || right == BoundExpression.Error) {
      BoundExpression.Error
    } else {
      val leftType = binder.getType(left)
      val rightType = binder.getType(right)
      if (leftType == Type.Error || rightType == Type.Error) {
        BoundExpression.Error
      } else {
        val op = bindBinaryOperator(node.operator)
        val resultType = operators.checkBinary(leftType, rightType, op)
        if (resultType == Type.Error) {
          diagnosticBag.reportNoOperatorForOperands(
            node.operator.location,
            node.operator.text,
            leftType,
            rightType
          )
          BoundExpression.Error
        } else {
          BoundExpression.BinaryExpression(
            node.operator.location,
            left,
            op,
            right,
            resultType
          )
        }
      }
    }
  }

  def bindBlockExpression(
      node: Expression.BlockExpression,
      scope: Scope
  ): BoundExpression = {
    val block = scope.newBlock()
    val statements = bindStatements(node.block.statements, block)
    val expr = node.block.expression match {
      case Option.None =>
        BoundExpression.UnitExpression(TextLocationFactory.empty())
      case Option.Some(value) => bind(value, block)
    }

    if (expr == BoundExpression.Error) {
      expr
    } else {
      BoundExpression.Block(statements, expr)
    }
  }

  def isCast(node: Expression): bool = {
    node match {
      case Expression.IdentifierName(
            SimpleNameSyntax.IdentifierNameSyntax(value)
          ) =>
        val name = value.text
        name == "string" || name == "int" || name == "bool" || name == "char"
      case _ => false
    }
  }

  def typesWithError(list: List[Type]): bool = {
    list match {
      case List.Nil => false
      case List.Cons(head, tail) =>
        if (head == Type.Error) true
        else typesWithError(tail)
    }
  }

  def bindCallExpression(
      node: Expression.CallExpression,
      scope: Scope
  ): BoundExpression = {
    if (isCast(node.name)) {
      bindCast(node, scope)
    } else {
      val function = bind(node.name, scope)
      if (function == BoundExpression.Error) {
        BoundExpression.Error
      } else {
        val exprs = fromExpressionList(node.arguments.expressions)
        val args = bindExpressions(exprs, scope)
        bindCall(function, args, scope)
      }
    }
  }

  def bindCall(
      function: BoundExpression,
      args: List[BoundExpression],
      scope: Scope
  ): BoundExpression = {
    // cases:
    // 1. functions
    // 2. generic functions
    // 3. class instantiation
    // 4. generic class instantiation
    // 5. string indexing

    val functionType = binder.getType(function)
    val argTypes = binder.getTypes(args)
    if (typesWithError(List.Cons(functionType, argTypes))) {
      BoundExpression.Error
    } else
      functionType match {
        case func: Type.Function =>
          bindFunctionCall(function, func, args, scope)
        case Type.Class(_, ns, name, _, _) =>
          val location = AstUtils.locationOfBoundExpression(function)
          findConstructor(ns, name) match {
            case Option.None =>
              diagnosticBag.reportNotCallable(location)
              BoundExpression.Error
            case Option.Some(ctor) =>
              binder.getSymbolType(ctor) match {
                case Option.Some(Type.Function(loc, params, _)) =>
                  bindNewExpressionForSymbol(
                    location,
                    ctor,
                    Type.Function(loc, params, functionType),
                    args,
                    scope
                  )
                case _ =>
                  diagnosticBag.reportNotCallable(location)
                  BoundExpression.Error
              }
          }

        case _ =>
          boundErrorExpression("bindCallExpression")
      }
    //    {
    //          val inference = new Inference(diagnosticBag)
    //          val methodType = inference.instantiate(
    //            DictionaryModule.empty(),
    //            binder.getType(function)
    //          )
    //          methodType match {
    //            case Type.Function(_, parameters, returnType) =>
    //              if (parameters.length != args.length) {
    //                diagnosticBag.reportArgumentCountMismatch(
    //                  location,
    //                  parameters.length,
    //                  args.length
    //                )
    //                BoundExpression.Error
    //              } else {
    //                val paramTypes = getParameterTypes(parameters)
    //                val constraints =
    //                  buildConstraints(paramTypes, argTypes, List.Nil)
    //                val func = inference.inferFunction(
    //                  constraints,
    //                  Type.Function(location, parameters, returnType)
    //                )
    //
    //                BoundExpression.CallExpression(
    //                  location,
    //                  function,
    //                  List.Nil,
    //                  args,
    //                  func.returnType
    //                )
    //              }
    //            case Type.Error => BoundExpression.Error
    //            case Type.Class(
    //                  _,
    //                  List.Nil,
    //                  "Array",
    //                  List.Cons(elementType, List.Nil)
    //                ) =>
    //              if (node.openParen.sourceFile.isScala()) {
    //                args match {
    //                  case List.Nil =>
    //                    diagnosticBag.reportArgumentCountMismatch(
    //                      location,
    //                      1,
    //                      args.length
    //                    )
    //                    BoundExpression.Error
    //                  case List.Cons(head, List.Nil) =>
    //                    val arg = bindConversion(head, binder.intType)
    //                    BoundExpression.IndexExpression(
    //                      location,
    //                      function,
    //                      arg,
    //                      elementType
    //                    )
    //                  case List.Cons(_, _) =>
    //                    diagnosticBag.reportArgumentCountMismatch(
    //                      location,
    //                      1,
    //                      args.length
    //                    )
    //                    BoundExpression.Error
    //                }
    //              } else {
    //                diagnosticBag.reportNotCallable(location)
    //                BoundExpression.Error
    //              }
    //
    //            case Type.Class(_, List.Nil, "string", List.Nil) =>
    //              if (node.openParen.sourceFile.isScala()) {
    //                args match {
    //                  case List.Nil =>
    //                    diagnosticBag.reportArgumentCountMismatch(
    //                      location,
    //                      1,
    //                      args.length
    //                    )
    //                    BoundExpression.Error
    //                  case List.Cons(head, List.Nil) =>
    //                    val arg = bindConversion(head, binder.intType)
    //                    BoundExpression.IndexExpression(
    //                      location,
    //                      function,
    //                      arg,
    //                      binder.charType
    //                    )
    //                  case List.Cons(_, _) =>
    //                    diagnosticBag.reportArgumentCountMismatch(
    //                      location,
    //                      1,
    //                      args.length
    //                    )
    //                    BoundExpression.Error
    //                }
    //              } else {
    //                diagnosticBag.reportNotCallable(location)
    //                BoundExpression.Error
    //              }
    //
    //            case Type.Class(_, ns, name, genericTypeParameters) =>
    //              findConstructor(ns, name) match {
    //                case Option.Some(ctor) =>
    //                  bindNewExpressionForSymbol(location, ctor, args, scope)
    //
    //                case Option.None =>
    //                  diagnosticBag.reportNotCallable(location)
    //                  BoundExpression.Error
    //              }
    //            case _ =>
    //              diagnosticBag.reportNotCallable(location)
    //              BoundExpression.Error
    //          }
    //    }
  }

  def bindFunctionCall(
      function: BoundExpression,
      functionType: Type.Function,
      args: List[BoundExpression],
      scope: Scope
  ): BoundExpression = {
    val location = AstUtils.locationOfBoundExpression(function)
    if (functionType.parameters.length != args.length) {
      diagnosticBag.reportArgumentCountMismatch(
        location,
        functionType.parameters.length,
        args.length
      )
      BoundExpression.Error
    } else {
      val boundArgs = bindArguments(functionType.parameters, args, scope)

      BoundExpression.CallExpression(
        location,
        function,
        List.Nil,
        boundArgs,
        functionType.returnType
      )
    }
  }

  def buildConstraints(
      params: List[Type],
      args: List[Type],
      constraints: List[Constraint]
  ): List[Constraint] = {
    params match {
      case List.Nil =>
        args match {
          case List.Nil => constraints
          case List.Cons(_, _) =>
            panic("params is empty but args is not")
        }
      case List.Cons(param, tail) =>
        args match {
          case List.Nil =>
            panic("params is not empty but args is")
          case List.Cons(arg, argTail) =>
            buildConstraints(
              tail,
              argTail,
              List.Cons(Constraint.Equality(param, arg), constraints)
            )
        }
    }
  }

  def bindNewExpressionForSymbol(
      location: TextLocation,
      ctor: Symbol,
      ctorType: Type.Function,
      args: List[BoundExpression],
      scope: Scope
  ): BoundExpression = {
    if (ctorType.parameters.length != args.length) {
      diagnosticBag.reportArgumentCountMismatch(
        location,
        ctorType.parameters.length,
        args.length
      )
      BoundExpression.Error
    } else {
      val boundArgs = bindArguments(ctorType.parameters, args, scope)
      BoundExpression.NewExpression(
        location,
        ctor,
        List.Nil,
        boundArgs,
        ctorType.returnType
      )
    }
  }

  def findConstructor(ns: List[string], name: string): Option[Symbol] = {
    rootSymbol.findSymbol(ns, name) match {
      case Option.Some(symbol) =>
        symbol.lookupMember(".ctor")
      case Option.None => Option.None
    }
  }

  def bindCast(
      expression: Expression.CallExpression,
      scope: Scope
  ): BoundExpression = {
    expression.arguments.expressions match {
      case List.Nil =>
        diagnosticBag.reportArgumentCountMismatch(
          AstUtils.locationOfExpression(expression.name),
          1,
          0
        )
        BoundExpression.Error
      case List.Cons(head, tail) =>
        if (!tail.isEmpty) {
          diagnosticBag.reportArgumentCountMismatch(
            AstUtils.locationOfExpression(expression.name),
            1,
            tail.length + 1
          )
          BoundExpression.Error
        } else {
          val lhs = bind(expression.name, scope)
          val arg = bind(head.expression, scope)
          val location = AstUtils.locationOfBoundExpression(lhs)
          val targetType = binder.getType(lhs)

          if (targetType == Type.Error) {
            BoundExpression.Error
          } else {
            bindConversion(arg, targetType, true)
          }
        }
    }
  }

  def getParameterTypes(parameters: List[BoundParameter]): List[Type] = {
    parameters match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        List.Cons(head.typ, getParameterTypes(tail))
    }
  }

  def bindArguments(
      parameters: List[BoundParameter],
      arguments: List[BoundExpression],
      scope: Scope
  ): List[BoundExpression] = {
    parameters match {
      case List.Nil =>
        arguments match {
          case List.Nil => List.Nil
          case List.Cons(_, _) =>
            panic("parameters is empty but arguments is not")
        }
      case List.Cons(head, tail) =>
        arguments match {
          case List.Nil =>
            panic("parameters is not empty but arguments is")
          case List.Cons(argHead, argTail) =>
            val boundArg = bindConversion(argHead, head.typ, false)
            List.Cons(boundArg, bindArguments(tail, argTail, scope))
        }
    }
  }

  def fromExpressionList(
      list: List[ExpressionItemSyntax]
  ): List[Expression] =
    list match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        val expr = head.expression
        List.Cons(expr, fromExpressionList(tail))
    }

  def bindExpressions(
      list: List[Expression],
      scope: Scope
  ): List[BoundExpression] =
    list match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        val expr = bind(head, scope)
        List.Cons(expr, bindExpressions(tail, scope))
    }

  def bindForExpression(
      node: Expression.ForExpression,
      scope: Scope
  ): BoundExpression = {

    val blockScope = scope.newBlock()
    val lowerBound = bindConversionExpr(node.fromExpr, binder.intType, scope)
    val upperBound = bindConversionExpr(node.toExpr, binder.intType, scope)
    val identifier = node.identifier

    blockScope.defineLocal(identifier.text, identifier.location) match {
      case Either.Left(value) =>
        panic("symbol already defined even though that should not be possible")
      case Either.Right(variable) =>
        binder.setSymbolType(variable, binder.intType, Option.None)

        val body = bind(node.body, blockScope)

        BoundExpression.ForExpression(
          node.forKeyword.location,
          variable,
          lowerBound,
          upperBound,
          body
        )
    }
  }

  def bindGroupExpression(
      node: Expression.GroupExpression,
      scope: Scope
  ): BoundExpression =
    bind(node.expression, scope)

  def bindIdentifierName(
      node: Expression.IdentifierName,
      scope: Scope
  ): BoundExpression = {
    node.value match {
      case SimpleNameSyntax.IdentifierNameSyntax(identifier) =>
        bindIdentifier(identifier, scope)
      case generic: SimpleNameSyntax.GenericNameSyntax => ???
      case SimpleNameSyntax.ScalaAliasSyntax(open, name, arrow, alias, close) =>
        ???
      case SimpleNameSyntax.AliasSyntax(name, asKeyword, alias) => ???
    }
  }

  def bindIdentifier(
      identifier: SyntaxToken,
      scope: Scope
  ): BoundExpression = {
    scope.lookup(identifier.text) match {
      case Option.None =>
        diagnosticBag.reportSymbolNotFound(identifier.location, identifier.text)
        BoundExpression.Error
      case Option.Some(symbol) =>
        val symbolType = binder.getSymbolType(symbol)
        BoundExpression.Variable(
          identifier.location,
          symbol,
          symbolType
        )
    }
  }

  def bindIf(node: Expression.If, scope: Scope): BoundExpression = {
    val cond = bindConversionExpr(node.condition, binder.boolType, scope)
    val thenExpr = bind(node.thenExpr, scope)
    if (cond == BoundExpression.Error || thenExpr == BoundExpression.Error) {
      BoundExpression.Error
    } else {
      node.elseExpr match {
        case Option.None =>
          val boundThen = bindConversion(thenExpr, binder.unitType, false)
          BoundExpression.IfExpression(
            node.ifKeyword.location,
            cond,
            boundThen,
            Option.None,
            binder.unitType
          )
        case Option.Some(value) =>
          val resultType = binder.getType(thenExpr)
          val boundElse =
            bindConversionExpr(value.expression, resultType, scope)
          if (boundElse == BoundExpression.Error) {
            BoundExpression.Error
          } else {
            BoundExpression.IfExpression(
              node.ifKeyword.location,
              cond,
              thenExpr,
              Option.Some(boundElse),
              resultType
            )
          }
      }
    }
  }

  def bindLiteralExpression(
      node: Expression.LiteralExpression,
      scope: Scope
  ): BoundExpression = {
    node.value match {
      case SyntaxTokenValue.Number(value) =>
        BoundExpression.IntLiteral(node.token.location, value)
      case SyntaxTokenValue.Boolean(value) =>
        BoundExpression.BooleanLiteral(node.token.location, value)
      case SyntaxTokenValue.String(value) =>
        BoundExpression.StringLiteral(node.token.location, value)
      case SyntaxTokenValue.Character(value) =>
        BoundExpression.CharacterLiteral(node.token.location, value)
      case _ =>
        panic("unexpected literal expression")
        BoundExpression.Error
    }
  }

  def bindMemberAccessExpression(
      node: Expression.MemberAccessExpression,
      scope: Scope
  ): BoundExpression = {
    val left = bind(node.left, scope)

    node.right match {
      case SimpleNameSyntax.GenericNameSyntax(identifier, typeArgumentlist) =>
        ???
      case SimpleNameSyntax.ScalaAliasSyntax(open, name, arrow, alias, close) =>
        ???
      case SimpleNameSyntax.AliasSyntax(name, asKeyword, alias) => ???
      case SimpleNameSyntax.IdentifierNameSyntax(right)         =>
        //    val memberName = right.text
        val leftType = binder.getType(left)

        bindMemberForSymbolAndType(leftType, right) match {
          case Option.None => BoundExpression.Error
          case Option.Some(Tuple2(member, typ)) =>
            BoundExpression.MemberAccess(
              right.location,
              left,
              member,
              typ
            )
        }
    }
  }

  def bindMemberForSymbolAndType(
      leftType: Type,
      right: SyntaxToken
  ): Option[Tuple2[Symbol, Type]] = {
    leftType match {
      case Type.Class(_, ns, name, _, _) =>
        rootSymbol.findSymbol(ns, name) match {
          case Option.None =>
            diagnosticBag.reportSymbolNotFoundForType(
              right.location,
              leftType,
              right.text
            )
            Option.None
          case Option.Some(symbol) =>
            symbol.lookupMember(right.text) match {
              case Option.None =>
                diagnosticBag.reportSymbolNotFoundForType(
                  right.location,
                  leftType,
                  right.text
                )
                Option.None
              case Option.Some(member) =>
                binder.getSymbolType(member) match {
                  case Option.None => Option.None
                  case Option.Some(typ) =>
                    Option.Some(Tuple2(member, typ))
                }
            }
        }
      case Type.Error => Option.None
      case _ =>
        diagnosticBag.reportSymbolNotFoundForType(
          right.location,
          leftType,
          right.text
        )
        Option.None
    }
  }

  def bindMatchExpression(
      node: Expression.MatchExpression,
      scope: Scope
  ): BoundExpression =
    boundErrorExpression("bindMatchExpression")

  def bindNewExpression(
      node: Expression.NewExpression,
      scope: Scope
  ): BoundExpression = {
    val instantiationType = binder.bindTypeName(node.name, scope)
    instantiationType match {
      case Type.Class(_, ns, name, args, _) =>
        findConstructor(ns, name) match {
          case Option.None =>
            diagnosticBag.reportSymbolNotFound(
              AstUtils.locationOfName(node.name),
              name
            )
            BoundExpression.Error
          case Option.Some(ctor) =>
            val args = bindExpressions(
              fromExpressionList(node.arguments.expressions),
              scope
            )
            val location = AstUtils.locationOfExpression(node)

            val ctorType = binder.getSymbolType(ctor)
            ctorType match {
              case Option.Some(Type.Function(loc, params, _)) =>
                bindNewExpressionForSymbol(
                  location,
                  ctor,
                  Type.Function(loc, params, instantiationType),
                  args,
                  scope
                )
              case _ =>
                diagnosticBag.reportNotCallable(location)
                BoundExpression.Error
            }
        }
      case Type.Error => BoundExpression.Error
      case _ =>
        panic("expected named type, got " + instantiationType)
    }
  }

  def bindUnaryExpression(
      node: Expression.UnaryExpression,
      scope: Scope
  ): BoundExpression = {
    val op = bindUnaryOperator(node.operator)
    val operand = bind(node.expression, scope)
    if (operand == BoundExpression.Error) {
      BoundExpression.Error
    } else {
      val operandType = binder.getType(operand)
      if (operandType == Type.Error) {
        BoundExpression.Error
      } else {
        val resultType = operators.checkUnary(operandType, op)
        if (resultType == Type.Error) {
          diagnosticBag.reportNoOperatorForOperand(
            node.operator.location,
            node.operator.text,
            operandType
          )
          BoundExpression.Error
        } else {
          BoundExpression.UnaryExpression(
            node.operator.location,
            op,
            operand,
            resultType
          )
        }
      }
    }

  }

  def bindUnitExpression(
      node: Expression.UnitExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.UnitExpression(
      node.openParen.location.merge(node.closeParen.location)
    )

  def bindWhileExpression(
      node: Expression.WhileExpression,
      scope: Scope
  ): BoundExpression = {
    val cond = bind(node.condition, scope)
    val body = bind(node.body, scope)
    new BoundExpression.WhileExpression(node.whileKeyword.location, cond, body)
  }

  def bindStatement(
      statement: StatementSyntax,
      scope: Scope
  ): BoundStatement = {
    statement match {
      case stmt: StatementSyntax.VariableDeclarationStatement =>
        bindVariableDeclarationStatement(stmt, scope)
      case stmt: StatementSyntax.BreakStatement =>
        bindBreakStatement(stmt, scope)
      case stmt: StatementSyntax.ContinueStatement =>
        bindContinueStatement(stmt, scope)
      case stmt: StatementSyntax.ExpressionStatement =>
        bindExpressionStatement(stmt, scope)
    }
  }

  def bindVariableDeclarationStatement(
      statement: StatementSyntax.VariableDeclarationStatement,
      scope: Scope
  ): BoundStatement = {
    val identifier = statement.identifier
    val name = identifier.text
    val location = identifier.location
    scope.defineLocal(name, location) match {
      case Either.Left(originalLocation) =>
        // symbol is already defined in this scope
        diagnosticBag.reportDuplicateDefinition(
          name,
          originalLocation,
          location
        )
        BoundStatement.Error
      case Either.Right(symbol) =>
        // symbol was created so lets bind it
        val expr = bind(statement.expression, scope)

        statement.typeAnnotation match {
          case Option.None =>
            // no type annotation, so we need to infer the type from the expr
            val typ = binder.getType(expr)
            binder.setSymbolType(symbol, typ, Option.None)
            BoundStatement.VariableDeclaration(symbol, false, typ, expr)

          case Option.Some(value) =>
            val annotatedType = binder.bindTypeName(value.typ, scope)
            binder.setSymbolType(symbol, annotatedType, Option.None)

            // make sure we can convert the expression
            val boundExpr = bindConversion(expr, annotatedType, false)

            BoundStatement.VariableDeclaration(
              symbol,
              false,
              annotatedType,
              boundExpr
            )
        }
    }
  }
  def bindBreakStatement(
      statement: StatementSyntax.BreakStatement,
      scope: Scope
  ): BoundStatement = {
    boundErrorStatement("bindBreakStatement")
  }
  def bindContinueStatement(
      statement: StatementSyntax.ContinueStatement,
      scope: Scope
  ): BoundStatement = {
    boundErrorStatement("bindContinueStatement")
  }
  def bindExpressionStatement(
      statement: StatementSyntax.ExpressionStatement,
      scope: Scope
  ): BoundStatement = {
    val expr = bind(statement.expression, scope)
    BoundStatement.ExpressionStatement(expr)
  }

  def bindStatements(
      statements: List[StatementSyntax],
      scope: Scope
  ): List[BoundStatement] = {
    statements match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        val boundHead = bindStatement(head, scope)
        val boundTail = bindStatements(tail, scope)
        List.Cons(boundHead, boundTail)
    }
  }
}
