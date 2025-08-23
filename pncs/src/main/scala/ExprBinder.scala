import BoundLeftHandSide.Index
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
      case SyntaxKind.AmpersandToken          => BinaryOperatorKind.BitwiseAnd
      case SyntaxKind.BangEqualsToken         => BinaryOperatorKind.NotEquals
      case SyntaxKind.CaretToken              => BinaryOperatorKind.BitwiseXor
      case SyntaxKind.DashToken               => BinaryOperatorKind.Minus
      case SyntaxKind.EqualsEqualsToken       => BinaryOperatorKind.Equals
      case SyntaxKind.GreaterThanEqualsToken =>
        BinaryOperatorKind.GreaterThanOrEqual
      case SyntaxKind.GreaterThanToken => BinaryOperatorKind.GreaterThan
      case SyntaxKind.GreaterThanGreaterThanToken =>
        BinaryOperatorKind.ShiftRight
      case SyntaxKind.LessThanEqualsToken => BinaryOperatorKind.LessThanOrEqual
      case SyntaxKind.LessThanToken       => BinaryOperatorKind.LessThan
      case SyntaxKind.LessThanLessThanToken => BinaryOperatorKind.ShiftLeft
      case SyntaxKind.PercentToken          => BinaryOperatorKind.Modulus
      case SyntaxKind.PipePipeToken         => BinaryOperatorKind.LogicalOr
      case SyntaxKind.PipeToken             => BinaryOperatorKind.BitwiseOr
      case SyntaxKind.PlusToken             => BinaryOperatorKind.Plus
      case SyntaxKind.SlashToken            => BinaryOperatorKind.Divide
      case SyntaxKind.StarToken             => BinaryOperatorKind.Multiply
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
      case node: Expression.CallExpression =>
        bindCallExpression(node, scope) match {
          case Result.Error(value)                 => value
          case Result.Success(Either.Left(value))  => value
          case Result.Success(Either.Right(value)) => value
        }
      case node: Expression.CastExpression  => bindCast(node, scope)
      case node: Expression.ForExpression   => bindForExpression(node, scope)
      case node: Expression.GroupExpression => bindGroupExpression(node, scope)
      case node: Expression.IdentifierName =>
        bindIdentifierName(node, scope) match {
          case Result.Error(value)   => value
          case Result.Success(value) => value
        }
      case node: Expression.If => bindIf(node, scope)
      case node: Expression.LiteralExpression =>
        bindLiteralExpression(node, scope)
      case node: Expression.MemberAccessExpression =>
        bindMemberAccessExpression(node, scope) match {
          case Result.Error(value)   => value
          case Result.Success(value) => value
        }
      case node: Expression.MatchExpression => bindMatchExpression(node, scope)
      case node: Expression.NewExpression =>
        bindNewExpression(node, scope) match {
          case Result.Error(value)   => value
          case Result.Success(value) => value
        }
      case node: Expression.UnaryExpression => bindUnaryExpression(node, scope)
      case node: Expression.UnitExpression  => bindUnitExpression(node, scope)
      case node: Expression.WhileExpression => bindWhileExpression(node, scope)
    }
  }

  def bindLHS(
      expr: Expression,
      scope: Scope
  ): Result[BoundExpression.Error, BoundLeftHandSide] = {
    expr match {
      case node: Expression.IdentifierName =>
        bindIdentifierName(node, scope) match {
          case Result.Error(value) => Result.Error(value)
          case Result.Success(value) =>
            val location = AstUtils.locationOfExpression(node)
            Result.Success(BoundLeftHandSide.Variable(location, value.symbol))
        }
      case node: Expression.MemberAccessExpression =>
        bindMemberAccessExpression(node, scope) match {
          case Result.Error(value) => Result.Error(value)
          case Result.Success(value) =>
            Result.Success(BoundLeftHandSide.MemberAccess(value))
        }
      case node: Expression.CallExpression =>
        bindCallExpression(node, scope) match {
          case Result.Error(value) => Result.Error(value)
          case Result.Success(Either.Left(value)) =>
            Result.Success(
              BoundLeftHandSide.Call(value)
            )
          case Result.Success(Either.Right(value)) =>
            ???
        }
      case node: Expression.NewExpression =>
        bindNewExpression(node, scope) match {
          case Result.Error(value) => Result.Error(value)
          case Result.Success(value) =>
            Result.Success(BoundLeftHandSide.New(value))
        }
      case _ =>
        panic(
          "bindLHS called with non-LHS expression: " + expr.toString()
        )
        Result.Error(
          BoundExpression.Error(
            "Expected left-hand side expression but got: " + expr.toString()
          )
        )
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
    expr match {
      case _: BoundExpression.Error => expr
      case _ =>
        val from = binder.getType(expr)
        Tuple2(from, toType) match {
          case Tuple2(Type.Error(message), _) =>
            BoundExpression.Error(message)
          case Tuple2(_, Type.Error(message)) =>
            BoundExpression.Error(message)
          case _ =>
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
                  BoundExpression.Error(
                    "Cannot convert from " + from
                      .toString() + " to " + toType
                      .toString()
                  )
                }
              case Conversion.None =>
                val location = AstUtils.locationOfBoundExpression(expr)
                diagnosticBag.reportCannotConvert(location, from, toType)
                BoundExpression.Error(
                  "Cannot convert from " + from.toString() + " to " + toType
                    .toString()
                )
            }
        }
    }
  }

  def boundErrorExpression(text: string): BoundExpression = {
    println("\nbinding error: " + text + "\n")
    BoundExpression.Error("binding error: " + text)
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
    val lhs = bindLHS(node.left, scope)
    val rhs = bind(node.right, scope)
    Tuple2(lhs, rhs) match {
      case Tuple2(Result.Error(error), _) =>
        error
      case Tuple2(_, BoundExpression.Error(_)) =>
        rhs
      case Tuple2(Result.Success(lhs), rhs) =>
        getLHSType(lhs) match {
          case Type.Error(message) => BoundExpression.Error(message)
          case lhsType =>
            BoundExpression.Assignment(
              AstUtils.locationOfExpression(node),
              lhs,
              bindConversion(rhs, lhsType, false)
            )
        }

      case _ =>
        val location = AstUtils.locationOfExpression(node.left)
        diagnosticBag.reportExpressionIsNotAssignable(location)
        BoundExpression.Error("Expression is not assignable: " + location)
    }
  }

  def bindBinaryExpression(
      node: Expression.BinaryExpression,
      scope: Scope
  ): BoundExpression = {
    val left = bind(node.left, scope)
    val right = bind(node.right, scope)

    Tuple2(left, right) match {
      case Tuple2(BoundExpression.Error(_), _) =>
        left
      case Tuple2(_, BoundExpression.Error(_)) =>
        right
      case _ =>
        val leftType = binder.getType(left)
        val rightType = binder.getType(right)
        Tuple2(leftType, rightType) match {
          case Tuple2(Type.Error(message), _) =>
            BoundExpression.Error(message)
          case Tuple2(_, Type.Error(message)) =>
            BoundExpression.Error(message)

          case _ =>
            val op = bindBinaryOperator(node.operator)
            val resultType = operators.checkBinary(leftType, rightType, op)
            if (resultType == Type.Error) {
              diagnosticBag.reportNoOperatorForOperands(
                node.operator.location,
                node.operator.text,
                leftType,
                rightType
              )
              BoundExpression.Error(
                "No operator for operands: " + node.operator.text + " for types: " +
                  leftType.toString() + " and " + rightType.toString()
              )
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
  ): Result[
    BoundExpression.Error,
    Either[BoundExpression.CallExpression, BoundExpression.NewExpression]
  ] = {
    bindLHS(node.name, scope) match {
      case Result.Error(error) => Result.Error(error)

      case Result.Success(function) =>
        val exprs = fromExpressionList(node.arguments.expressions)
        val args = bindExpressions(exprs, scope)
        bindCall(function, args, scope)
    }
  }

  def bindCall(
      function: BoundLeftHandSide,
      args: List[BoundExpression],
      scope: Scope
  ): Result[
    BoundExpression.Error,
    Either[BoundExpression.CallExpression, BoundExpression.NewExpression]
  ] = {
    // cases:
    // 1. functions
    // 2. generic functions
    // 3. class instantiation
    // 4. generic class instantiation
    // 5. string indexing

    val functionType = getLHSType(function)
    val argTypes = binder.getTypes(args)
    if (typesWithError(List.Cons(functionType, argTypes))) {
      Result.Error(
        BoundExpression.Error(
          "Cannot bind call expression because of type errors"
        )
      )
    } else
      functionType match {
        case func: Type.Function =>
          bindFunctionCall(function, func, args, scope) match {
            case Result.Error(value)   => Result.Error(value)
            case Result.Success(value) => Result.Success(Either.Left(value))
          }
        case Type.Class(_, ns, name, _, symbol) =>
          val location = AstUtils.locationOfBoundLeftHandSide(function)
          findConstructor(symbol) match {
            case Option.None =>
              diagnosticBag.reportNotCallable(location)
              Result.Error(
                BoundExpression.Error(
                  "Cannot find constructor for class: " + name
                )
              )
            case Option.Some(ctor) =>
              binder.tryGetSymbolType(ctor) match {
                case Option.Some(Type.Function(loc, params, _)) =>

                  bindNewExpressionForSymbol(
                    location,
                    ctor,
                    Type.Function(loc, params, functionType),
                    args,
                    scope
                  ) match {
                    case Result.Error(value) => Result.Error(value)
                    case Result.Success(value) =>
                      Result.Success(Either.Right(value))
                  }
                case _ =>
                  diagnosticBag.reportNotCallable(location)
                  Result.Error(
                    BoundExpression.Error(
                      "Constructor symbol does not have a function type: " + name
                    )
                  )
              }
          }

        case _ =>
          panic(
            "\nbinding error: bindCallExpression: functionType = " + functionType
              .toString() + "\n"
          )

          ???
          Result.Error(
            BoundExpression.Error(
              "binding error: bindCallExpression"
            )
          )
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
      function: BoundLeftHandSide,
      functionType: Type.Function,
      args: List[BoundExpression],
      scope: Scope
  ): Result[BoundExpression.Error, BoundExpression.CallExpression] = {
    val location = AstUtils.locationOfBoundLeftHandSide(function)
    if (functionType.parameters.length != args.length) {
      diagnosticBag.reportArgumentCountMismatch(
        location,
        functionType.parameters.length,
        args.length
      )
      Result.Error(
        BoundExpression.Error(
          "Argument count mismatch: expected " + functionType.parameters.length +
            " but got " + args.length
        )
      )
    } else {
      val boundArgs = bindArguments(functionType.parameters, args, scope)

      function match {
        case BoundLeftHandSide.Index(expression) => ???
        case BoundLeftHandSide.MemberAccess(access) =>
          val receiver = if (access.member.isStatic()) {
            Option.None
          } else {
            Option.Some(access.receiver)
          }
          Result.Success(
            BoundExpression.CallExpression(
              location,
              receiver,
              access.member,
              List.Nil, // TODO: generic arguments
              boundArgs,
              functionType.returnType
            )
          )
        case BoundLeftHandSide.Variable(location, symbol) =>
          Result.Success(
            BoundExpression.CallExpression(
              location,
              Option.None,
              symbol,
              List.Nil,
              boundArgs,
              functionType.returnType
            )
          )
        case _ => ???
      }
    }
  }

//  def buildConstraints(
//      params: List[Type],
//      args: List[Type],
//      constraints: List[Constraint]
//  ): List[Constraint] = {
//    params match {
//      case List.Nil =>
//        args match {
//          case List.Nil => constraints
//          case List.Cons(_, _) =>
//            panic("params is empty but args is not")
//        }
//      case List.Cons(param, tail) =>
//        args match {
//          case List.Nil =>
//            panic("params is not empty but args is")
//          case List.Cons(arg, argTail) =>
//            buildConstraints(
//              tail,
//              argTail,
//              List.Cons(Constraint.Equality(param, arg), constraints)
//            )
//        }
//    }
//  }

  def bindNewExpressionForSymbol(
      location: TextLocation,
      ctor: Symbol,
      ctorType: Type.Function,
      args: List[BoundExpression],
      scope: Scope
  ): Result[BoundExpression.Error, BoundExpression.NewExpression] = {
    if (ctorType.parameters.length != args.length) {
      diagnosticBag.reportArgumentCountMismatch(
        location,
        ctorType.parameters.length,
        args.length
      )
      Result.Error(
        BoundExpression.Error(
          "Argument count mismatch: expected " + ctorType.parameters.length +
            " but got " + args.length
        )
      )
    } else {
      val boundArgs = bindArguments(ctorType.parameters, args, scope)
      Result.Success(
        BoundExpression.NewExpression(
          location,
          ctor,
          List.Nil,
          boundArgs,
          ctorType.returnType
        )
      )
    }
  }

  def findConstructor(symbol: Symbol): Option[Symbol] =
    symbol.lookupMember(".ctor")

  def bindCast(
      cast: Expression.CastExpression,
      scope: Scope
  ): BoundExpression = {
    val expr = bind(cast.expression, scope)
    val typ = binder.bindTypeName(cast.typ, scope)
    bindConversion(expr, typ, true)
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
        binder.setSymbolType(variable, binder.intType)

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
  ): Result[BoundExpression.Error, BoundExpression.Variable] = {
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
  ): Result[BoundExpression.Error, BoundExpression.Variable] = {
    scope.lookup(identifier.text) match {
      case Option.None =>
        diagnosticBag.reportSymbolNotFound(identifier.location, identifier.text)
        Result.Error(
          BoundExpression.Error(
            "Symbol not found: " + identifier.text + " at " + identifier.location
          )
        )
      case Option.Some(symbol) =>
        val symbolType = binder.tryGetSymbolType(symbol)
        Result.Success(
          BoundExpression.Variable(
            identifier.location,
            symbol,
            symbolType
          )
        )
    }
  }

  def bindIf(node: Expression.If, scope: Scope): BoundExpression = {
    val cond = bindConversionExpr(node.condition, binder.boolType, scope)
    val thenExpr = bind(node.thenExpr, scope)
    Tuple2(cond, thenExpr) match {
      case Tuple2(BoundExpression.Error(_), _) =>
        cond
      case Tuple2(_, BoundExpression.Error(_)) =>
        thenExpr
      case _ =>
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
            boundElse match {
              case _: BoundExpression.Error => boundElse
              case _ =>
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
        BoundExpression.Int(node.token.location, value)
      case SyntaxTokenValue.Boolean(value) =>
        BoundExpression.Boolean(node.token.location, value)
      case SyntaxTokenValue.String(value) =>
        BoundExpression.String(node.token.location, value)
      case SyntaxTokenValue.Character(value) =>
        BoundExpression.Character(node.token.location, value)
      case _ =>
        panic("unexpected literal expression")
        BoundExpression.Error("unexpected literal expression")
    }
  }

  def getLHSType(lhs: BoundLeftHandSide): Type = {
    lhs match {
      case BoundLeftHandSide.Variable(_, variable) =>
        binder.getSymbolType(variable)
      case BoundLeftHandSide.MemberAccess(expression) =>
        binder.getType(expression)
      case BoundLeftHandSide.Index(expression) =>
        binder.getType(expression)
      case BoundLeftHandSide.Call(expression) =>
        binder.getType(expression)
      case BoundLeftHandSide.New(expression) =>
        binder.getType(expression)
    }
  }

  def bindMemberAccessExpression(
      node: Expression.MemberAccessExpression,
      scope: Scope
  ): Result[BoundExpression.Error, BoundExpression.MemberAccess] = {
    // For member access, the left side doesn't need to be a left-hand side
    // It just needs to be a valid expression that produces a value
    val leftExpr = bindLHS(node.left, scope)
    leftExpr match {
      case Result.Error(error) =>
        Result.Error(error)
      case Result.Success(leftExpr) =>
        getLHSType(leftExpr) match {
          case Type.Error(message) =>
            Result.Error(BoundExpression.Error(message))
          case leftType =>
            // Check what kind of expression we have and create appropriate LHS wrapper

            node.right match {
              case SimpleNameSyntax.GenericNameSyntax(
                    right,
                    typeArgumentlist
                  ) =>
                val typeArguments =
                  binder.bindTypeArgumentList(typeArgumentlist.arguments, scope)

                bindMemberForSymbolAndType(leftType, right) match {
                  case Either.Left(message) =>
                    Result.Error(BoundExpression.Error(message))
                  case Either.Right(Tuple2(member, typ)) =>
                    Result.Success(
                      BoundExpression.MemberAccess(
                        right.location,
                        leftExpr,
                        member,
                        typeArguments,
                        typ
                      )
                    )
                }
              case SimpleNameSyntax.ScalaAliasSyntax(
                    open,
                    name,
                    arrow,
                    alias,
                    close
                  ) =>
                ???
              case SimpleNameSyntax.AliasSyntax(name, asKeyword, alias) => ???
              case SimpleNameSyntax.IdentifierNameSyntax(right) =>
                bindMemberForSymbolAndType(leftType, right) match {
                  case Either.Left(message) =>
                    Result.Error(BoundExpression.Error(message))
                  case Either.Right(Tuple2(member, typ)) =>
                    Result.Success(
                      BoundExpression.MemberAccess(
                        right.location,
                        leftExpr,
                        member,
                        List.Nil,
                        typ
                      )
                    )
                }
            }
        }
    }
  }

  def bindMemberForSymbolAndType(
      leftType: Type,
      right: SyntaxToken
  ): Either[string, Tuple2[Symbol, Type]] = {
    binder.getTypeSymbol(leftType) match {
      case Option.None =>
        diagnosticBag.reportSymbolNotFoundForType(
          right.location,
          leftType,
          right.text
        )
        Either.Left(
          "Symbol not found for type: " + leftType
            .toString() + " for member: " + right.text
        )
      case Option.Some(symbol) =>
        symbol.lookupMember(right.text) match {
          case Option.None =>
            diagnosticBag.reportSymbolNotFoundForType(
              right.location,
              leftType,
              right.text
            )
            Either.Left(
              "Member not found: " + right.text + " for type: " + leftType
                .toString()
            )
          case Option.Some(member) =>
            binder.tryGetSymbolType(member) match {
              case Option.None =>
                // TODO: i think  we should report an error here
                Either.Left(
                  "Symbol type not found for member: " + right.text +
                    " for type: " + leftType.toString()
                )
              case Option.Some(typ) =>
                Either.Right(Tuple2(member, typ))
            }
        }
    }
  }

  def bindPattern(
      pattern: PatternSyntax,
      scope: Scope
  ): Result[BoundExpression.Error, BoundPattern] = {
    pattern match {
      case PatternSyntax.Literal(token) =>
        token.value match {
          case SyntaxTokenValue.Number(value) =>
            Result.Success(
              BoundPattern.Literal(
                BoundLiteral.Int(token.location, value)
              )
            )
          case SyntaxTokenValue.String(value) =>
            Result.Success(
              BoundPattern.Literal(
                BoundLiteral.String(token.location, value)
              )
            )
          case SyntaxTokenValue.Boolean(value) =>
            Result.Success(
              BoundPattern.Literal(
                BoundLiteral.Bool(token.location, value)
              )
            )
          case SyntaxTokenValue.Character(value) =>
            Result.Success(
              BoundPattern.Literal(
                BoundLiteral.Char(token.location, value)
              )
            )
          case _ =>
            diagnosticBag.reportInvalidPattern(token.location)
            Result.Error(
              BoundExpression.Error(
                "Invalid literal pattern: " + token.text
              )
            )
        }
      case PatternSyntax.Discard(_) =>
        Result.Success(BoundPattern.Discard)
      case PatternSyntax.Identifier(identifier, typeAnnotation) =>
        // Create a new local variable for the pattern binding
        scope.defineLocal(identifier.text, identifier.location) match {
          case Either.Left(originalLocation) =>
            diagnosticBag.reportDuplicateDefinition(
              identifier.text,
              originalLocation,
              identifier.location
            )
            Result.Error(
              BoundExpression.Error(
                "Duplicate pattern variable: " + identifier.text
              )
            )
          case Either.Right(symbol) =>
            panic(
              "this isnt quite right, this should be a type check, not setting the type here"
            )
            // Set the symbol type from the type annotation
            val boundType = binder.bindTypeName(typeAnnotation.typ, scope)
            binder.setSymbolType(symbol, boundType)
            Result.Success(BoundPattern.Variable(symbol))
        }
      case PatternSyntax.Type(typ) =>
        // For type patterns, we create a wildcard pattern
        // Type checking will be handled elsewhere
        Result.Success(BoundPattern.Discard)
      case PatternSyntax.Extract(_, _, patterns, _) =>
        // For now, treat extract patterns as wildcards
        // Full ADT support would require more complex pattern matching
        Result.Success(BoundPattern.Discard)
    }
  }

  def bindMatchCase(
      matchCase: MatchCaseSyntax,
      scope: Scope
  ): Result[BoundExpression.Error, BoundMatchCase] = {
    // Create a new scope for this case to allow pattern variables
    val caseScope = scope.newBlock()

    bindPattern(matchCase.pattern, caseScope) match {
      case Result.Error(error)          => Result.Error(error)
      case Result.Success(boundPattern) =>
        // Bind the statements and expression within the case scope
        val statements = bindStatements(matchCase.block.statements, caseScope)
        val resultExpr = matchCase.block.expression match {
          case Option.None =>
            BoundExpression.UnitExpression(TextLocationFactory.empty())
          case Option.Some(expr) => bind(expr, caseScope)
        }

        resultExpr match {
          case error: BoundExpression.Error => Result.Error(error)
          case _ =>
            val caseResult = if (statements.isEmpty) {
              resultExpr
            } else {
              BoundExpression.Block(statements, resultExpr)
            }
            Result.Success(
              BoundMatchCase(
                matchCase.caseKeyword.location,
                boundPattern,
                caseResult
              )
            )
        }
    }
  }

  def bindMatchCases(
      head: MatchCaseSyntax,
      tail: List[MatchCaseSyntax],
      scope: Scope
  ): Result[BoundExpression.Error, NonEmptyList[BoundMatchCase]] = {
    bindMatchCase(head, scope) match {
      case Result.Error(expr) =>
        Result.Error(expr)
      case Result.Success(boundCase) =>
        tail match {
          case List.Nil =>
            Result.Success(NonEmptyList(boundCase, List.Nil))
          case List.Cons(head, tail) =>
            bindMatchCases(head, tail, scope) match {
              case Result.Error(expr) => Result.Error(expr)
              case Result.Success(tailCases) =>
                Result.Success(NonEmptyList(boundCase, tailCases.toList()))
            }
        }
    }
  }

  def calculateMatchResultType(typ: Type, tail: List[BoundMatchCase]): Type = {
    tail match {
      case List.Nil              => typ
      case List.Cons(head, tail) =>
        // Multiple cases - for now, use Any as the union type
        // In a more sophisticated implementation, this would compute
        // the least upper bound (LUB) of all case types
        val headType = binder.getType(head.result)

        calculateMatchResultType(Types.union(typ, headType), tail)
    }
  }

  def bindMatchExpression(
      node: Expression.MatchExpression,
      scope: Scope
  ): BoundExpression = {
    // Bind the expression being matched against
    val matchedExpr = bind(node.expression, scope)

    matchedExpr match {
      case error: BoundExpression.Error => error
      case _                            =>
        // Bind all the match cases
        bindMatchCases(node.cases.head, node.cases.tail, scope) match {
          case Result.Error(value)        => value
          case Result.Success(boundCases) =>
            // Calculate the result type from all cases
            val headType = binder.getType(boundCases.head.result)
            val resultType = calculateMatchResultType(headType, boundCases.tail)
            val location = AstUtils.locationOfExpression(node)

            BoundExpression.MatchExpression(
              location,
              resultType,
              matchedExpr,
              boundCases
            )
        }
    }
  }

  def bindNewExpression(
      node: Expression.NewExpression,
      scope: Scope
  ): Result[BoundExpression.Error, BoundExpression.NewExpression] = {
    val instantiationType = binder.bindTypeName(node.name, scope)
    instantiationType match {
      case Type.Class(_, ns, name, args, symbol) =>
        findConstructor(symbol) match {
          case Option.None =>
            diagnosticBag.reportSymbolNotFound(
              AstUtils.locationOfName(node.name),
              name
            )
            Result.Error(
              BoundExpression.Error(
                "Cannot find constructor for class: " + name
              )
            )
          case Option.Some(ctor) =>
            val args = bindExpressions(
              fromExpressionList(node.arguments.expressions),
              scope
            )
            val location = AstUtils.locationOfExpression(node)

            val ctorType = binder.tryGetSymbolType(ctor)
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
                Result.Error(
                  BoundExpression.Error(
                    "Constructor symbol does not have a function type: " + name
                  )
                )
            }
        }
      case Type.Error(message) => Result.Error(BoundExpression.Error(message))
      case _ =>
        println(node.closeParen.location.toString())
        panic("expected named type, got " + instantiationType)
    }
  }

  def bindUnaryExpression(
      node: Expression.UnaryExpression,
      scope: Scope
  ): BoundExpression = {
    val op = bindUnaryOperator(node.operator)
    bind(node.expression, scope) match {
      case error: BoundExpression.Error => error
      case operand =>
        binder.getType(operand) match {
          case Type.Error(message) =>
            BoundExpression.Error(message)
          case operandType =>
            operators.checkUnary(operandType, op) match {
              case Type.Error(message) =>
                diagnosticBag.reportNoOperatorForOperand(
                  node.operator.location,
                  node.operator.text,
                  operandType
                )
                BoundExpression.Error(message)
              case resultType =>
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
            binder.setSymbolType(symbol, typ)
            BoundStatement.VariableDeclaration(symbol, false, typ, expr)

          case Option.Some(value) =>
            val annotatedType = binder.bindTypeName(value.typ, scope)
            binder.setSymbolType(symbol, annotatedType)

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

  def bindGlobalStatements(
      statements: List[MemberSyntax.GlobalStatementSyntax],
      scope: Scope
  ): List[BoundStatement] = {
    statements match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        val boundHead = bindStatement(head.statement, scope)
        val boundTail = bindGlobalStatements(tail, scope)
        List.Cons(boundHead, boundTail)
    }
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
