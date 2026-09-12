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

class ExprBinder(
    rootSymbol: Symbol,
    binder: Binder,
    conversionClassifier: ConversionClassifier,
    diagnosticBag: DiagnosticBag
) {

  val operators = new BinaryOperators(binder)
  val typeInference = TypeInference(binder)

  /**   1. Base rules:
    *      - Never is a subtype of all types.
    *      - All types are subtypes of Any.
    *      - A type is a subtype of itself.
    *        2. Function Subtyping:
    *   - Function types are covariant in the return type and contravariant in
    *     the parameter types:
    *     - (A1, A2) => R1 is a subtype of (B1, B2) => R2 if:
    *     - B1 is a subtype of A1 (contravariant in parameters),
    *     - B2 is a subtype of A2 (contravariant in parameters), and
    *     - R1 is a subtype of R2 (covariant in the return type).
    *
    *   3. Array Subtyping:
    *      - ArrayType(T1) is a subtype of ArrayType(T2) if T1 is a subtype of
    *        T2.
    *        4. Option Subtyping:
    *      - OptionType(T1) is a subtype of OptionType(T2) if T1 is a subtype of
    *        T2.
    *        5. Reference Types:
    *      - Reference(S1) is a subtype of Reference(S2) if they refer to the
    *        same symbol (assuming no polymorphism in this example).
    */

  def isErrorType(typ: Type): bool =
    typ match {
      case Type.Error(_) => true
      case _             => false
    }

  def isSubtype(subType: Type, superType: Type): bool = {
    if (isErrorType(subType) || isErrorType(superType)) {
      false
    } else if (
      subType == superType || subType == binder.neverType ||
      superType == binder.anyType
    ) {
      true
    } else {
      isSubtypeStructurally(subType, superType)
    }
  }

  def isSubtypeStructurally(subType: Type, superType: Type): bool = {
    Tuple2(subType, superType) match {
      // Function subtyping
      case Tuple2(
            Type.Function(_, subParams, subReturn),
            Type.Function(_, superParams, superReturn)
          ) =>
        // isSubtypeList is false for lists of different lengths
        val paramsAreSubtypes = Tuple2(subParams, superParams) match {
          case Tuple2(
                List.Cons(subParam, subTail),
                List.Cons(superParam, superTail)
              ) =>
            isSubtype(superParam.typ, subParam.typ) && // Contravariant
            isSubtypeList(subTail, superTail)
          case Tuple2(List.Nil, List.Nil) => true
          case _                          => false
        }
        paramsAreSubtypes && isSubtype(subReturn, superReturn) // Covariant

      // Array subtyping
      case Tuple2(
            Type.Class(_, _, "Array", List.Cons(subElemType, List.Nil), _),
            Type.Class(_, _, "Array", List.Cons(superElemType, List.Nil), _)
          ) =>
        isSubtype(subElemType, superElemType)

      case _ => false
    }
  }

  def isSubtypeList(
      value: List[BoundParameter],
      value1: List[BoundParameter]
  ): bool = {
    Tuple2(value, value1) match {
      case Tuple2(
            List.Cons(head1, tail1),
            List.Cons(head2, tail2)
          ) =>
        isSubtype(head1.typ, head2.typ) && isSubtypeList(tail1, tail2)
      case Tuple2(List.Nil, List.Nil) => true
      case _                          => false
    }
  }

  def subsume(expr: BoundExpression, expectedType: Type): BoundExpression = {
    val exprType = binder.getType(expr)
    if (isSubtype(exprType, expectedType)) {
      expr
    } else {
      bindConversion(expr, expectedType, false)
    }
  }

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

  def check(
      expr: Expression,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    expr match {
      case node: Expression.ArrayCreation =>
        checkArrayCreation(node, expectedType, scope)
      case node: Expression.Assignment =>
        checkAssignment(node, expectedType, scope)
      case node: Expression.Binary => checkBinary(node, expectedType, scope)
      case node: Expression.Block  => checkBlock(node, expectedType, scope)
      case node: Expression.Call   => checkCall(node, expectedType, scope)
      case node: Expression.Cast   => checkCast(node, expectedType, scope)
      case node: Expression.For    => checkFor(node, expectedType, scope)
      case node: Expression.Group  => checkGroup(node, expectedType, scope)
      case node: Expression.IdentifierName =>
        checkIdentifierName(node, expectedType, scope)
      case node: Expression.If      => checkIf(node, expectedType, scope)
      case node: Expression.Is      => checkIs(node, expectedType, scope)
      case node: Expression.Literal => checkLiteral(node, expectedType, scope)
      case node: Expression.MemberAccess =>
        checkMemberAccess(node, expectedType, scope)
      case node: Expression.Match => checkMatch(node, expectedType, scope)
      case node: Expression.New   => checkNew(node, expectedType, scope)
      case node: Expression.Unary => checkUnary(node, expectedType, scope)
      case node: Expression.Unit  => checkUnit(node, expectedType, scope)
      case node: Expression.While => checkWhile(node, expectedType, scope)
    }
  }

  def checkArrayCreation(
      expr: Expression.ArrayCreation,
      expectedType: Type,
      scope: Scope
  ): BoundExpression =
    subsume(inferArrayCreationExpression(expr, scope), expectedType)

  def checkAssignment(
      expr: Expression.Assignment,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    val lhs = inferLHS(expr.left, scope)
    val rhs = infer(expr.right, scope)
    Tuple2(lhs, rhs) match {
      case Tuple2(Result.Error(error), _) =>
        error
      case Tuple2(_, BoundExpression.Error(_)) =>
        rhs
      case Tuple2(Result.Success(lhs), rhs) =>
        if (!isAssignableLHS(lhs)) {
          val location = AstUtils.locationOfExpression(expr.left)
          diagnosticBag.reportExpressionIsNotAssignable(location)
          BoundExpression.Error(
            "Expression is not assignable: " + string(location)
          )
        } else {
          reportIfReadOnly(lhs)
          getLHSType(lhs) match {
            case Type.Error(message) => BoundExpression.Error(message)
            case lhsType =>
              BoundExpression.Assignment(
                AstUtils.locationOfExpression(expr),
                lhs,
                bindConversion(rhs, lhsType, false)
              )
          }
        }

      case _ =>
        val location = AstUtils.locationOfExpression(expr.left)
        diagnosticBag.reportExpressionIsNotAssignable(location)
        BoundExpression.Error(
          "Expression is not assignable: " + string(location)
        )
    }
  }

  def checkBinary(
      expr: Expression.Binary,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    val inferred = inferBinary(expr, scope)
    subsume(inferred, expectedType)
  }

  def checkBlock(
      node: Expression.Block,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    val block = scope.newBlock()
    val statements = bindStatements(node.block.statements, block)
    val expr = node.block.expression match {
      case Option.None =>
        BoundExpression.Unit(TextLocationFactory.empty())
      case Option.Some(value) => check(value, expectedType, block)
    }

    expr match {
      case _: BoundExpression.Error => expr
      case _                        => BoundExpression.Block(statements, expr)
    }
  }

  def checkCall(
      node: Expression.Call,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    // For call expressions, we can use the expected type to improve type inference
    // for generic functions
    inferLHS(node.name, scope) match {
      case Result.Error(error) => error
      case Result.Success(function) =>
        val exprs = fromExpressionList(node.arguments.expressions)
        val args = bindArgumentExpressions(
          function,
          exprs,
          Option.Some(expectedType),
          scope
        )

        // Try to use expected type for better generic inference
        checkCallLHS(
          function,
          args,
          explicitCallTypeArguments(node.name, scope),
          expectedType,
          scope
        ) match {
          case Result.Error(value) => value
          case Result.Success(value) =>
            val inferred = convertLHSToExpression(value)
            subsume(inferred, expectedType)
        }
    }
  }

  def checkCallLHS(
      function: BoundLeftHandSide,
      args: List[BoundExpression],
      writtenTypeArguments: List[Type],
      expectedType: Type,
      scope: Scope
  ): Result[BoundExpression.Error, BoundLeftHandSide] = {
    val functionType = getLHSType(function)
    val argTypes = binder.getTypes(args)

    if (typesWithError(List.Cons(functionType, argTypes))) {
      Result.Error(
        BoundExpression.Error(
          "Cannot bind call expression because of type errors"
        )
      )
    } else if (isTraitMemberAccess(function)) {
      // Same interception as the infer path: a body with a declared return type
      // arrives here instead.
      bindEvidenceCall(function, args, scope)
    } else {
      functionType match {
        case func: Type.Function =>
          // Non-generic function: use normal binding
          bindFunctionCall(function, func, args, scope) match {
            case Result.Error(value) => Result.Error(value)
            case Result.Success(value) =>
              Result.Success(BoundLeftHandSide.Call(value))
          }

        case gf: Type.GenericFunction =>
          // Generic function: use expected type to help infer type arguments!
          checkGenericFunctionCall(
            function,
            gf,
            args,
            writtenTypeArguments,
            expectedType,
            scope
          ) match {
            case Result.Error(value) => Result.Error(value)
            case Result.Success(value) =>
              Result.Success(BoundLeftHandSide.Call(value))
          }

        case Type.GenericClass(_, ns, name, _, symbol) =>
          bindGenericClassCall(
            function,
            functionType,
            ns,
            name,
            symbol,
            args,
            Option.Some(expectedType),
            scope
          )

        case _ =>
          // Fall back to regular inference
          inferCallBound(function, args, writtenTypeArguments, scope)
      }
    }
  }

  def checkCast(
      expr: Expression.Cast,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    val inferred = inferCast(expr, scope)
    subsume(inferred, expectedType)
  }

  def checkFor(
      expr: Expression.For,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    // For expressions always return unit, so just infer and subsume
    val inferred = inferForExpression(expr, scope)
    subsume(inferred, expectedType)
  }

  def checkGroup(
      expr: Expression.Group,
      expectedType: Type,
      scope: Scope
  ): BoundExpression =
    check(expr.expression, expectedType, scope)

  def checkIdentifierName(
      expr: Expression.IdentifierName,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    inferIdentifierName(expr, scope) match {
      case Result.Error(value) => value
      case Result.Success(value) =>
        subsume(value, expectedType)
    }
  }

  def checkIf(
      expr: Expression.If,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    val cond = check(expr.condition, binder.boolType, scope)
    val thenBranch = check(expr.thenExpr, expectedType, scope)
    val elseBranch = expr.elseExpr match {
      case Option.None =>
        // If there's no else branch, we assume it's a unit type
        Option.None
      case Option.Some(elseExpr) =>
        Option.Some(check(elseExpr.expression, expectedType, scope))
    }

    BoundExpression.If(
      AstUtils.locationOfExpression(expr),
      cond,
      thenBranch,
      elseBranch,
      expectedType
    )
  }

  def checkIs(
      expr: Expression.Is,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    val inferred = inferIsExpression(expr, scope)
    subsume(inferred, expectedType)
  }

  def checkLiteral(
      expr: Expression.Literal,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    val inferred = inferLiteral(expr, scope)
    subsume(inferred, expectedType)
  }

  def checkMemberAccess(
      expr: Expression.MemberAccess,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    inferMemberAccess(expr, scope) match {
      case Result.Error(value) => value
      case Result.Success(value) =>
        subsume(value, expectedType)
    }
  }

  def checkMatch(
      expr: Expression.Match,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {

    // Bind the expression being matched against
    val matchedExpr = infer(expr.expression, scope)

    matchedExpr match {
      case error: BoundExpression.Error => error
      case _                            =>
        // Bind all the match cases
        checkMatchCases(
          expr.cases.head,
          expr.cases.tail,
          binder.getType(matchedExpr),
          expectedType,
          scope
        ) match {
          case Result.Error(value) => value
          case Result.Success(boundCases) =>
            val location = AstUtils.locationOfExpression(expr)

            BoundExpression.Match(
              location,
              expectedType,
              matchedExpr,
              boundCases
            )
        }
    }
  }

  def checkNew(
      expr: Expression.New,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    bindNew(expr, Option.Some(expectedType), scope) match {
      case Result.Error(value) => value
      case Result.Success(value) =>
        val inferred = convertLHSToExpression(value)
        subsume(inferred, expectedType)
    }
  }

  def checkUnary(
      expr: Expression.Unary,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    val inferred = inferUnary(expr, scope)
    subsume(inferred, expectedType)
  }

  def checkUnit(
      expr: Expression.Unit,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    val inferred = inferUnit(expr, scope)
    subsume(inferred, expectedType)
  }

  def checkWhile(
      expr: Expression.While,
      expectedType: Type,
      scope: Scope
  ): BoundExpression = {
    val inferred = inferWhileExpression(expr, scope)
    subsume(inferred, expectedType)
  }

  def infer(expr: Expression, scope: Scope): BoundExpression = {
    expr match {
      case node: Expression.ArrayCreation =>
        inferArrayCreationExpression(node, scope)
      case node: Expression.Assignment =>
        inferAssignmentExpression(node, scope)
      case node: Expression.Binary =>
        inferBinary(node, scope)
      case node: Expression.Block => inferBlock(node, scope)
      case node: Expression.Call =>
        inferCallNode(node, scope) match {
          case Result.Error(value)   => value
          case Result.Success(value) => convertLHSToExpression(value)
        }
      case node: Expression.Cast  => inferCast(node, scope)
      case node: Expression.For   => inferForExpression(node, scope)
      case node: Expression.Group => inferGroup(node, scope)
      case node: Expression.IdentifierName =>
        inferIdentifierName(node, scope) match {
          case Result.Error(value)   => value
          case Result.Success(value) => value
        }
      case node: Expression.If => inferIf(node, scope)
      case node: Expression.Is => inferIsExpression(node, scope)
      case node: Expression.Literal =>
        inferLiteral(node, scope)
      case node: Expression.MemberAccess =>
        inferMemberAccess(node, scope) match {
          case Result.Error(value)   => value
          case Result.Success(value) => value
        }
      case node: Expression.Match => inferMatchExpression(node, scope)
      case node: Expression.New =>
        inferNew(node, scope) match {
          case Result.Error(value)   => value
          case Result.Success(value) => convertLHSToExpression(value)
        }
      case node: Expression.Unary => inferUnary(node, scope)
      case node: Expression.Unit  => inferUnit(node, scope)
      case node: Expression.While => inferWhileExpression(node, scope)
    }
  }

  def inferLHS(
      expr: Expression,
      scope: Scope
  ): Result[BoundExpression.Error, BoundLeftHandSide] = {
    expr match {
      case node: Expression.IdentifierName =>
        inferIdentifierName(node, scope) match {
          case Result.Error(value) => Result.Error(value)
          case Result.Success(value) =>
            val location = AstUtils.locationOfExpression(node)
            Result.Success(BoundLeftHandSide.Variable(location, value.symbol))
        }
      case node: Expression.MemberAccess =>
        inferMemberAccess(node, scope) match {
          case Result.Error(value) => Result.Error(value)
          case Result.Success(value) =>
            Result.Success(BoundLeftHandSide.MemberAccess(value))
        }
      case node: Expression.Call =>
        inferCallNode(node, scope) match {
          case Result.Error(value)   => Result.Error(value)
          case Result.Success(value) => Result.Success(value)
        }
      case node: Expression.New =>
        inferNew(node, scope) match {
          case Result.Error(value)   => Result.Error(value)
          case Result.Success(value) => Result.Success(value)
        }
      case _ =>
        panic("bindLHS called with non-LHS expression")
        Result.Error(
          BoundExpression.Error(
            "Expected left-hand side expression"
          )
        )
    }
  }

  def bindConversionExpr(
      expr: Expression,
      toType: Type,
      scope: Scope
  ): BoundExpression = {
    val bound = infer(expr, scope)
    bound match {
      case _: BoundExpression.Error => bound
      case _                        => bindConversion(bound, toType, false)
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
                new BoundExpression.Cast(location, expr, toType)
              case Conversion.Explicit =>
                if (allowExplicit) {
                  val location = AstUtils.locationOfBoundExpression(expr)
                  new BoundExpression.Cast(location, expr, toType)
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

  def inferArrayCreationExpression(
      node: Expression.ArrayCreation,
      scope: Scope
  ): BoundExpression = {
    // Extract the array element type from the name (e.g., Array[int])
    val arrayType = binder.bindTypeName(node.name, scope)

    // Extract the element type from the Array[T] type
    arrayType match {
      case Type.Class(_, _, "Array", List.Cons(elementType, List.Nil), _) =>
        // Bind the array size expression
        val sizeExpr = node.arrayRank match {
          case Option.Some(rankExpr) =>
            bindConversion(infer(rankExpr, scope), binder.intType, false)
          case Option.None =>
            // Default to size 0 if no size provided
            BoundExpression.Int(TextLocationFactory.empty(), 0)
        }

        val location = AstUtils.locationOfExpression(node)

        elementType match {
          case Type.Error(message) => BoundExpression.Error(message)
          case _                   =>
            // Create array type from element type
            val arrayType = Type.Class(
              location,
              List.Nil,
              "Array",
              List.Cons(elementType, List.Nil),
              binder.arraySymbol
            )

            // Return a special ArrayCreation expression that will be handled in lowering
            BoundExpression.ArrayCreation(
              location,
              elementType,
              sizeExpr,
              arrayType
            )
        }
      case _ =>
        panic("Invalid array type in ArrayCreation: " + string(arrayType))
        BoundExpression.Error(
          "Invalid array type in ArrayCreation: " + arrayType.toString()
        )
    }

  }

  /** `val` bindings are immutable. Reports at the assignment rather than the
    * declaration, and binding continues so the right-hand side still gets
    * checked.
    *
    * A field initializer is not a reassignment: fieldsToInitStatements rewrites
    * `val x = e` into an assignment for $runtimeInit, reusing the declaration's
    * own identifier token. That synthesized assignment is the one whose
    * location matches the symbol's, so it is skipped.
    */
  def reportIfReadOnly(lhs: BoundLeftHandSide): unit =
    lhs match {
      case BoundLeftHandSide.Variable(location, symbol) =>
        // Compared on offset and file rather than on TextLocation itself:
        // the self-hosted binder has no == for a class type.
        val atDeclaration =
          location.span.start == symbol.location.span.start &&
            location.fileName == symbol.location.fileName
        if (symbol.isReadOnly && !atDeclaration) {
          diagnosticBag.reportAssignmentToVal(location, symbol.name)
        }
      case BoundLeftHandSide.MemberAccess(expression) =>
        if (expression.member.isReadOnly) {
          diagnosticBag.reportAssignmentToVal(
            expression.location,
            expression.member.name
          )
        }
      case _ => ()
    }

  def inferAssignmentExpression(
      node: Expression.Assignment,
      scope: Scope
  ): BoundExpression = {
    inferLHS(node.left, scope) match {
      case Result.Error(value) => value
      case Result.Success(lhs) =>
        if (!isAssignableLHS(lhs)) {
          val location = AstUtils.locationOfExpression(node.left)
          diagnosticBag.reportExpressionIsNotAssignable(location)
          BoundExpression.Error(
            "Expression is not assignable: " + string(location)
          )
        } else {
          reportIfReadOnly(lhs)
          getLHSType(lhs) match {
            case Type.Error(message) => BoundExpression.Error(message)
            case lhsType =>
              val rhs = check(node.right, lhsType, scope)
              BoundExpression.Assignment(
                AstUtils.locationOfExpression(node),
                lhs,
                rhs
              )
          }
        }
    }
  }

  def inferBinary(
      node: Expression.Binary,
      scope: Scope
  ): BoundExpression = {
    val left = infer(node.left, scope)
    val right = infer(node.right, scope)

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
            operators.checkBinary(leftType, rightType, op) match {
              case Type.Error(_) =>
                // The builtin table has no row, so evidence is the fallback:
                // `a == b` is valid exactly when `Eq[typeof(a)]` is available
                // ([ADR 0004](../../../docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)).
                //
                // Evidence is tried before ADR 0003's reference-identity rule,
                // not after. A type that declares how it compares must compare
                // that way; identity is what is left when nothing says
                // otherwise.
                bindOperatorThroughEvidence(
                  node,
                  left,
                  right,
                  leftType,
                  scope
                ) match {
                  case Option.Some(bound) => bound
                  case Option.None =>
                    if (operators.referenceEquality(leftType, rightType, op)) {
                      BoundExpression.Binary(
                        node.operator.location,
                        left,
                        op,
                        right,
                        binder.boolType
                      )
                    } else {
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
                    }
                }
              case resultType =>
                BoundExpression.Binary(
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

  /** `a == b` resolved through the trait that claims `==`.
    *
    * Two ways the evidence can be reached, and they are not interchangeable:
    *
    *   - inside `def same[T: Eq](a: T, b: T)` the operands have a type
    *     parameter's type, and the evidence is a symbol the enclosing
    *     declaration holds — a parameter or a field — so the call has to go
    *     through the record, which is what `EvidenceCall` emits;
    *   - on a ground type the given is known here, and its members are static,
    *     so an ordinary call to the member is what it compiles to.
    *
    * Returns `None` when nothing proves the goal, leaving the caller to report
    * the operator as missing — which is the better message, because from the
    * source's point of view what is missing is the operator, not the evidence.
    */
  def bindOperatorThroughEvidence(
      node: Expression.Binary,
      left: BoundExpression,
      right: BoundExpression,
      leftType: Type,
      scope: Scope
  ): Option[BoundExpression] = {
    val name = node.operator.text
    val location = node.operator.location

    binder.findEvidenceMember(leftType, name, scope.current) match {
      case Option.Some(KeyValue(evidence, member)) =>
        bindOperatorCall(
          location,
          member,
          binder.getSymbolType(member),
          left,
          right,
          scope,
          Option.Some(evidence),
          Option.None
        )
      case Option.None =>
        findOperatorGivenMember(
          binder.evidenceTypes(leftType),
          node.operator.kind,
          name,
          location
        ) match {
          case Option.None => Option.None
          case Option.Some(found) =>
            bindOperatorCall(
              location,
              found.member,
              binder.instantiateGivenMember(found),
              left,
              right,
              scope,
              Option.None,
              Option.Some(found.record)
            )
        }
    }
  }

  /** The given's member for the first of `types` a trait claiming this token
    * has evidence for. `types` widens a case type to its enum, so
    * `Shape.Circle(1) < Shape.Rect(1, 1)` finds the enum's `Ord`.
    */
  def findOperatorGivenMember(
      types: List[Type],
      tokenKind: int,
      name: string,
      location: TextLocation
  ): Option[GivenMember] = {
    types match {
      case List.Nil => Option.None
      case List.Cons(head, tail) =>
        val found: Option[GivenMember] =
          binder.operatorGoal(tokenKind, head) match {
            case Option.None => Option.None
            case Option.Some(goal) =>
              if (!binder.isGroundType(goal)) Option.None
              else binder.findGivenMember(goal, name, location)
          }

        found match {
          case Option.Some(_) => found
          case Option.None =>
            findOperatorGivenMember(tail, tokenKind, name, location)
        }
    }
  }

  /** Whether the right operand widens to the operator member's second
    * parameter.
    */
  def operandFits(
      right: BoundExpression,
      parameters: List[BoundParameter]
  ): bool = {
    parameters match {
      case List.Cons(_, List.Cons(second, _)) =>
        conversionClassifier.widensTo(binder.getType(right), second.typ)
      case _ => false
    }
  }

  /** Binds the two operands against the operator member's declared parameters.
    *
    * `evidence` decides the node: `Some` for a record the caller has to load,
    * `None` for a static call on the given itself. `record` is the static field
    * that call has to pass on as the member's trailing argument ([ADR
    * 0006](../../../docs/architecture/adr/0006-conditional-givens.md), decision
    * C); the `EvidenceCall` shape loads its own, so the two are never both set.
    */
  def bindOperatorCall(
      location: TextLocation,
      member: Symbol,
      memberType: Type,
      left: BoundExpression,
      right: BoundExpression,
      scope: Scope,
      evidence: Option[BoundEvidence],
      record: Option[Symbol]
  ): Option[BoundExpression] = {
    // `memberType` rather than the member's declared type: a conditional
    // given's member is stated in that given's type variables, so the operands
    // have to be checked against the instantiated signature (ADR 0006).
    memberType match {
      case Type.Function(_, parameters, returnType) =>
        // The goal was built from the left operand alone, so the right one is
        // still unchecked. Rejecting here rather than binding the arguments
        // and letting the conversion fail keeps `"a" == 'c'` reported as a
        // missing operator, which is what the source is actually missing.
        if (parameters.length != 2 || !operandFits(right, parameters))
          Option.None
        else {
          val arguments = bindArgumentsToTypes(
            getParameterTypes(parameters),
            List.Cons(left, ListModule.one(right)),
            scope
          )

          evidence match {
            case Option.Some(symbol) =>
              Option.Some(
                BoundExpression.EvidenceCall(
                  location,
                  symbol,
                  member,
                  arguments,
                  returnType
                )
              )
            case Option.None =>
              Option.Some(
                BoundExpression.Call(
                  location,
                  Option.None,
                  member,
                  List.Nil,
                  binder.withRecordArgument(arguments, record, location),
                  returnType
                )
              )
          }
        }
      case _ => Option.None
    }
  }

  def inferBlock(
      node: Expression.Block,
      scope: Scope
  ): BoundExpression = {
    val block = scope.newBlock()
    val statements = bindStatements(node.block.statements, block)
    val expr = node.block.expression match {
      case Option.None =>
        BoundExpression.Unit(TextLocationFactory.empty())
      case Option.Some(value) => infer(value, block)
    }

    expr match {
      case _: BoundExpression.Error => expr
      case _                        => BoundExpression.Block(statements, expr)
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
        if (isErrorType(head)) true
        else typesWithError(tail)
    }
  }

  def inferCallNode(
      node: Expression.Call,
      scope: Scope
  ): Result[
    BoundExpression.Error,
    BoundLeftHandSide
  ] = {
    inferLHS(node.name, scope) match {
      case Result.Error(error) => Result.Error(error)

      case Result.Success(function) =>
        val exprs = fromExpressionList(node.arguments.expressions)
        val args =
          bindArgumentExpressions(function, exprs, Option.None, scope)
        inferCallBound(
          function,
          args,
          explicitCallTypeArguments(node.name, scope),
          scope
        )
    }
  }

  /** Bind a call's arguments, checking each against its parameter's type where
    * the callee fixes one.
    *
    * This is how an expected type reaches a nested call.
    * `LoweredBlock(Chain.Empty())` only knows to instantiate the chain because
    * the parameter says `Chain[LoweredStatement]`; inferred on its own the
    * chain's parameter has nothing to solve it and defaults to `any`.
    *
    * `argumentExpectedTypes` answers with either every parameter type or none,
    * so a short list here means the callee could not say and everything falls
    * back to inference.
    */
  def bindArgumentExpressions(
      function: BoundLeftHandSide,
      exprs: List[Expression],
      expectedType: Option[Type],
      scope: Scope
  ): List[BoundExpression] =
    bindExpressionsToTypes(
      exprs,
      argumentExpectedTypes(function, exprs.length, expectedType),
      scope
    )

  def bindExpressionsToTypes(
      exprs: List[Expression],
      expectedTypes: List[Type],
      scope: Scope
  ): List[BoundExpression] =
    Tuple2(exprs, expectedTypes) match {
      case Tuple2(List.Cons(expr, exprTail), List.Cons(typ, typeTail)) =>
        List.Cons(
          check(expr, typ, scope),
          bindExpressionsToTypes(exprTail, typeTail, scope)
        )
      case _ => bindExpressions(exprs, scope)
    }

  /** The types a call's arguments are checked against, or `List.Nil` when the
    * callee cannot fix them.
    *
    * All or nothing on purpose. A generic callee's parameter types mention its
    * own type variables until they are solved, so handing a half-instantiated
    * parameter type to an argument would check it against a variable. Either
    * the callee is monomorphic, or the expected type solves every one of its
    * type parameters, or the arguments are inferred as they were before.
    */
  def argumentExpectedTypes(
      function: BoundLeftHandSide,
      argCount: int,
      expectedType: Option[Type]
  ): List[Type] = {
    if (isTraitMemberAccess(function)) {
      // An evidence call passes the receiver as its first argument, so the
      // parameters do not line up with the list written at the call site.
      List.Nil
    } else {
      getLHSType(function) match {
        case Type.Function(_, params, _) =>
          ofMatchingArity(getParameterTypes(params), argCount)

        case gf: Type.GenericFunction =>
          solvedParameterTypes(
            gf.generics,
            getParameterTypes(gf.parameters),
            gf.returnType,
            expectedType,
            argCount
          )

        case Type.Class(_, _, name, _, symbol) =>
          // Array and string indexing reach the call path too, and their
          // callee is not a constructor; the index infers as an int on its
          // own.
          if (name == "Array") List.Nil
          else if (name == "string" && !namesItsOwnType(function, symbol))
            List.Nil
          else
            constructorType(symbol) match {
              case Option.Some(Type.Function(_, params, _)) =>
                ofMatchingArity(getParameterTypes(params), argCount)
              case _ => List.Nil
            }

        case Type.GenericClass(loc, ns, name, _, symbol) =>
          constructorType(symbol) match {
            case Option.Some(
                  Type.GenericFunction(_, generics, _, params, _)
                ) =>
              solvedParameterTypes(
                generics,
                getParameterTypes(params),
                // What the constructor produces with its own parameters left
                // as variables, which is what the expected type is matched
                // against.
                Type.Class(
                  loc,
                  ns,
                  name,
                  genericsAsVariables(generics, 0),
                  symbol
                ),
                expectedType,
                argCount
              )
            case _ => List.Nil
          }

        case _ => List.Nil
      }
    }
  }

  def constructorType(symbol: Symbol): Option[Type] =
    findConstructor(symbol) match {
      case Option.None       => Option.None
      case Option.Some(ctor) => binder.tryGetSymbolType(ctor)
    }

  def solvedParameterTypes(
      generics: List[GenericTypeParameter],
      parameterTypes: List[Type],
      resultType: Type,
      expectedType: Option[Type],
      argCount: int
  ): List[Type] =
    expectedType match {
      case Option.None => List.Nil
      case Option.Some(expected) =>
        typeInference.solveTypeArgumentsFromExpected(
          generics,
          resultType,
          expected
        ) match {
          case Option.None => List.Nil
          case Option.Some(typeArgs) =>
            ofMatchingArity(
              Types.substituteList(parameterTypes, typeArgs),
              argCount
            )
        }
    }

  /** Parameter types are only usable as expected types when there are as many
    * of them as there are arguments; otherwise the call is a count mismatch,
    * which the callee reports on its own.
    */
  def ofMatchingArity(types: List[Type], argCount: int): List[Type] =
    if (types.length == argCount) types else List.Nil

  def inferCallBound(
      function: BoundLeftHandSide,
      args: List[BoundExpression],
      writtenTypeArguments: List[Type],
      scope: Scope
  ): Result[
    BoundExpression.Error,
    BoundLeftHandSide
  ] = {
    // cases:
    // 1. functions
    // 2. generic functions
    // 3. array indexing - array(0)
    // 3. class instantiation - Enum.Case(0)
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
    } else if (isTraitMemberAccess(function)) {
      bindEvidenceCall(function, args, scope)
    } else
      functionType match {
        case func: Type.Function =>
          bindFunctionCall(function, func, args, scope) match {
            case Result.Error(value) => Result.Error(value)
            case Result.Success(value) =>
              Result.Success(BoundLeftHandSide.Call(value))
          }

        case gf: Type.GenericFunction =>
          bindGenericFunctionCall(
            function,
            gf,
            args,
            writtenTypeArguments,
            scope
          ) match {
            case Result.Error(value) => Result.Error(value)
            case Result.Success(value) =>
              Result.Success(BoundLeftHandSide.Call(value))
          }

        case Type.Class(_, ns, name, typeArgs, symbol) =>
          val location = AstUtils.locationOfBoundLeftHandSide(function)
          // Handle array indexing
          if (name == "Array") {
            val elementType = typeArgs match {
              case List.Cons(elemType, List.Nil) => elemType
              case _                             => binder.anyType
            }
            bindIndexAccess(function, args, elementType, location)
          } else if (name == "string" && !namesItsOwnType(function, symbol)) {
            // `str(0)` reads a character out of a string. It reaches here the
            // same way `string(x)` does — both are a call whose callee types
            // as `string` — and the two are told apart by whether the callee
            // is the type itself or a value of it. Without this the index
            // falls through to `apply`, which is the conversion, so `str(0)`
            // types as `string` and `str(0) == '-'` compares a string to a
            // char.
            bindIndexAccess(function, args, binder.charType, location)
          } else {
            // Handle regular class constructors
            findConstructor(symbol) match {
              case Option.None =>
                bindApply(args, scope, functionType, symbol, location)
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
                      case Result.Error(value)   => Result.Error(value)
                      case Result.Success(value) => Result.Success(value)
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
          }

        case Type.GenericClass(_, ns, name, _, symbol) =>
          bindGenericClassCall(
            function,
            functionType,
            ns,
            name,
            symbol,
            args,
            Option.None,
            scope
          )

        case _ =>
          val location = AstUtils.locationOfBoundLeftHandSide(function)
          diagnosticBag.reportInternalError(location, "bindCallExpression")

          Result.Error(
            BoundExpression.Error(
              "binding error: bindCallExpression"
            )
          )
      }
  }

  /** True when the callee names the class itself rather than a value of it.
    * `string(x)` is the conversion; `str(x)` is an index into `str`.
    */
  def namesItsOwnType(function: BoundLeftHandSide, symbol: Symbol): bool =
    function match {
      case BoundLeftHandSide.Variable(_, callee) => callee == symbol
      case _                                     => false
    }

  /** `receiver(index)`, for the two receivers that have one: an array yields
    * its element type, a string yields `char`.
    */
  def bindIndexAccess(
      function: BoundLeftHandSide,
      args: List[BoundExpression],
      elementType: Type,
      location: TextLocation
  ): Result[BoundExpression.Error, BoundLeftHandSide] = {
    args match {
      case List.Cons(indexArg, List.Nil) =>
        val boundIndex = bindConversion(indexArg, binder.intType, false)
        val receiver = convertLHSToExpression(function)
        val indexExpr = new BoundExpression.Index(
          location,
          receiver,
          boundIndex,
          elementType
        )
        Result.Success(BoundLeftHandSide.Index(indexExpr))
      case _ =>
        diagnosticBag.reportArgumentCountMismatch(
          location,
          1,
          args.length
        )
        Result.Error(
          BoundExpression.Error(
            "Indexing requires exactly one argument but got " + string(
              args.length
            )
          )
        )
    }
  }

  def bindApply(
      args: List[BoundExpression],
      scope: Scope,
      functionType: Type,
      symbol: Symbol,
      location: TextLocation
  ): Result[BoundExpression.Error, BoundLeftHandSide] = {
    findApply(symbol) match {
      case Option.None =>
        diagnosticBag.reportNotCallable(location)
        Result.Error(
          BoundExpression.Error(
            "Cannot find constructor for class: " + symbol.name
          )
        )
      case Option.Some(applySymbol) =>
        binder.tryGetSymbolType(applySymbol) match {
          case Option.Some(Type.Function(loc, params, _)) =>
            bindFunctionCall(
              BoundLeftHandSide.Variable(
                location,
                applySymbol
              ),
              Type.Function(loc, params, functionType),
              args,
              scope
            ) match {
              case Result.Error(value) => Result.Error(value)
              case Result.Success(value) =>
                Result.Success(BoundLeftHandSide.Call(value))
            }
          case _ =>
            diagnosticBag.reportNotCallable(location)
            Result.Error(
              BoundExpression.Error(
                "Apply symbol does not have a function type: " + symbol.name
              )
            )
        }
    }
  }

  def findApply(symbol: Symbol): Option[Symbol] =
    symbol.lookupMember("apply")

  def bindFunctionCall(
      function: BoundLeftHandSide,
      functionType: Type.Function,
      args: List[BoundExpression],
      scope: Scope
  ): Result[BoundExpression.Error, BoundExpression.Call] = {
    val location = AstUtils.locationOfBoundLeftHandSide(function)
    if (functionType.parameters.length != args.length) {
      diagnosticBag.reportArgumentCountMismatch(
        location,
        functionType.parameters.length,
        args.length
      )
      Result.Error(
        BoundExpression.Error(
          "Argument count mismatch: expected " + string(
            functionType.parameters.length
          ) +
            " but got " + string(args.length)
        )
      )
    } else {
      val boundArgs = bindArguments(functionType.parameters, args, scope)

      function match {
        case BoundLeftHandSide.MemberAccess(access) =>
          val receiver = if (access.member.isStatic()) {
            Option.None
          } else {
            Option.Some(access.receiver)
          }
          Result.Success(
            BoundExpression.Call(
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
            BoundExpression.Call(
              location,
              receiverForUnqualifiedCall(location, symbol, scope),
              symbol,
              List.Nil,
              boundArgs,
              functionType.returnType
            )
          )
        case _ => invalidFunctionTarget(location)
      }
    }
  }

  /** Type arguments written at the call site, as in
    * `DictionaryModule.empty[Symbol, int]()`. Only a member access carries
    * them: `inferMemberAccess` is the one place a `GenericNameSyntax` is bound
    * on the way to a call, and it stores the result on the node.
    */
  def explicitTypeArguments(function: BoundLeftHandSide): List[Type] =
    function match {
      case BoundLeftHandSide.MemberAccess(access) => access.genericArguments
      case _                                      => List.Nil
    }

  /** Type arguments on an unqualified callee live on its syntax node. Member
    * calls retain theirs on `BoundExpression.MemberAccess` instead.
    */
  def explicitCallTypeArguments(
      function: Expression,
      scope: Scope
  ): List[Type] =
    function match {
      case Expression.IdentifierName(
            SimpleNameSyntax.GenericNameSyntax(_, arguments)
          ) =>
        binder.bindTypeArgumentList(arguments.arguments, scope)
      case _ => List.Nil
    }

  /** The type arguments a generic call is instantiated with. Ones written at
    * the call site win, as they do for a generic constructor; a list of the
    * wrong length is reported and then ignored, because substituting it would
    * put an argument in the wrong slot or leave a slot unfilled.
    */
  def callTypeArguments(
      function: BoundLeftHandSide,
      generics: List[GenericTypeParameter],
      location: TextLocation,
      written: List[Type],
      inferred: List[Type]
  ): List[Type] = {
    val explicit =
      if (written.isEmpty) explicitTypeArguments(function) else written
    if (explicit.isEmpty) inferred
    else if (explicit.length == generics.length) explicit
    else {
      diagnosticBag.reportTypeArgumentCountMismatch(
        location,
        generics.length,
        explicit.length
      )
      inferred
    }
  }

  def bindGenericFunctionCall(
      function: BoundLeftHandSide,
      genericFunctionType: Type.GenericFunction,
      args: List[BoundExpression],
      writtenTypeArguments: List[Type],
      scope: Scope
  ): Result[BoundExpression.Error, BoundExpression.Call] = {
    val location = AstUtils.locationOfBoundLeftHandSide(function)

    // First, infer type arguments from the call arguments
    val argTypes = binder.getTypes(args)
    val parameterTypes = getParameterTypes(genericFunctionType.parameters)
    val inferredTypeArgs = callTypeArguments(
      function,
      genericFunctionType.generics,
      location,
      writtenTypeArguments,
      typeInference.inferTypeArgumentsFromCall(
        genericFunctionType.generics,
        parameterTypes,
        argTypes
      )
    )

    // Discharge the callee's context bounds now that the type arguments are
    // known. Nothing is stored on the call: `BoundExpression.Call` already
    // carries the type arguments, so lowering can re-derive the goals from the
    // callee's declared constraints.
    binder.requireEvidence(
      genericFunctionType.traits,
      inferredTypeArgs,
      location,
      scope
    )

    // Instantiate the generic function with inferred type arguments
    val instantiatedParameterTypes =
      Types.substituteList(parameterTypes, inferredTypeArgs)

    val instantiatedReturnType =
      Types.substitute(genericFunctionType.returnType, inferredTypeArgs)

    // Check argument count matches
    if (instantiatedParameterTypes.length != args.length) {
      diagnosticBag.reportArgumentCountMismatch(
        location,
        instantiatedParameterTypes.length,
        args.length
      )
      Result.Error(
        BoundExpression.Error(
          "Argument count mismatch: expected " +
            string(instantiatedParameterTypes.length) +
            " but got " + string(args.length)
        )
      )
    } else {
      // Bind arguments with instantiated parameter types
      val boundArgs =
        bindArgumentsToTypes(instantiatedParameterTypes, args, scope)

      function match {
        case BoundLeftHandSide.MemberAccess(access) =>
          val receiver = if (access.member.isStatic()) {
            Option.None
          } else {
            Option.Some(access.receiver)
          }
          Result.Success(
            BoundExpression.Call(
              location,
              receiver,
              access.member,
              inferredTypeArgs, // Pass the inferred type arguments
              boundArgs,
              instantiatedReturnType
            )
          )
        case BoundLeftHandSide.Variable(location, symbol) =>
          Result.Success(
            BoundExpression.Call(
              location,
              receiverForUnqualifiedCall(location, symbol, scope),
              symbol,
              inferredTypeArgs, // Pass the inferred type arguments
              boundArgs,
              instantiatedReturnType
            )
          )
        case _ => invalidFunctionTarget(location)
      }
    }
  }

  /** Bind a generic function call using BOTH argument types AND expected return
    * type for improved type inference (bidirectional typing)
    */
  def checkGenericFunctionCall(
      function: BoundLeftHandSide,
      genericFunctionType: Type.GenericFunction,
      args: List[BoundExpression],
      writtenTypeArguments: List[Type],
      expectedType: Type,
      scope: Scope
  ): Result[BoundExpression.Error, BoundExpression.Call] = {
    val location = AstUtils.locationOfBoundLeftHandSide(function)

    // Infer type arguments using BOTH arguments AND expected return type!
    val argTypes = binder.getTypes(args)
    val parameterTypes = getParameterTypes(genericFunctionType.parameters)

    val inferredTypeArgs = callTypeArguments(
      function,
      genericFunctionType.generics,
      location,
      writtenTypeArguments,
      typeInference.checkTypeArgumentsFromCall(
        genericFunctionType.generics,
        parameterTypes,
        argTypes,
        genericFunctionType.returnType, // The declared return type (may contain type vars)
        expectedType // The expected type from context
      )
    )

    // Discharge the callee's context bounds now that the type arguments are
    // known. Nothing is stored on the call: `BoundExpression.Call` already
    // carries the type arguments, so lowering can re-derive the goals from the
    // callee's declared constraints.
    binder.requireEvidence(
      genericFunctionType.traits,
      inferredTypeArgs,
      location,
      scope
    )

    // Instantiate the generic function with inferred type arguments
    val instantiatedParameterTypes =
      Types.substituteList(parameterTypes, inferredTypeArgs)

    val instantiatedReturnType =
      Types.substitute(genericFunctionType.returnType, inferredTypeArgs)

    // Check argument count matches
    if (instantiatedParameterTypes.length != args.length) {
      diagnosticBag.reportArgumentCountMismatch(
        location,
        instantiatedParameterTypes.length,
        args.length
      )
      Result.Error(
        BoundExpression.Error(
          "Argument count mismatch: expected " +
            string(instantiatedParameterTypes.length) +
            " but got " + string(args.length)
        )
      )
    } else {
      // Bind arguments with instantiated parameter types
      val boundArgs =
        bindArgumentsToTypes(instantiatedParameterTypes, args, scope)

      function match {
        case BoundLeftHandSide.MemberAccess(access) =>
          val receiver = if (access.member.isStatic()) {
            Option.None
          } else {
            Option.Some(access.receiver)
          }
          Result.Success(
            BoundExpression.Call(
              location,
              receiver,
              access.member,
              inferredTypeArgs,
              boundArgs,
              instantiatedReturnType
            )
          )
        case BoundLeftHandSide.Variable(location, symbol) =>
          Result.Success(
            BoundExpression.Call(
              location,
              receiverForUnqualifiedCall(location, symbol, scope),
              symbol,
              inferredTypeArgs,
              boundArgs,
              instantiatedReturnType
            )
          )
        case _ => invalidFunctionTarget(location)
      }
    }
  }

  /** A bare call to an instance method, such as `grow(1)` inside another
    * method, is `this.grow(1)`. The method symbol alone used to produce a call
    * with no receiver, leaving the VM to read an unrelated stack slot as
    * argument zero.
    */
  def receiverForUnqualifiedCall(
      location: TextLocation,
      method: Symbol,
      scope: Scope
  ): Option[BoundLeftHandSide] = {
    if (method.isStatic()) {
      Option.None
    } else {
      scope.lookup("this") match {
        case Option.None => Option.None
        case Option.Some(receiver) =>
          val receiverOwnsMethod = binder.tryGetSymbolType(receiver) match {
            case Option.None => false
            case Option.Some(receiverType) =>
              binder.getTypeSymbol(receiverType) match {
                case Option.None => false
                case Option.Some(receiverTypeSymbol) =>
                  method.parent match {
                    case Option.None => false
                    case Option.Some(methodOwner) =>
                      receiverTypeSymbol == methodOwner ||
                      receiverTypeSymbol.parent == Option.Some(methodOwner)
                  }
              }
          }
          if (receiverOwnsMethod) {
            Option.Some(BoundLeftHandSide.Variable(location, receiver))
          } else {
            Option.None
          }
      }
    }
  }

  /** Direct calls currently have bytecode forms only for a named function or
    * method. Other left-hand-side shapes can acquire a function type through
    * error recovery (and, eventually, first-class functions); reject those at
    * the language boundary instead of letting binding crash.
    */
  def invalidFunctionTarget(
      location: TextLocation
  ): Result[BoundExpression.Error, BoundExpression.Call] = {
    diagnosticBag.reportNotCallable(location)
    Result.Error(BoundExpression.Error("Expression is not callable"))
  }

  /** A call to a generic class's constructor, with or without `new`. Type
    * arguments written at the call site win. Otherwise they are inferred from
    * the arguments and, when there is an expected type, from that too, so
    * `val r: Result[E, B] = Result.Error(e)` solves B from the left-hand side.
    */
  def bindGenericConstructor(
      location: TextLocation,
      ctor: Symbol,
      classLocation: TextLocation,
      ns: List[string],
      name: string,
      symbol: Symbol,
      explicitTypeArgs: List[Type],
      args: List[BoundExpression],
      expectedType: Option[Type],
      scope: Scope
  ): Result[BoundExpression.Error, BoundLeftHandSide] = {
    binder.tryGetSymbolType(ctor) match {
      case Option.Some(Type.Function(loc, params, _)) =>
        bindNewExpressionForSymbol(
          location,
          ctor,
          Type.Function(
            loc,
            params,
            Type.Class(classLocation, ns, name, explicitTypeArgs, symbol)
          ),
          args,
          scope
        )
      case Option.Some(
            Type.GenericFunction(loc, generics, traits, params, _)
          ) =>
        val typeArgs =
          if (!explicitTypeArgs.isEmpty) explicitTypeArgs
          else {
            val parameterTypes = getParameterTypes(params)
            val argumentTypes = getArgumentTypes(args)
            expectedType match {
              case Option.Some(expected) =>
                typeInference.checkTypeArgumentsFromCall(
                  generics,
                  parameterTypes,
                  argumentTypes,
                  Type.Class(
                    classLocation,
                    ns,
                    name,
                    genericsAsVariables(generics, 0),
                    symbol
                  ),
                  expected
                )
              case Option.None =>
                typeInference.inferTypeArgumentsFromCall(
                  generics,
                  parameterTypes,
                  argumentTypes
                )
            }
          }
        // A constrained class resolves its evidence at the `new` site, which is
        // where its type arguments are concrete (ADR 0005, decision B).
        binder.requireEvidence(traits, typeArgs, location, scope)

        val instantiatedType =
          Type.Class(classLocation, ns, name, typeArgs, symbol)
        bindNewExpressionForSymbol(
          location,
          ctor,
          Type.Function(
            loc,
            Types.substituteParameters(params, typeArgs),
            instantiatedType
          ),
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

  /** The class's own parameters as type variables, e.g. Result<$0, $1>, so an
    * expected type can be matched against the constructor's result.
    */
  def genericsAsVariables(
      generics: List[GenericTypeParameter],
      index: int
  ): List[Type] = {
    generics match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        List.Cons(
          Type.Variable(head.location, index),
          genericsAsVariables(tail, index + 1)
        )
    }
  }

  /** `List.Cons(x, xs)` - a generic class called like a function. */
  def bindGenericClassCall(
      function: BoundLeftHandSide,
      functionType: Type,
      ns: List[string],
      name: string,
      symbol: Symbol,
      args: List[BoundExpression],
      expectedType: Option[Type],
      scope: Scope
  ): Result[BoundExpression.Error, BoundLeftHandSide] = {
    val location = AstUtils.locationOfBoundLeftHandSide(function)
    findConstructor(symbol) match {
      case Option.None =>
        bindApply(args, scope, functionType, symbol, location)
      case Option.Some(ctor) =>
        bindGenericConstructor(
          location,
          ctor,
          location,
          ns,
          name,
          symbol,
          // `Chain.Empty[LoweredStatement]()` says which chain it is; without
          // this the case is only ever instantiated from an expected type.
          explicitTypeArguments(function),
          args,
          expectedType,
          scope
        )
    }
  }

  def bindArgumentsToTypes(
      parameterTypes: List[Type],
      args: List[BoundExpression],
      scope: Scope
  ): List[BoundExpression] = {
    Tuple2(parameterTypes, args) match {
      case Tuple2(List.Cons(paramType, paramTail), List.Cons(arg, argTail)) =>
        val boundArg = bindConversion(arg, paramType, false)
        List.Cons(boundArg, bindArgumentsToTypes(paramTail, argTail, scope))
      case _ => List.Nil
    }
  }

  def bindNewExpressionForSymbol(
      location: TextLocation,
      ctor: Symbol,
      ctorType: Type.Function,
      args: List[BoundExpression],
      scope: Scope
  ): Result[BoundExpression.Error, BoundLeftHandSide] = {
    if (ctorType.parameters.length != args.length) {
      diagnosticBag.reportArgumentCountMismatch(
        location,
        ctorType.parameters.length,
        args.length
      )
      Result.Error(
        BoundExpression.Error(
          "Argument count mismatch: expected " + string(
            ctorType.parameters.length
          ) +
            " but got " + string(args.length)
        )
      )
    } else {
      val boundArgs = bindArguments(ctorType.parameters, args, scope)
      // Extract generic arguments from the return type (instantiation type)
      val genericArguments = ctorType.returnType match {
        case Type.Class(_, _, _, args, _) => args
        case _                            => List.Nil
      }
      Result.Success(
        BoundLeftHandSide.New(
          BoundExpression.New(
            location,
            ctor,
            genericArguments,
            boundArgs,
            ctorType.returnType
          )
        )
      )
    }
  }

  def findConstructor(symbol: Symbol): Option[Symbol] =
    symbol.lookupMember(".ctor")

  def inferCast(
      cast: Expression.Cast,
      scope: Scope
  ): BoundExpression = {
    val expr = infer(cast.expression, scope)
    val typ = binder.bindTypeName(cast.typ, scope)

    expr match {
      case _: BoundExpression.Error => expr
      case _ =>
        typ match {
          case Type.Error(message) => BoundExpression.Error(message)
          case _ =>
            val location = AstUtils.locationOfExpression(cast)
            BoundExpression.Cast(location, expr, typ)
        }
    }
  }

  def inferIsExpression(
      isExpr: Expression.Is,
      scope: Scope
  ): BoundExpression = {
    val expr = infer(isExpr.expression, scope)
    val typ = binder.bindTypeName(isExpr.typ, scope)
    val location = AstUtils.locationOfExpression(isExpr)

    new BoundExpression.Is(location, expr, typ)
  }

  def getParameterTypes(parameters: List[BoundParameter]): List[Type] = {
    parameters match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        List.Cons(head.typ, getParameterTypes(tail))
    }
  }

  def getArgumentTypes(arguments: List[BoundExpression]): List[Type] = {
    arguments match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        List.Cons(binder.getType(head), getArgumentTypes(tail))
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
        val expr = infer(head, scope)
        List.Cons(expr, bindExpressions(tail, scope))
    }

  def inferForExpression(
      node: Expression.For,
      scope: Scope
  ): BoundExpression = {

    val blockScope = scope.newBlock()
    val lowerBound = check(node.fromExpr, binder.intType, scope)
    val upperBound = check(node.toExpr, binder.intType, scope)
    val identifier = node.identifier

    blockScope.defineLocal(identifier.text, identifier.location, true) match {
      case Either.Left(value) =>
        panic("symbol already defined even though that should not be possible")
      case Either.Right(variable) =>
        binder.setSymbolType(variable, binder.intType)

        val body = check(node.body, binder.unitType, blockScope)

        BoundExpression.For(
          node.forKeyword.location,
          variable,
          lowerBound,
          upperBound,
          body
        )
    }
  }

  def inferGroup(
      node: Expression.Group,
      scope: Scope
  ): BoundExpression =
    infer(node.expression, scope)

  def inferIdentifierName(
      node: Expression.IdentifierName,
      scope: Scope
  ): Result[BoundExpression.Error, BoundExpression.Variable] = {
    node.value match {
      case SimpleNameSyntax.IdentifierNameSyntax(identifier) =>
        bindIdentifier(identifier, scope)
      case SimpleNameSyntax.GenericNameSyntax(identifier, _) =>
        // The identifier still names the same symbol. Explicit type arguments
        // are consumed by member calls today; retaining them on a free
        // function requires first-class generic function values.
        bindIdentifier(identifier, scope)
      case SimpleNameSyntax.ScalaAliasSyntax(open, name, arrow, alias, close) =>
        diagnosticBag.reportInvalidNamespace(
          open.location.merge(close.location)
        )
        Result.Error(BoundExpression.Error("Invalid alias in expression"))
      case SimpleNameSyntax.AliasSyntax(name, asKeyword, alias) =>
        diagnosticBag.reportInvalidNamespace(
          name.location.merge(alias.location)
        )
        Result.Error(BoundExpression.Error("Invalid alias in expression"))
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
            "Symbol not found: " + identifier.text + " at " + string(
              identifier.location
            )
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

  def inferIf(node: Expression.If, scope: Scope): BoundExpression = {
    val cond = bindConversionExpr(node.condition, binder.boolType, scope)
    val thenExpr = infer(node.thenExpr, scope)
    Tuple2(cond, thenExpr) match {
      case Tuple2(BoundExpression.Error(_), _) =>
        cond
      case Tuple2(_, BoundExpression.Error(_)) =>
        thenExpr
      case _ =>
        node.elseExpr match {
          case Option.None =>
            val boundThen = bindConversion(thenExpr, binder.unitType, false)
            BoundExpression.If(
              node.ifKeyword.location,
              cond,
              boundThen,
              Option.None,
              binder.unitType
            )
          case Option.Some(value) =>
            // With no expected type there is nothing to check either branch
            // against, so infer both and union them, exactly as a match unions
            // its cases. Checking the else branch against the then branch's
            // type instead — which is what this used to do — makes the two
            // branches asymmetric and rejects every if whose branches are
            // different cases of one enum, `Option.Some(x)` against
            // `Option.None` being the common one. A union of an enum's cases
            // converts back to the enum, so the result is still usable
            // wherever the enum is expected.
            //
            // `checkIf` handles the case where an expected type does exist,
            // and checks both branches against it.
            val elseExpr = infer(value.expression, scope)
            elseExpr match {
              case _: BoundExpression.Error => elseExpr
              case _ =>
                BoundExpression.If(
                  node.ifKeyword.location,
                  cond,
                  thenExpr,
                  Option.Some(elseExpr),
                  Types.union(
                    binder.getType(thenExpr),
                    binder.getType(elseExpr)
                  )
                )
            }
        }
    }
  }

  def inferLiteral(
      node: Expression.Literal,
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

  /** `x`, `obj.field`, and `arr[i]` name a storage location; a call, a
    * constructor invocation, or an evidence-dispatched call do not, even though
    * they can appear syntactically on the left of `=` (`inferLHS` binds
    * `Expression.Call`/`Expression.New` structurally, not semantically).
    */
  def isAssignableLHS(lhs: BoundLeftHandSide): bool =
    lhs match {
      case _: BoundLeftHandSide.Variable      => true
      case _: BoundLeftHandSide.MemberAccess  => true
      case _: BoundLeftHandSide.Index         => true
      case _: BoundLeftHandSide.ArrayCreation => false
      case _: BoundLeftHandSide.Call          => false
      case _: BoundLeftHandSide.EvidenceCall  => false
      case _: BoundLeftHandSide.New           => false
    }

  def getLHSType(lhs: BoundLeftHandSide): Type = {
    lhs match {
      case BoundLeftHandSide.ArrayCreation(expression) =>
        binder.getType(expression)
      case BoundLeftHandSide.Call(expression) =>
        binder.getType(expression)
      case BoundLeftHandSide.EvidenceCall(expression) =>
        binder.getType(expression)
      case BoundLeftHandSide.Index(expression) =>
        binder.getType(expression)
      case BoundLeftHandSide.MemberAccess(expression) =>
        binder.getType(expression)
      case BoundLeftHandSide.New(expression) =>
        binder.getType(expression)

      case BoundLeftHandSide.Variable(_, variable) =>
        binder.getSymbolType(variable)
    }
  }

  def convertLHSToExpression(lhs: BoundLeftHandSide): BoundExpression = {
    lhs match {
      case BoundLeftHandSide.ArrayCreation(expression) =>
        expression
      case BoundLeftHandSide.Call(expression) =>
        expression
      case BoundLeftHandSide.EvidenceCall(expression) =>
        expression
      case BoundLeftHandSide.MemberAccess(expression) =>
        expression
      case BoundLeftHandSide.Index(expression) =>
        expression
      case BoundLeftHandSide.New(expression) =>
        expression
      case BoundLeftHandSide.Variable(location, variable) =>
        BoundExpression.Variable(
          location,
          variable,
          binder.tryGetSymbolType(variable)
        )
    }
  }

  def inferMemberAccess(
      node: Expression.MemberAccess,
      scope: Scope
  ): Result[BoundExpression.Error, BoundExpression.MemberAccess] = {
    // For member access, the left side doesn't need to be a left-hand side
    // It just needs to be a valid expression that produces a value
    val leftExpr = inferLHS(node.left, scope)
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

                bindMemberForSymbolAndType(leftType, right, scope) match {
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
                diagnosticBag.reportInvalidNamespace(
                  open.location.merge(close.location)
                )
                Result.Error(
                  BoundExpression.Error("Invalid alias in member access")
                )
              case SimpleNameSyntax.AliasSyntax(name, asKeyword, alias) =>
                diagnosticBag.reportInvalidNamespace(
                  name.location.merge(alias.location)
                )
                Result.Error(
                  BoundExpression.Error("Invalid alias in member access")
                )
              case SimpleNameSyntax.IdentifierNameSyntax(right) =>
                bindMemberForSymbolAndType(leftType, right, scope) match {
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

  /** Whether a call's target is a contextual extension rather than an ordinary
    * method. Only `bindMemberForSymbolAndType` produces one, and only where an
    * ordinary lookup could not answer.
    *
    * A trait's member is one reached through a record; a given's member is one
    * on a ground type, where the given is already known. Both call for the
    * value on the left of the dot to become the first argument, which is what
    * `bindEvidenceCall` does; only the dispatch differs.
    */
  def isTraitMemberAccess(function: BoundLeftHandSide): bool = {
    function match {
      case BoundLeftHandSide.MemberAccess(access) =>
        access.member.parent match {
          case Option.Some(owner) =>
            owner.kind == SymbolKind.Trait || owner.kind == SymbolKind.Given
          case Option.None => false
        }
      case _ => false
    }
  }

  /** `a.equals(b)` where `equals` came from evidence becomes `equals(a, b)`:
    * the trait declares both operands as parameters, so the value on the left
    * of the dot is the first argument, not a receiver.
    */
  def bindEvidenceCall(
      function: BoundLeftHandSide,
      args: List[BoundExpression],
      scope: Scope
  ): Result[BoundExpression.Error, BoundLeftHandSide] = {
    function match {
      case BoundLeftHandSide.MemberAccess(access) =>
        val receiver = convertLHSToExpression(access.receiver)
        val receiverType = binder.getType(receiver)
        val location = access.location

        binder.findEvidenceMember(
          receiverType,
          access.member.name,
          scope.current
        ) match {
          case Option.Some(KeyValue(evidence, member)) =>
            bindContextualCall(
              location,
              member,
              binder.getSymbolType(member),
              Option.Some(evidence),
              Option.None,
              receiver,
              args,
              scope
            )
          case Option.None =>
            // Nothing in scope holds evidence for the receiver's type, which
            // for a ground type is expected: the given is known already, and
            // its members are static, so this is an ordinary call. Resolving
            // it here rather than trusting the member the access already
            // carries is what gives the call its record and, for a conditional
            // given, the type arguments its signature is stated in.
            binder.findGivenExtensionForAny(
              binder.evidenceTypes(receiverType),
              access.member.name,
              location
            ) match {
              case Option.Some(found) =>
                bindContextualCall(
                  location,
                  found.member,
                  binder.instantiateGivenMember(found),
                  Option.None,
                  Option.Some(found.record),
                  receiver,
                  args,
                  scope
                )
              case Option.None =>
                diagnosticBag.reportSymbolNotFoundForType(
                  location,
                  receiverType,
                  access.member.name
                )
                Result.Error(
                  BoundExpression.Error(
                    "No evidence supplies " + access.member.name
                  )
                )
            }
        }
      case _ =>
        Result.Error(BoundExpression.Error("Expected a trait member access"))
    }
  }

  /** Binds the receiver and arguments against the member's parameters.
    *
    * `evidence` decides the dispatch: `Some` reads the member's slot out of a
    * record, `None` calls the given's static member directly.
    */
  def bindContextualCall(
      location: TextLocation,
      member: Symbol,
      memberType: Type,
      evidence: Option[BoundEvidence],
      record: Option[Symbol],
      receiver: BoundExpression,
      args: List[BoundExpression],
      scope: Scope
  ): Result[BoundExpression.Error, BoundLeftHandSide] = {
    // annotated: `length` is a member of the enum, and lookup on a bare
    // `List.Cons` does not consult it
    val allArgs: List[BoundExpression] = List.Cons(receiver, args)

    // `memberType` rather than the member's declared type: a conditional
    // given's member is stated in that given's type variables (ADR 0006).
    memberType match {
      case Type.Function(_, parameters, returnType) =>
        if (parameters.length != allArgs.length) {
          diagnosticBag.reportArgumentCountMismatch(
            location,
            // the receiver is one of them, so report what the user has to
            // write rather than the elaborated count
            parameters.length - 1,
            args.length
          )
          Result.Error(
            BoundExpression.Error(
              "Argument count mismatch calling " + member.name
            )
          )
        } else {
          val bound =
            bindArgumentsToTypes(getParameterTypes(parameters), allArgs, scope)

          evidence match {
            case Option.Some(symbol) =>
              Result.Success(
                BoundLeftHandSide.EvidenceCall(
                  BoundExpression.EvidenceCall(
                    location,
                    symbol,
                    member,
                    bound,
                    returnType
                  )
                )
              )
            case Option.None =>
              Result.Success(
                BoundLeftHandSide.Call(
                  BoundExpression.Call(
                    location,
                    Option.None,
                    member,
                    List.Nil,
                    binder.withRecordArgument(bound, record, location),
                    returnType
                  )
                )
              )
          }
        }
      case _ =>
        diagnosticBag.reportNotCallable(location)
        Result.Error(
          BoundExpression.Error(
            "Trait member " + member.name + " is not callable"
          )
        )
    }
  }

  def reportMemberNotFound(
      leftType: Type,
      right: SyntaxToken
  ): Either[string, Tuple2[Symbol, Type]] = {
    diagnosticBag.reportSymbolNotFoundForType(
      right.location,
      leftType,
      right.text
    )
    Either.Left(
      "Member not found: " + right.text + " for type: " + leftType.toString()
    )
  }

  /** A case's own members, then its enum's. `List.Cons` declares `head` and
    * `tail`; `reverse` is declared once, on `List`, and every case shares it.
    * `Symbol.lookupMember` only looks at `_children`, so a case's symbol never
    * saw the enum's members on its own — the case wins when both declare a
    * name, because this only reaches the enum when the case lookup fails.
    */
  def lookupMemberOrEnum(symbol: Symbol, name: string): Option[Symbol] = {
    symbol.lookupMember(name) match {
      case Option.Some(member) => Option.Some(member)
      case Option.None         => lookupEnumMember(symbol, name)
    }
  }

  /** `symbol` is an enum case exactly when it is a `Class` defined directly
    * inside an `Alias` — `bindEnumCases` calls `defineClass` on the scope
    * `bindEnum` entered on the enum's own symbol, which is what makes the case
    * a child of it. Anything else — an ordinary class, a type parameter's
    * symbol — has no enum to fall back to.
    */
  def lookupEnumMember(symbol: Symbol, name: string): Option[Symbol] = {
    if (symbol.kind == SymbolKind.Class) {
      symbol.parent match {
        case Option.None => Option.None
        case Option.Some(parent) =>
          if (parent.kind == SymbolKind.Alias) {
            parent.lookupMember(name)
          } else {
            Option.None
          }
      }
    } else {
      Option.None
    }
  }

  def bindMemberForSymbolAndType(
      leftType: Type,
      right: SyntaxToken,
      scope: Scope
  ): Either[string, Tuple2[Symbol, Type]] = {
    binder.getTypeSymbol(leftType) match {
      case Option.None =>
        // A type parameter has no members of its own, so an ordinary lookup was
        // never going to succeed. Evidence in scope may still supply the name
        // as a contextual extension (ADR 0004).
        binder.findEvidenceMember(leftType, right.text, scope.current) match {
          case Option.Some(KeyValue(_, member)) =>
            Either.Right(Tuple2(member, binder.getSymbolType(member)))
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
        }
      case Option.Some(symbol) =>
        lookupMemberOrEnum(symbol, right.text) match {
          case Option.None =>
            // The type has members but not this one, so a given may still
            // supply it as a contextual extension. Reached only after the
            // ordinary lookup has failed, which is what makes a member the
            // type declares itself always win (ADR 0004).
            binder.findGivenExtensionForAny(
              binder.evidenceTypes(leftType),
              right.text,
              right.location
            ) match {
              case Option.Some(found) =>
                Either.Right(
                  Tuple2(found.member, binder.instantiateGivenMember(found))
                )
              case Option.None =>
                reportMemberNotFound(leftType, right)
            }
          case Option.Some(member) =>
            // getSymbolType, not tryGetSymbolType: a member referenced from
            // inside the same type may not be typed yet, and getSymbolType
            // binds it on demand.
            binder.tryGetSymbolType(member) match {
              case Option.None =>
                Either.Right(
                  Tuple2(
                    member,
                    instantiateMemberType(
                      binder.getSymbolType(member),
                      leftType
                    )
                  )
                )
              case Option.Some(typ) =>
                Either.Right(
                  Tuple2(member, instantiateMemberType(typ, leftType))
                )
            }
        }
    }
  }

  /** A member's type is written in terms of its class's type parameters; the
    * receiver's arguments instantiate it. An enum is an alias, so its members
    * go through the same path.
    */
  def instantiateMemberType(memberType: Type, receiverType: Type): Type =
    receiverType match {
      case Type.Class(_, _, _, typeArgs, _) =>
        Types.substitute(memberType, typeArgs)
      case Type.Alias(_, _, _, typeArgs, _, _) =>
        Types.substitute(memberType, typeArgs)
      case _ => memberType
    }

  def resolveConstructorSymbol(
      name: NameSyntax,
      scope: Scope
  ): Result[BoundExpression.Error, Symbol] = {
    name match {
      case NameSyntax.SimpleName(
            SimpleNameSyntax.IdentifierNameSyntax(identifier)
          ) =>
        scope.lookup(identifier.text) match {
          case Option.None =>
            diagnosticBag.reportSymbolNotFound(
              identifier.location,
              identifier.text
            )
            Result.Error(
              BoundExpression.Error(
                "Constructor not found: " + identifier.text
              )
            )
          case Option.Some(symbol) =>
            // First check if this symbol itself is a constructor
            if (symbol.kind == SymbolKind.Constructor) {
              Result.Success(symbol)
            } else {
              // Look for the constructor inside the class/enum case
              val classScope = Scope(symbol, List.Nil)
              classScope.lookup(".ctor") match {
                case Option.None =>
                  diagnosticBag.reportSymbolNotFound(
                    identifier.location,
                    ".ctor"
                  )
                  Result.Error(
                    BoundExpression.Error(
                      "Constructor not found for: " + identifier.text
                    )
                  )
                case Option.Some(ctorSymbol) =>
                  Result.Success(ctorSymbol)
              }
            }
        }
      case NameSyntax.QualifiedName(
            left,
            _,
            SimpleNameSyntax.IdentifierNameSyntax(identifier)
          ) =>
        // Resolve the left part to get the type/namespace
        val leftScope = binder.bindNameToScope(left, scope)
        leftScope.lookup(identifier.text) match {
          case Option.None =>
            diagnosticBag.reportSymbolNotFound(
              identifier.location,
              identifier.text
            )
            Result.Error(
              BoundExpression.Error(
                "Constructor not found: " + identifier.text
              )
            )
          case Option.Some(symbol) =>
            // First check if this symbol itself is a constructor
            if (symbol.kind == SymbolKind.Constructor) {
              Result.Success(symbol)
            } else {
              // Look for the constructor inside the class/enum case
              val classScope = Scope(symbol, List.Nil)
              classScope.lookup(".ctor") match {
                case Option.None =>
                  diagnosticBag.reportSymbolNotFound(
                    identifier.location,
                    ".ctor"
                  )
                  Result.Error(
                    BoundExpression.Error(
                      "Constructor not found for: " + identifier.text
                    )
                  )
                case Option.Some(ctorSymbol) =>
                  Result.Success(ctorSymbol)
              }
            }
        }
      case _ =>
        diagnosticBag.reportInternalError(
          AstUtils.locationOfName(name),
          "resolveConstructorSymbol - unsupported name syntax"
        )
        Result.Error(
          BoundExpression.Error(
            "Unsupported constructor name syntax"
          )
        )
    }
  }

  def bindPattern(
      pattern: PatternSyntax,
      scope: Scope,
      expectedType: Type
  ): Result[BoundExpression.Error, BoundPattern] = {
    pattern match {
      case PatternSyntax.Literal(token) =>
        bindLiteralFromSyntaxToken(token) match {
          case Result.Error(value) => Result.Error(value)
          case Result.Success(literal) =>
            Result.Success(BoundPattern.Literal(literal))
        }
      case PatternSyntax.Discard(_) =>
        Result.Success(BoundPattern.Discard)
      case PatternSyntax.Identifier(identifier) =>
        bindIdentifierPattern(scope, identifier, expectedType)
      case PatternSyntax.Type(name) =>
        // A bare type pattern binds nothing, but it is still a test — the
        // type has to survive to the lowerer, which is what makes
        // `case Color.Red` different from `case _`.
        binder.bindTypeName(name, scope) match {
          case Type.Error(message) =>
            Result.Error(BoundExpression.Error(message))
          case typ => Result.Success(BoundPattern.TypeTest(typ))
        }
      case PatternSyntax.TypeAssertion(innerPattern, typeAnnotation) =>
        // The annotation, not the scrutinee, is what the inner pattern sees —
        // and it is a test as well as a type, or `case x: int` would catch
        // every value and bind it.
        binder.bindTypeName(typeAnnotation.typ, scope) match {
          case Type.Error(message) =>
            Result.Error(BoundExpression.Error(message))
          case annotatedType =>
            bindPattern(innerPattern, scope, annotatedType) match {
              case Result.Error(error) => Result.Error(error)
              case Result.Success(inner) =>
                Result.Success(BoundPattern.Typed(annotatedType, inner))
            }
        }
      case PatternSyntax.Extract(constructorName, _, patterns, _) =>
        resolveConstructorSymbol(constructorName, scope) match {
          case Result.Error(error) => Result.Error(error)
          case Result.Success(constructor) =>
            val typeArgs = patternTypeArguments(expectedType, constructor)
            getFunctionParameterTypes(constructor, typeArgs) match {
              case Either.Left(error) =>
                diagnosticBag.reportNotCallable(
                  AstUtils.locationOfName(constructorName)
                )
                Result.Error(
                  BoundExpression.Error(
                    "Constructor is not callable: " + constructor.name
                  )
                )
              case Either.Right(parameterTypes) =>
                if (patterns.length != parameterTypes.length) {
                  diagnosticBag.reportInternalError(
                    AstUtils.locationOfName(constructorName),
                    "Pattern parameter count mismatch"
                  )
                  Result.Error(
                    BoundExpression.Error("Parameter count mismatch")
                  )
                } else {
                  val boundPatterns = new Array[BoundPattern](patterns.length)

                  var i = 0
                  var hasError = false
                  var errorResult: Option[BoundExpression.Error] = Option.None

                  while (i < patterns.length && !hasError) {
                    bindPattern(
                      patterns(i).pattern,
                      scope,
                      parameterTypes(i)
                    ) match {
                      case Result.Error(error) =>
                        hasError = true
                        errorResult = Option.Some(error)
                      case Result.Success(pattern) =>
                        boundPatterns(i) = pattern
                        i = i + 1
                    }
                  }

                  errorResult match {
                    case Option.None =>
                      Result.Success(
                        BoundPattern.Extract(constructor, boundPatterns)
                      )
                    case Option.Some(value) =>
                      Result.Error(value)
                  }
                }
            }
        }
    }
  }

  /** The type arguments a constructor pattern inherits from the value it
    * destructures. A case's parameter list is its enum's, so the enum's
    * arguments apply directly; a class carries its own. Anything else has no
    * arguments to give, and the parameters bind as declared.
    */
  def patternTypeArguments(
      expectedType: Type,
      constructor: Symbol
  ): List[Type] = {
    expectedType match {
      case Type.Alias(_, _, _, args, _, _) => args
      case Type.Class(_, _, _, args, _)    => args
      case Type.Union(_, cases) =>
        constructor.parent match {
          case Option.Some(caseSymbol) => unionCaseArguments(cases, caseSymbol)
          case Option.None             => List.Nil
        }
      case _ => List.Nil
    }
  }

  def unionCaseArguments(cases: List[Type], caseSymbol: Symbol): List[Type] = {
    cases match {
      case List.Nil => List.Nil
      case List.Cons(head, tail) =>
        head match {
          case Type.Class(_, _, _, args, symbol) =>
            if (symbol == caseSymbol) args
            else unionCaseArguments(tail, caseSymbol)
          case Type.Alias(_, _, _, args, _, symbol) =>
            if (typeInference.isCaseOf(caseSymbol, symbol)) args
            else unionCaseArguments(tail, caseSymbol)
          case _ => unionCaseArguments(tail, caseSymbol)
        }
    }
  }

  def getFunctionParameterTypes(
      symbol: Symbol,
      typeArgs: List[Type]
  ): Either[Type.Error, Array[Type]] = {
    binder.getSymbolType(symbol) match {
      case Type.Error(message) => Either.Left(Type.Error(message))
      case f: Type.Function =>
        Either.Right(
          instantiatedParameterTypes(f.parameters, typeArgs)
        )
      case gf: Type.GenericFunction =>
        Either.Right(
          instantiatedParameterTypes(gf.parameters, typeArgs)
        )
      case x =>
        diagnosticBag.reportNotCallable(symbol.location)
        Either.Left(Type.Error("Symbol is not a function: " + symbol.name))
    }
  }

  def instantiatedParameterTypes(
      parameters: List[BoundParameter],
      typeArgs: List[Type]
  ): Array[Type] = {
    val paramTypes =
      Types.substituteList(getParameterTypes(parameters), typeArgs)
    val result = new Array[Type](paramTypes.length)
    fillParameterTypes(result, 0, paramTypes)
    result
  }

  def fillParameterTypes(
      array: Array[Type],
      index: int,
      list: List[Type]
  ): unit = {
    list match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        array(index) = head
        fillParameterTypes(array, index + 1, tail)
    }
  }

  def bindIdentifierPattern(
      scope: Scope,
      identifier: SyntaxToken,
      expectedType: Type
  ): Result[BoundExpression.Error, BoundPattern] = {
    // Create a new local variable for the pattern binding
    scope.defineLocal(identifier.text, identifier.location, true) match {
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
        // For now, set a generic type - proper type inference would be complex
        // TODO: Implement proper pattern variable type inference

        binder.setSymbolType(symbol, expectedType)
        Result.Success(BoundPattern.Variable(symbol))
    }
  }

  def bindLiteralFromSyntaxToken(
      token: SyntaxToken
  ): Result[BoundExpression.Error, BoundLiteral] = {
    token.value match {
      case SyntaxTokenValue.Number(value) =>
        Result.Success(BoundLiteral.Int(token.location, value))
      case SyntaxTokenValue.String(value) =>
        Result.Success(BoundLiteral.String(token.location, value))
      case SyntaxTokenValue.Boolean(value) =>
        Result.Success(BoundLiteral.Bool(token.location, value))
      case SyntaxTokenValue.Character(value) =>
        Result.Success(BoundLiteral.Char(token.location, value))
      case _ =>
        diagnosticBag.reportInvalidPattern(token.location)
        Result.Error(
          BoundExpression.Error(
            "Invalid literal pattern: " + token.text
          )
        )
    }
  }

  def inferMatchCase(
      matchCase: MatchCaseSyntax,
      scrutineeType: Type,
      scope: Scope
  ): Result[BoundExpression.Error, BoundMatchCase] = {
    // Create a new scope for this case to allow pattern variables
    val caseScope = scope.newBlock()

    bindPattern(matchCase.pattern, caseScope, scrutineeType) match {
      case Result.Error(error)          => Result.Error(error)
      case Result.Success(boundPattern) =>
        // Bind the statements and expression within the case scope
        val statements = bindStatements(matchCase.block.statements, caseScope)
        val resultExpr = matchCase.block.expression match {
          case Option.None =>
            BoundExpression.Unit(TextLocationFactory.empty())
          case Option.Some(expr) => infer(expr, caseScope)
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

  def checkMatchCase(
      matchCase: MatchCaseSyntax,
      scrutineeType: Type,
      expectedType: Type,
      scope: Scope
  ): Result[BoundExpression.Error, BoundMatchCase] = {
    // Create a new scope for this case to allow pattern variables
    val caseScope = scope.newBlock()

    bindPattern(matchCase.pattern, caseScope, scrutineeType) match {
      case Result.Error(error)          => Result.Error(error)
      case Result.Success(boundPattern) =>
        // Bind the statements and expression within the case scope
        val statements = bindStatements(matchCase.block.statements, caseScope)
        val resultExpr = matchCase.block.expression match {
          case Option.None =>
            BoundExpression.Unit(TextLocationFactory.empty())
          case Option.Some(expr) => check(expr, expectedType, caseScope)
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

  def checkMatchCases(
      head: MatchCaseSyntax,
      tail: List[MatchCaseSyntax],
      scrutineeType: Type,
      expectedType: Type,
      scope: Scope
  ): Result[BoundExpression.Error, NonEmptyList[BoundMatchCase]] = {
    checkMatchCase(head, scrutineeType, expectedType, scope) match {
      case Result.Error(expr) =>
        Result.Error(expr)
      case Result.Success(boundCase) =>
        tail match {
          case List.Nil =>
            Result.Success(NonEmptyList(boundCase, List.Nil))
          case List.Cons(head, tail) =>
            checkMatchCases(
              head,
              tail,
              scrutineeType,
              expectedType,
              scope
            ) match {
              case Result.Error(expr) => Result.Error(expr)
              case Result.Success(tailCases) =>
                Result.Success(NonEmptyList(boundCase, tailCases.toList()))
            }
        }
    }
  }

  def inferMatchCases(
      head: MatchCaseSyntax,
      tail: List[MatchCaseSyntax],
      scrutineeType: Type,
      scope: Scope
  ): Result[BoundExpression.Error, NonEmptyList[BoundMatchCase]] = {
    inferMatchCase(head, scrutineeType, scope) match {
      case Result.Error(expr) =>
        Result.Error(expr)
      case Result.Success(boundCase) =>
        tail match {
          case List.Nil =>
            Result.Success(NonEmptyList(boundCase, List.Nil))
          case List.Cons(head, tail) =>
            inferMatchCases(head, tail, scrutineeType, scope) match {
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

  def inferMatchExpression(
      node: Expression.Match,
      scope: Scope
  ): BoundExpression = {
    // Bind the expression being matched against
    val matchedExpr = infer(node.expression, scope)

    matchedExpr match {
      case error: BoundExpression.Error => error
      case _                            =>
        // Bind all the match cases
        inferMatchCases(
          node.cases.head,
          node.cases.tail,
          binder.getType(matchedExpr),
          scope
        ) match {
          case Result.Error(value)        => value
          case Result.Success(boundCases) =>
            // Calculate the result type from all cases
            val headType = binder.getType(boundCases.head.result)
            val resultType = calculateMatchResultType(headType, boundCases.tail)
            val location = AstUtils.locationOfExpression(node)

            BoundExpression.Match(
              location,
              resultType,
              matchedExpr,
              boundCases
            )
        }
    }
  }

  def inferNew(
      node: Expression.New,
      scope: Scope
  ): Result[BoundExpression.Error, BoundLeftHandSide] =
    bindNew(node, Option.None, scope)

  def reportTraitNotInstantiable(
      node: Expression.New,
      name: string
  ): Result[BoundExpression.Error, BoundLeftHandSide] = {
    diagnosticBag.reportTraitNotInstantiable(
      AstUtils.locationOfName(node.name),
      name
    )
    Result.Error(
      BoundExpression.Error("Trait " + name + " cannot be instantiated")
    )
  }

  def bindNew(
      node: Expression.New,
      expectedType: Option[Type],
      scope: Scope
  ): Result[BoundExpression.Error, BoundLeftHandSide] = {
    val instantiationType = binder.bindTypeName(node.name, scope)
    instantiationType match {
      case Type.Error(message) => Result.Error(BoundExpression.Error(message))
      case Type.Class(classLocation, ns, name, typeArgs, symbol) =>
        // Special handling for Array construction - convert to ArrayCreation
        if (name == "Array") {
          val elementType = typeArgs match {
            case List.Cons(elemType, List.Nil) => elemType
            case _                             => binder.anyType
          }
          val argsList = bindExpressions(
            fromExpressionList(node.arguments.expressions),
            scope
          )
          argsList match {
            case List.Cons(sizeArg, List.Nil) =>
              val boundSize = bindConversion(sizeArg, binder.intType, false)
              val location = AstUtils.locationOfExpression(node)
              val arrayCreation = new BoundExpression.ArrayCreation(
                location,
                elementType,
                boundSize,
                instantiationType // This is the Array[T] type
              )

              Result.Success(BoundLeftHandSide.ArrayCreation(arrayCreation))
            case _ =>
              val location = AstUtils.locationOfExpression(node)
              diagnosticBag.reportArgumentCountMismatch(
                location,
                1,
                argsList.length
              )
              Result.Error(
                BoundExpression.Error(
                  "Array constructor requires exactly one argument (size) but got " +
                    string(argsList.length)
                )
              )
          }
        } else if (symbol.kind == SymbolKind.Trait) {
          reportTraitNotInstantiable(node, name)
        } else {
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

              // The type arguments, if any, were written at the call site
              bindGenericConstructor(
                location,
                ctor,
                classLocation,
                ns,
                name,
                symbol,
                typeArgs,
                args,
                expectedType,
                scope
              )
          }
        }
      case Type.GenericClass(_, ns, name, _, symbol) =>
        if (symbol.kind == SymbolKind.Trait) {
          reportTraitNotInstantiable(node, name)
        } else
          findConstructor(symbol) match {
            case Option.None =>
              diagnosticBag.reportSymbolNotFound(
                AstUtils.locationOfName(node.name),
                name
              )
              Result.Error(
                BoundExpression.Error(
                  "Cannot find constructor for generic class: " + name
                )
              )
            case Option.Some(ctor) =>
              val args = bindExpressions(
                fromExpressionList(node.arguments.expressions),
                scope
              )
              val location = AstUtils.locationOfExpression(node)

              bindGenericConstructor(
                location,
                ctor,
                location,
                ns,
                name,
                symbol,
                List.Nil,
                args,
                expectedType,
                scope
              )
          }
      case _ =>
        println(node.closeParen.location.toString())
        panic("expected named type, got " + string(instantiationType))
    }
  }

  def inferUnary(
      node: Expression.Unary,
      scope: Scope
  ): BoundExpression = {
    val op = bindUnaryOperator(node.operator)
    infer(node.expression, scope) match {
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
                BoundExpression.Unary(
                  node.operator.location,
                  op,
                  operand,
                  resultType
                )
            }
        }
    }

  }

  def inferUnit(
      node: Expression.Unit,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Unit(
      node.openParen.location.merge(node.closeParen.location)
    )

  def inferWhileExpression(
      node: Expression.While,
      scope: Scope
  ): BoundExpression = {
    val cond = infer(node.condition, scope)
    val body = infer(node.body, scope)
    new BoundExpression.While(node.whileKeyword.location, cond, body)
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
    val isReadOnly = statement.valOrVarKeyword.kind == SyntaxKind.ValKeyword
    scope.defineLocal(name, location, isReadOnly) match {
      case Either.Left(originalLocation) =>
        // symbol is already defined in this scope
        diagnosticBag.reportDuplicateDefinition(
          name,
          originalLocation,
          location
        )
        BoundStatement.Error
      case Either.Right(symbol) =>
        statement.typeAnnotation match {
          case Option.None =>
            // no type annotation so lets use type inference
            val expr = infer(statement.expression, scope)
            // no type annotation, so we need to infer the type from the expr
            val typ = binder.getType(expr)
            binder.setSymbolType(symbol, typ)
            BoundStatement.VariableDeclaration(symbol, isReadOnly, typ, expr)

          case Option.Some(value) =>
            val annotatedType = binder.bindTypeName(value.typ, scope)
            val boundExpr = check(statement.expression, annotatedType, scope)
            binder.setSymbolType(symbol, annotatedType)

            BoundStatement.VariableDeclaration(
              symbol,
              isReadOnly,
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
    // break parses, but there is no lowering for it yet. Reject it with a
    // diagnostic rather than taking the compiler down.
    diagnosticBag.reportUnsupportedStatement(
      statement.breakKeyword.location,
      "break"
    )
    BoundStatement.Error
  }
  def bindContinueStatement(
      statement: StatementSyntax.ContinueStatement,
      scope: Scope
  ): BoundStatement = {
    diagnosticBag.reportUnsupportedStatement(
      statement.continueKeyword.location,
      "continue"
    )
    BoundStatement.Error
  }
  def bindExpressionStatement(
      statement: StatementSyntax.ExpressionStatement,
      scope: Scope
  ): BoundStatement = {
    val expr = infer(statement.expression, scope)
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
