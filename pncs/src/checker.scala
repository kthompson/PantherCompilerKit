//import Type.Reference
import panther._

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
    diagnosticBag: DiagnosticBag
) {

  val binaryOperators = new BinaryOperators()

  val emptyTypeArray: List[Type] = List.Nil

  // ensure these type are kept in sync with the Binder
  val stringType = Type.Named("panther", "string", emptyTypeArray)
  val intType = Type.Named("panther", "int", emptyTypeArray)
  val boolType = Type.Named("panther", "bool", emptyTypeArray)

  // initialize binary operators
  simpleOp(intType, BinaryOperatorKind.Plus)
  simpleOp(intType, BinaryOperatorKind.Minus)
  simpleOp(intType, BinaryOperatorKind.Multiply)
  simpleOp(intType, BinaryOperatorKind.Divide)
  simpleOp(intType, BinaryOperatorKind.Modulus)
  equalityOp(intType, BinaryOperatorKind.Equals)
  equalityOp(intType, BinaryOperatorKind.NotEquals)
  equalityOp(intType, BinaryOperatorKind.LessThan)
  equalityOp(intType, BinaryOperatorKind.LessThanOrEqual)
  equalityOp(intType, BinaryOperatorKind.GreaterThan)
  equalityOp(intType, BinaryOperatorKind.GreaterThanOrEqual)
  equalityOp(boolType, BinaryOperatorKind.Equals)
  equalityOp(boolType, BinaryOperatorKind.NotEquals)
  equalityOp(stringType, BinaryOperatorKind.Equals)
  equalityOp(stringType, BinaryOperatorKind.NotEquals)

  def simpleOp(typ: Type, operator: BinaryOperatorKind): unit =
    binaryOperators.addOperator(BinaryOperator(typ, typ, operator, typ))

  def equalityOp(typ: Type, operator: BinaryOperatorKind): unit =
    binaryOperators.addOperator(BinaryOperator(typ, typ, operator, boolType))

  /**   1. Base rules:
    *      - Never is a subtype of all types.
    *      - All types are subtypes of Any.
    *      - A type is a subtype of itself.
    *   2. Function Subtyping:
    *   - Function types are covariant in the return type and contravariant in
    *     the parameter types:
    *     - (A1, A2) => R1 is a subtype of (B1, B2) => R2 if:
    *     - B1 is a subtype of A1 (contravariant in parameters),
    *     - B2 is a subtype of A2 (contravariant in parameters), and
    *     - R1 is a subtype of R2 (covariant in the return type).
    *
    *   3. Array Subtyping:
    *   - ArrayType(T1) is a subtype of ArrayType(T2) if T1 is a subtype of T2.
    *
    *   4. Option Subtyping:
    *      - OptionType(T1) is a subtype of OptionType(T2) if T1 is a subtype of
    *        T2.
    *   5. Reference Types:
    *   - Reference(S1) is a subtype of Reference(S2) if they refer to the same
    * symbol (assuming no polymorphism in this example).
    */
  def isSubtype(t1: Type, t2: Type): bool = {
    // Reflexivity: a type is a subtype of itself
    if (t1 == t2) true
    else {
      TypePair(t1, t2) match {
        // `Never` is a subtype of everything
        case TypePair(Type.Never, _) => true

        // Everything is a subtype of `Any`
        case TypePair(_, Type.Any) => true

        // Function subtyping: contravariant in parameters, covariant in return type
        case TypePair(
              Type.Function(genTypeParams1, params1, return1),
              Type.Function(genTypeParams2, params2, return2)
            ) =>
          assert(genTypeParams1.length == 0, "generics not supported")
          assert(genTypeParams2.length == 0, "generics not supported")

          isSubtype(return1, return2) && areParametersSubtype(params2, params1)

        // Array subtyping: inner types must be subtypes
        case TypePair(
              Type.Named(ns1, name1, args1),
              Type.Named(ns2, name2, args2)
            ) =>
          if (ns1 == ns2 && name1 == name2) {
            areArgsSubtype(args1, args2)
          } else {
            false
          }

        // Error type: not a subtype of anything
        case TypePair(Type.Error, _) => false

        // Otherwise, not a subtype
        case _ => false
      }
    }
  }

  def areArgsSubtype(args1: List[Type], args2: List[Type]): bool = {
    args1 match {
      case List.Nil =>
        args2 match {
          case List.Nil        => true
          case List.Cons(_, _) => false
        }
      case List.Cons(head1, tail1) =>
        args2 match {
          case List.Nil => false
          case List.Cons(head2, tail2) =>
            if (isSubtype(head1, head2)) {
              areArgsSubtype(tail1, tail2)
            } else {
              false
            }
        }
    }
  }

  def areParametersSubtype(
      params1: List[BoundParameter],
      params2: List[BoundParameter]
  ): bool = {
    params1 match {
      case List.Nil =>
        params2 match {
          case List.Nil        => true
          case List.Cons(_, _) => false
        }
      case List.Cons(parameter, tail) =>
        params2 match {
          case List.Nil => false
          case List.Cons(parameter2, tail2) =>
            isSubtype(parameter.typ, parameter2.typ) && areParametersSubtype(
              tail,
              tail2
            )
        }
    }
  }

  def checkBinaryOperator(
      left: Type,
      right: Type,
      operator: BinaryOperatorKind
  ): Type = {
    _checkBinaryOperator(0, left, right, operator)
  }

  def _checkBinaryOperator(
      i: int,
      left: Type,
      right: Type,
      operator: BinaryOperatorKind
  ): Type =
    if (i >= binaryOperators.size) panic("todo")
    else {
      val op = binaryOperators.operators(i)
      if (op.left == left && op.right == right && op.operator == operator) {
        op.result
      } else {
        _checkBinaryOperator(i + 1, left, right, operator)
      }
    }

  def getStringMemberType(member: string): Type = {
    member match {
      case "length" => intType
      case _ =>
        panic("unknown string member: " + member)
        Type.Error
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
      case node: Expression.IndexExpression => bindIndexExpression(node, scope)
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

  def bindArrayCreationExpression(
      node: Expression.ArrayCreationExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

  def bindAssignmentExpression(
      node: Expression.AssignmentExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

  def bindBinaryExpression(
      node: Expression.BinaryExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

  def bindBlockExpression(
      node: Expression.BlockExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

  def bindCallExpression(
      node: Expression.CallExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

  def bindForExpression(
      node: Expression.ForExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

  def bindGroupExpression(
      node: Expression.GroupExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

  def bindIdentifierName(
      node: Expression.IdentifierName,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

  def bindIf(node: Expression.If, scope: Scope): BoundExpression =
    BoundExpression.Error

  def bindIndexExpression(
      node: Expression.IndexExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

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

        BoundExpression.Error
    }
  }

  def bindMemberAccessExpression(
      node: Expression.MemberAccessExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

  def bindMatchExpression(
      node: Expression.MatchExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

  def bindNewExpression(
      node: Expression.NewExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

  def bindUnaryExpression(
      node: Expression.UnaryExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

  def bindUnitExpression(
      node: Expression.UnitExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

  def bindWhileExpression(
      node: Expression.WhileExpression,
      scope: Scope
  ): BoundExpression =
    BoundExpression.Error

}
