import panther._

case class BinaryOperators(binder: Binder) {

  var binaryOps: List[BinaryOperator] = List.Nil
  var unaryOps: List[UnaryOperator] = List.Nil

  // initialize unary operators
  unaryOp(binder.intType, UnaryOperatorKind.Identity)
  unaryOp(binder.intType, UnaryOperatorKind.Negation)
  unaryOp(binder.intType, UnaryOperatorKind.BitwiseNegation)
  unaryOp(binder.boolType, UnaryOperatorKind.LogicalNegation)

  // initialize binary operators
  simpleOp(binder.intType, BinaryOperatorKind.BitwiseAnd)
  simpleOp(binder.intType, BinaryOperatorKind.BitwiseOr)
  simpleOp(binder.intType, BinaryOperatorKind.BitwiseXor)
  simpleOp(binder.intType, BinaryOperatorKind.Divide)
  simpleOp(binder.intType, BinaryOperatorKind.Minus)
  simpleOp(binder.intType, BinaryOperatorKind.Modulus)
  simpleOp(binder.intType, BinaryOperatorKind.Multiply)
  simpleOp(binder.intType, BinaryOperatorKind.Plus)
  simpleOp(binder.intType, BinaryOperatorKind.ShiftLeft)
  simpleOp(binder.intType, BinaryOperatorKind.ShiftRight)

  simpleOp(binder.boolType, BinaryOperatorKind.LogicalOr)
  simpleOp(binder.boolType, BinaryOperatorKind.LogicalAnd)
  simpleOp(binder.boolType, BinaryOperatorKind.BitwiseAnd)
  simpleOp(binder.boolType, BinaryOperatorKind.BitwiseOr)
  simpleOp(binder.boolType, BinaryOperatorKind.BitwiseXor)

  equalityOp(binder.charType, BinaryOperatorKind.Equals)
  equalityOp(binder.charType, BinaryOperatorKind.NotEquals)
  equalityOp(binder.charType, BinaryOperatorKind.LessThan)
  equalityOp(binder.charType, BinaryOperatorKind.LessThanOrEqual)
  equalityOp(binder.charType, BinaryOperatorKind.GreaterThan)
  equalityOp(binder.charType, BinaryOperatorKind.GreaterThanOrEqual)

  equalityOp(binder.intType, BinaryOperatorKind.Equals)
  equalityOp(binder.intType, BinaryOperatorKind.NotEquals)
  equalityOp(binder.intType, BinaryOperatorKind.LessThan)
  equalityOp(binder.intType, BinaryOperatorKind.LessThanOrEqual)
  equalityOp(binder.intType, BinaryOperatorKind.GreaterThan)
  equalityOp(binder.intType, BinaryOperatorKind.GreaterThanOrEqual)

  equalityOp(binder.boolType, BinaryOperatorKind.Equals)
  equalityOp(binder.boolType, BinaryOperatorKind.NotEquals)

  equalityOp(binder.stringType, BinaryOperatorKind.Equals)
  equalityOp(binder.stringType, BinaryOperatorKind.NotEquals)
  // Ordering is lexicographic. The VM compares Value.String directly for Clt
  // and Cgt, and the emitter builds <= and >= from those two, so all four
  // work end to end.
  equalityOp(binder.stringType, BinaryOperatorKind.LessThan)
  equalityOp(binder.stringType, BinaryOperatorKind.LessThanOrEqual)
  equalityOp(binder.stringType, BinaryOperatorKind.GreaterThan)
  equalityOp(binder.stringType, BinaryOperatorKind.GreaterThanOrEqual)
  binaryOp(
    binder.stringType,
    binder.stringType,
    BinaryOperatorKind.Plus,
    binder.stringType
  )

  def addOperator(operator: BinaryOperator): unit = {
    binaryOps = List.Cons(operator, binaryOps)
  }

  def unaryOp(typ: Type, operator: UnaryOperatorKind): unit =
    addUnaryOperator(UnaryOperator(typ, operator, typ))

  def addUnaryOperator(operator: UnaryOperator): unit =
    unaryOps = List.Cons(operator, unaryOps)

  def simpleOp(typ: Type, operator: BinaryOperatorKind): unit =
    binaryOp(typ, typ, operator, typ)

  def binaryOp(
      left: Type,
      right: Type,
      operator: BinaryOperatorKind,
      result: Type
  ): unit =
    addOperator(new BinaryOperator(left, right, operator, result))

  def equalityOp(typ: Type, operator: BinaryOperatorKind): unit =
    binaryOp(typ, typ, operator, binder.boolType)

  /** The builtin table only.
    *
    * Reference equality used to be folded in here. It is now a separate
    * question the caller asks last, because evidence has to come first: a type
    * with `Eq` evidence must compare through that evidence rather than by
    * identity, and folding the identity rule in here would answer before the
    * evidence was ever consulted
    * ([ADR 0004](../../../docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)).
    */
  def checkBinary(
      left: Type,
      right: Type,
      operator: BinaryOperatorKind
  ): Type = _checkBinary(binaryOps, left, right, operator)

  /** [ADR 0003](../../../docs/architecture/adr/0003-equality-on-reference-types.md)'s
    * rule, now the last resort rather than part of the table lookup.
    */
  def referenceEquality(
      left: Type,
      right: Type,
      operator: BinaryOperatorKind
  ): bool = isEquality(operator) && referenceEqualityApplies(left, right)

  def isEquality(operator: BinaryOperatorKind): bool =
    operator match {
      case BinaryOperatorKind.Equals    => true
      case BinaryOperatorKind.NotEquals => true
      case _                            => false
    }

  /** `==` and `!=` between two reference types, which the table above cannot
    * enumerate because the types are declared by the program. The rule is that
    * one side has to widen to the other without a cast, so a case can be
    * compared against its own enum (`kind == SymbolKind.Field`) and a type
    * against itself, while unrelated types stay a diagnostic.
    *
    * Value types are excluded. Their comparisons come from the table, and what
    * it rejects there — `string == char` — is a real mismatch, not a missing
    * entry.
    */
  def referenceEqualityApplies(left: Type, right: Type): bool = {
    if (isValueType(left) || isValueType(right)) false
    else {
      val classifier = binder.classifier
      classifier.widensTo(left, right) || classifier.widensTo(right, left)
    }
  }

  def isValueType(typ: Type): bool =
    typ == binder.intType || typ == binder.boolType ||
      typ == binder.charType || typ == binder.stringType ||
      typ == binder.unitType

  def _checkBinary(
      ops: List[BinaryOperator],
      left: Type,
      right: Type,
      operator: BinaryOperatorKind
  ): Type =
    ops match {
      case List.Nil =>
        Type.Error(
          "Binary operator '" + operator + "' not found for types '" + left + "' and '" + right + "'."
        )
      case List.Cons(op, tail) =>
        if (op.left == left && op.right == right && op.operator == operator) {
          op.result
        } else {
          _checkBinary(tail, left, right, operator)
        }
    }

  def checkUnary(operand: Type, operator: UnaryOperatorKind): Type = {
    _checkUnary(unaryOps, operand, operator)
  }

  def _checkUnary(
      ops: List[UnaryOperator],
      operand: Type,
      operator: UnaryOperatorKind
  ): Type =
    ops match {
      case List.Nil =>
        Type.Error(
          "Unary operator '" + operator + "' not found for type '" + operand + "'."
        )
      case List.Cons(op, tail) =>
        if (op.operand == operand && op.operator == operator) {
          op.result
        } else {
          _checkUnary(tail, operand, operator)
        }
    }
}
