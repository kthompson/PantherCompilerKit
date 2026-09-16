import panther._

case class BinaryOperators(binder: Binder) {

  /** The builtin table only.
    *
    * Reference equality used to be folded in here. It is now a separate
    * question the caller asks last, because evidence has to come first: a type
    * with `Eq` evidence must compare through that evidence rather than by
    * identity, and folding the identity rule in here would answer before the
    * evidence was ever consulted ([ADR
    * 0004](../../../docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)).
    */
  def checkBinary(
      left: Type,
      right: Type,
      operator: BinaryOperatorKind
  ): Type = {
    val leftCode = builtinTypeCode(left)
    val rightCode = builtinTypeCode(right)
    val operatorCode = binaryOperatorCode(operator)
    if (leftCode == 1 && rightCode == 1) {
      if (
        operatorCode <= 4 ||
        (operatorCode >= 13 && operatorCode <= 17)
      ) binder.intType
      else if (operatorCode >= 5 && operatorCode <= 10) binder.boolType
      else missingBinary(left, right, operator)
    } else if (leftCode == 2 && rightCode == 2) {
      if (
        operatorCode == 5 || operatorCode == 6 ||
        (operatorCode >= 11 && operatorCode <= 15)
      ) binder.boolType
      else missingBinary(left, right, operator)
    } else if (leftCode == 3 && rightCode == 3) {
      if (operatorCode >= 5 && operatorCode <= 10) binder.boolType
      else if (operatorCode == 1) binder.intType
      else missingBinary(left, right, operator)
    } else if (leftCode == 3 && rightCode == 1) {
      if (operatorCode == 0 || operatorCode == 1) binder.intType
      else missingBinary(left, right, operator)
    } else if (leftCode == 4 && rightCode == 4) {
      if (operatorCode == 0) binder.stringType
      else if (operatorCode >= 5 && operatorCode <= 10) binder.boolType
      else missingBinary(left, right, operator)
    } else missingBinary(left, right, operator)
  }

  def missingBinary(
      left: Type,
      right: Type,
      operator: BinaryOperatorKind
  ): Type = {
    Type.Error(
      "Binary operator '" + string(operator) + "' not found for types '" +
        left.toString() + "' and '" + right.toString() + "'."
    )
  }

  /** [ADR
    * 0003](../../../docs/architecture/adr/0003-equality-on-reference-types.md)'s
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
    builtinTypeCode(typ) != 0

  /** Rows in this table are only the root prelude's non-generic value types.
    * Comparing their full recursive `Type` values invokes derived equality over
    * locations and symbol graphs in the self-hosted compiler. The table's
    * actual key is the canonical builtin name.
    */
  def builtinTypeCode(typ: Type): int =
    typ match {
      case Type.Class(_, _, name, _, _) =>
        if (name == "int") 1
        else if (name == "bool") 2
        else if (name == "char") 3
        else if (name == "string") 4
        else if (name == "unit") 5
        else 0
      case _ => 0
    }

  /** Use compact keys for the builtin table. Besides avoiding allocations, this
    * does not depend on derived equality dispatch while the compiler is
    * compiling its own derived enum implementations.
    */
  def binaryOperatorCode(operator: BinaryOperatorKind): int =
    operator match {
      case BinaryOperatorKind.Plus               => 0
      case BinaryOperatorKind.Minus              => 1
      case BinaryOperatorKind.Multiply           => 2
      case BinaryOperatorKind.Divide             => 3
      case BinaryOperatorKind.Modulus            => 4
      case BinaryOperatorKind.Equals             => 5
      case BinaryOperatorKind.NotEquals          => 6
      case BinaryOperatorKind.LessThan           => 7
      case BinaryOperatorKind.LessThanOrEqual    => 8
      case BinaryOperatorKind.GreaterThan        => 9
      case BinaryOperatorKind.GreaterThanOrEqual => 10
      case BinaryOperatorKind.LogicalAnd         => 11
      case BinaryOperatorKind.LogicalOr          => 12
      case BinaryOperatorKind.BitwiseAnd         => 13
      case BinaryOperatorKind.BitwiseOr          => 14
      case BinaryOperatorKind.BitwiseXor         => 15
      case BinaryOperatorKind.ShiftLeft          => 16
      case BinaryOperatorKind.ShiftRight         => 17
      case BinaryOperatorKind.Error              => 18
    }

  def unaryOperatorCode(operator: UnaryOperatorKind): int =
    operator match {
      case UnaryOperatorKind.Identity        => 0
      case UnaryOperatorKind.Negation        => 1
      case UnaryOperatorKind.LogicalNegation => 2
      case UnaryOperatorKind.BitwiseNegation => 3
      case UnaryOperatorKind.Error           => 4
    }

  def checkUnary(operand: Type, operator: UnaryOperatorKind): Type = {
    val typeCode = builtinTypeCode(operand)
    val operatorCode = unaryOperatorCode(operator)
    if (
      typeCode == 1 && (operatorCode == 0 || operatorCode == 1 || operatorCode == 3)
    )
      binder.intType
    else if (typeCode == 2 && operatorCode == 2) binder.boolType
    else
      Type.Error(
        "Unary operator '" + string(operator) + "' not found for type '" +
          operand.toString() + "'."
      )
  }
}
