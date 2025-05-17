enum BinaryOperatorKind {
  case Plus
  case Minus
  case Multiply
  case Divide
  case Modulus
  case Equals
  case NotEquals
  case LessThan
  case LessThanOrEqual
  case GreaterThan
  case GreaterThanOrEqual
  case LogicalAnd // booleans
  case LogicalOr
  case BitwiseAnd // numbers
  case BitwiseOr
  case BitwiseXor
  case ShiftLeft
  case ShiftRight

  case Error
}

enum UnaryOperatorKind {

  case Identity
  case Negation // -x => neg
  case LogicalNegation // !  => x == 0
  case BitwiseNegation // ~  => not

  case Error
}
