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
  case LogicalAnd
  case LogicalOr
  
  case Error
}

enum UnaryOperatorKind {
  case LogicalNot
  case Negate
  
  case Error
}
