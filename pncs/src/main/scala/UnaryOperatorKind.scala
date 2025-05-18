enum UnaryOperatorKind {

  case Identity
  case Negation // -x => neg
  case LogicalNegation // !  => x == 0
  case BitwiseNegation // ~  => not

  case Error
}
