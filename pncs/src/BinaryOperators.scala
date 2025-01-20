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
  simpleOp(binder.intType, BinaryOperatorKind.Plus)
  simpleOp(binder.intType, BinaryOperatorKind.Minus)
  simpleOp(binder.intType, BinaryOperatorKind.Multiply)
  simpleOp(binder.intType, BinaryOperatorKind.Divide)
  simpleOp(binder.intType, BinaryOperatorKind.Modulus)

  simpleOp(binder.boolType, BinaryOperatorKind.LogicalOr)
  simpleOp(binder.boolType, BinaryOperatorKind.LogicalAnd)

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

  def checkBinary(
      left: Type,
      right: Type,
      operator: BinaryOperatorKind
  ): Type = {
    _checkBinary(binaryOps, left, right, operator)
  }

  def _checkBinary(
      ops: List[BinaryOperator],
      left: Type,
      right: Type,
      operator: BinaryOperatorKind
  ): Type =
    ops match {
      case List.Nil => Type.Error
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
      case List.Nil => Type.Error
      case List.Cons(op, tail) =>
        if (op.operand == operand && op.operator == operator) {
          op.result
        } else {
          _checkUnary(tail, operand, operator)
        }
    }
}
