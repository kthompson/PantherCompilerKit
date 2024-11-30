import panther._

case class BinaryOperators() {

  var size = 0
  var _capacity = 0
  var operators = new Array[BinaryOperator](0)

  def _ensureSpace(n: int): unit = {
    if (size + n > _capacity) {
      val newCapacity = if (_capacity == 0) 1 else _capacity * 2
      val newItems = new Array[BinaryOperator](newCapacity)
      for (i <- 0 to (size - 1)) {
        newItems(i) = operators(i)
      }
      operators = newItems
      _capacity = newCapacity
    } else {
      ()
    }
  }

  def addOperator(operator: BinaryOperator): unit = {
    _ensureSpace(1)
    operators(size) = operator
    size = size + 1
  }

}
