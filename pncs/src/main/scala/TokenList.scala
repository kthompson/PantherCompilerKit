import panther._

case class TokenList() {
  var _items: Array[SyntaxToken] = new Array[SyntaxToken](0)
  var _size = 0

  def ensureCapacity(count: int): unit = {
    if (_size + count >= _items.length) {
      var newItems = new Array[SyntaxToken]((_size + count) * 2)
      for (i <- 0 to (_size - 1)) {
        newItems(i) = _items(i)
      }
      _items = newItems
    } else {
      ()
    }
  }

  def add(token: SyntaxToken): unit = {
    ensureCapacity(1)
    _items(_size) = token
    _size = _size + 1
  }

  def tokens(): Array[SyntaxToken] = {
    var newItems = new Array[SyntaxToken](_size)
    for (i <- 0 to (_size - 1)) {
      newItems(i) = _items(i)
    }
    newItems
  }
}
