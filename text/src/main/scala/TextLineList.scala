import panther._

case class TextLineList() {
  var _size: int = 0
  var _items: Array[TextLine] = new Array[TextLine](0)

  def ensureCapacity(count: int): unit = {
    if (_size + count >= _items.length) {
      val newItems = new Array[TextLine]((_size + count) * 2)
      for (i <- 0 to (_size - 1)) {
        newItems(i) = _items(i)
      }
      _items = newItems
    } else {
      ()
    }
  }

  def add(line: TextLine): unit = {
    ensureCapacity(1)
    _items(_size) = line
    _size = _size + 1
  }

  def textLines(): Array[TextLine] = {
    var newItems = new Array[TextLine](_size)
    for (i <- 0 to (_size - 1)) {
      newItems(i) = _items(i)
    }
    newItems
  }
}
