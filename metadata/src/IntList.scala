import panther._

case class IntList() {
  var array = new Array[int](0)
  var capacity = 0
  var size = 0

  def ensureSpace(n: int): unit = {
    if (size + n > capacity) {
      var newCapacity = if (capacity == 0) 16 else capacity * 2
      while (size + n > newCapacity) {
        newCapacity = newCapacity * 2
      }
      val newBlobs = new Array[int](newCapacity)
      for (i <- 0 to (size - 1)) {
        newBlobs(i) = array(i)
      }

      array = newBlobs
      capacity = newCapacity
    } else ()
  }

  def read(offset: int): int = array(offset)

  def add(value: int): unit = {
    ensureSpace(1)
    array(size) = value
    size = size + 1
  }

  def toArray(): Array[int] = {
    val result = new Array[int](size)
    for (i <- 0 to (size - 1)) {
      result(i) = array(i)
    }
    result
  }
}