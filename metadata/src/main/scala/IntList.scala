import panther._

case class IntList() {
  val pageSize = 256
  var pages = new Array[Array[int]](0)
  var capacity = 0
  var size = 0

  def ensureSpace(n: int): unit = {
    if (size + n > capacity) {
      var newCapacity = if (capacity == 0) 16 else capacity * 2
      while (size + n > newCapacity) {
        newCapacity = newCapacity * 2
      }
      val newPageCount = pageCount(newCapacity)
      val newPages = new Array[Array[int]](newPageCount)
      for (i <- 0 to (pages.length - 1)) {
        newPages(i) = pages(i)
      }
      for (i <- pages.length to (newPageCount - 1)) {
        newPages(i) = new Array[int](pageSize)
      }

      pages = newPages
      capacity = newCapacity
    } else ()
  }

  def pageCount(requiredCapacity: int): int = {
    val fullPages = requiredCapacity / pageSize
    if (requiredCapacity % pageSize == 0) fullPages else fullPages + 1
  }

  def read(offset: int): int = pages(offset / pageSize)(offset % pageSize)

  def update(offset: int, value: int): unit = {
    pages(offset / pageSize)(offset % pageSize) = value
  }

  def add(value: int): unit = {
    ensureSpace(1)
    update(size, value)
    size = size + 1
  }

  def toArray(): Array[int] = {
    val result = new Array[int](size)
    for (i <- 0 to (size - 1)) {
      result(i) = read(i)
    }
    result
  }
}
