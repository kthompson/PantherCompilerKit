import panther._

case class ParamTable() {
  var params = new Array[ParamMetadata](0)
  var capacity = 0
  var size = 0

  def ensureCapacity(n: int): unit = {
    if (n > capacity) {
      var newCapacity = if (capacity == 0) 16 else capacity * 2
      while (n > newCapacity) {
        newCapacity = newCapacity * 2
      }
      val newParams = new Array[ParamMetadata](newCapacity)
      for (i <- 0 to (size - 1)) {
        newParams(i) = params(i)
      }

      params = newParams
      capacity = newCapacity
    } else ()
  }

  def addParam(name: int, flags: int, paramSig: int): int = {
    // ensure params capacity
    ensureCapacity(size + 1)

    // add ParamMetadata to params
    params(size) = ParamMetadata(name, flags, paramSig)
    size = size + 1
    size - 1
  }

  def write(buffer: IntList): unit = {
    buffer.add(size)
    for (i <- 0 to (size - 1)) {
      buffer.add(params(i).name)
      buffer.add(params(i).flags)
      buffer.add(params(i).paramSig)
    }
  }

  def read(buffer: IntList, offset: int): int = {
    val tableSize = buffer.read(offset)
    size = 0
    ensureCapacity(tableSize)
    size = tableSize

    val recordSize = 3

    for (i <- 0 to (tableSize - 1)) {
      val recordOffset = offset + 1 + i * recordSize
      val name = buffer.read(recordOffset + 0)
      val flags = buffer.read(recordOffset + 1)
      val paramSig = buffer.read(recordOffset + 2)
      params(i) = ParamMetadata(name, flags, paramSig)
    }

    // return offset after reading the table
    offset + 1 + size * recordSize
  }
}
