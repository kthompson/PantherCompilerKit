import panther._

case class FieldTable() {
  var fields = new Array[FieldMetadata](0)
  var capacity = 0
  var size = 0

  def ensureSpace(n: int): unit = {
    if (size + n > capacity) {
      var newCapacity = if (capacity == 0) 16 else capacity * 2
      while (size + n > newCapacity) {
        newCapacity = newCapacity * 2
      }
      val newFields = new Array[FieldMetadata](newCapacity)
      for (i <- 0 to (size - 1)) {
        newFields(i) = fields(i)
      }

      fields = newFields
      capacity = newCapacity
    } else ()
  }

  def addField(name: StringToken, flags: int, fieldSig: int): int = {
    // ensure fields capacity
    ensureSpace(1)

    // add FieldMetadata to fields
    fields(size) = FieldMetadata(name, flags, fieldSig)
    size = size + 1
    size - 1
  }

  def write(buffer: IntList): unit = {
    buffer.add(size)
    for (i <- 0 to (size - 1)) {
      buffer.add(fields(i).name.token)
      buffer.add(fields(i).flags)
      buffer.add(fields(i).fieldSig)
    }
  }

  def read(buffer: IntList, offset: int): int = {
    val tableSize = buffer.read(offset)
    size = 0
    ensureSpace(tableSize)
    size = tableSize

    val recordSize = 3

    for (i <- 0 to (tableSize - 1)) {
      val recordOffset = offset + 1 + i * recordSize
      val name = StringToken(buffer.read(recordOffset + 0))
      val flags = buffer.read(recordOffset + 1)
      val fieldSig = buffer.read(recordOffset + 2)
      fields(i) = FieldMetadata(name, flags, fieldSig)
    }

    // return offset after reading the table
    offset + 1 + size * recordSize
  }
}