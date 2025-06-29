import panther._

case class TypeDefTable() {
  var typeDefs = new Array[TypeDefMetadata](0)
  var capacity = 0
  var size = 0

  def ensureCapacity(n: int): unit = {
    if (n > capacity) {
      var newCapacity = if (capacity == 0) 16 else capacity * 2
      while (n > newCapacity) {
        newCapacity = newCapacity * 2
      }
      val newTypeDefs = new Array[TypeDefMetadata](newCapacity)
      for (i <- 0 to (size - 1)) {
        newTypeDefs(i) = typeDefs(i)
      }

      typeDefs = newTypeDefs
      capacity = newCapacity
    } else ()
  }

  def get(token: TypeDefToken): TypeDefMetadata = typeDefs(token.token)

  def addTypeDef(
      name: StringToken,
      ns: StringToken,
      flags: MetadataFlags,
      fieldList: int,
      methodList: int
  ): int = {
    // ensure typeDefs capacity
    ensureCapacity(size + 1)

    // add TypeDefMetadata to typeDefs
    typeDefs(size) = TypeDefMetadata(name, ns, flags, fieldList, methodList)
    size = size + 1
    size - 1
  }

  def write(buffer: IntList): unit = {
    buffer.add(size)
    for (i <- 0 to (size - 1)) {
      buffer.add(typeDefs(i).name.token)
      buffer.add(typeDefs(i).ns.token)
      buffer.add(MetadataFlagsHelpers.toInt(typeDefs(i).flags))
      buffer.add(typeDefs(i).fieldList)
      buffer.add(typeDefs(i).methodList)
    }
  }

  def read(buffer: IntList, offset: int): int = {
    val tableSize = buffer.read(offset)
    size = 0
    ensureCapacity(tableSize)
    size = tableSize

    val recordSize = 5

    for (i <- 0 to (size - 1)) {
      val recordOffset = offset + 1 + i * recordSize
      val name = StringToken(buffer.read(recordOffset + 0))
      val ns = StringToken(buffer.read(recordOffset + 1))
      val flags = buffer.read(recordOffset + 2)
      val fieldList = buffer.read(recordOffset + 3)
      val methodList = buffer.read(recordOffset + 4)
      typeDefs(i) = TypeDefMetadata(
        name,
        ns,
        MetadataFlagsHelpers.fromInt(flags),
        fieldList,
        methodList
      )
    }

    // return offset after reading the table
    offset + 1 + size * recordSize
  }
}
