import panther._

case class MethodTable() {
  var methods = new Array[MethodMetadata](0)
  var capacity = 0
  var size = 0

  def ensureCapacity(n: int): unit = {
    if (n > capacity) {
      var newCapacity = if (capacity == 0) 16 else capacity * 2
      while (n > newCapacity) {
        newCapacity = newCapacity * 2
      }
      val newMethods = new Array[MethodMetadata](newCapacity)
      for (i <- 0 to (size - 1)) {
        newMethods(i) = methods(i)
      }

      methods = newMethods
      capacity = newCapacity
    } else ()
  }

  def get(method: MethodToken): MethodMetadata = methods(method.token)

  def addMethod(
      name: StringToken,
      flags: MetadataFlags,
      methodSig: int,
      paramList: int,
      locals: int,
      address: int,
      hasThis: bool
  ): MethodToken = {
    // ensure methods capacity
    ensureCapacity(size + 1)

    // add MethodMetadata to methods
    methods(size) = MethodMetadata(
      name,
      flags,
      methodSig,
      paramList,
      locals,
      address,
      hasThis
    )
    size = size + 1
    MethodToken(size - 1)
  }

  def write(buffer: IntList): unit = {
    buffer.add(size)
    for (i <- 0 to (size - 1)) {
      buffer.add(methods(i).name.token)
      buffer.add(MetadataFlagsHelpers.toInt(methods(i).flags))
      buffer.add(methods(i).methodSig)
      buffer.add(methods(i).paramList)
      buffer.add(methods(i).locals)
      buffer.add(methods(i).address)
      buffer.add(if (methods(i).hasThis) 1 else 0)
    }
  }

  def read(buffer: IntList, offset: int): int = {
    val tableSize = buffer.read(offset)
    size = 0
    ensureCapacity(tableSize)
    size = tableSize

    val recordSize = 7 // fields in MethodMetadata

    for (i <- 0 to (size - 1)) {
      val recordOffset = offset + 1 + i * recordSize
      val name = StringToken(buffer.read(recordOffset + 0))
      val flags = buffer.read(recordOffset + 1)
      val methodSig = buffer.read(recordOffset + 2)
      val paramList = buffer.read(recordOffset + 3)
      val locals = buffer.read(recordOffset + 4)
      val address = buffer.read(recordOffset + 5)
      val hasThis = buffer.read(recordOffset + 6) == 1

      methods(i) = MethodMetadata(
        name,
        MetadataFlagsHelpers.fromInt(flags),
        methodSig,
        paramList,
        locals,
        address,
        hasThis
      )
    }

    // return offset after reading the table
    offset + 1 + size * recordSize
  }
}
