import panther._

object MetadataFlags {
  val None = 0
  val Static = 1
}

case class Metadata() {
  var fields = FieldTable()
  var params = ParamTable()
  var methods = MethodTable()
  var typeDefs = TypeDefTable()
  var strings = StringTable()
  var signatures = BlobTable()

  var lastField = 0
  var lastParam = 0
  var lastMethod = 0

  def addField(name: string, flags: int, sigId: int): int = {
    val nameId = strings.addBlob(name)
    lastField = fields.addField(nameId, flags, sigId)
    lastField
  }

  def addSignature(signature: Signature): int =
    signatures.addBlob(signature.value, signature.value.length)

  def addParam(name: string, flags: int, sigId: int): int = {
    val nameId = strings.addBlob(name)
    lastParam = params.addParam(nameId, flags, sigId)
    lastParam
  }

  def addMethod(name: string, flags: int, sigId: int): int = {
    val nameId = strings.addBlob(name)
    lastMethod = methods.addMethod(nameId, flags, sigId, lastParam)
    lastMethod
  }

  def addTypeDef(name: string, namespace: string, flags: int): int = {
    val nameId = strings.addBlob(name)
    val namespaceId = strings.addBlob(namespace)
    typeDefs.addTypeDef(nameId, namespaceId, flags, lastField, lastMethod)
  }

  def write(buffer: IntList): unit = {
    typeDefs.write(buffer)
    fields.write(buffer)
    methods.write(buffer)
    params.write(buffer)
//    strings.write(buffer)
    signatures.write(buffer)
  }

  def read(buffer: IntList): int = {
    var offset = 0
    offset = typeDefs.read(buffer, offset)
    offset = fields.read(buffer, offset)
    offset = methods.read(buffer, offset)
    offset = params.read(buffer, offset)
    // offset = strings.read(buffer, offset)
    offset = signatures.read(buffer, offset)
    offset
  }

}

case class Signature(value: Array[int])

case class SignatureBuilder() {
  var signature = new IntList()

  def toSignature(): Signature = Signature(signature.toArray())

  def write(value: int): unit = signature.add(value)

//  def writeMethod(hasThis: bool, paramCount: int): unit = {
//    write(if (hasThis) 1 else 0)
//    write(paramCount)
//  }

  def writeUnit() = write(0)
  def writeBool(): unit = write(1)
//  def writeByte(): unit = write(2)
  def writeChar(): unit = write(3)
  def writeInt(): unit = write(4)
//  def writeFloat(): unit = write(5)
  def writeString(): unit = write(6)
  def writeFunction(): unit = write(7)

  def writeTypeArray(length: int) = {
    write(8)
    write(length)
  }

  def writeArray() = write(9)

  def writeOption() = write(10)

  def writeTypeConstructor() = write(11)
  def writeAny() = write(12)


}

case class FieldMetadata(name: int, flags: int, var fieldSig: int)
case class ParamMetadata(name: int, flags: int, var paramSig: int)
case class MethodMetadata(name: int, flags: int, var methodSig:int, paramList: int)
case class TypeDefMetadata(name: int, namespace: int, flags: int, fieldList: int, methodList: int)

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

  def addField(name: int, flags: int, fieldSig: int): int = {
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
      buffer.add(fields(i).name)
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
      val name = buffer.read(recordOffset + 0)
      val flags = buffer.read(recordOffset + 1)
      val fieldSig = buffer.read(recordOffset + 2)
      fields(i) = FieldMetadata(name, flags, fieldSig)
    }

    // return offset after reading the table
    offset + 1 + size * recordSize
  }
}

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

  def addMethod(name: int, flags: int, methodSig: int, paramList: int): int = {
    // ensure methods capacity
    ensureCapacity(size + 1)

    // add MethodMetadata to methods
    methods(size) = MethodMetadata(name, flags, methodSig, paramList)
    size = size + 1
    size - 1
  }

  def write(buffer: IntList): unit = {
    buffer.add(size)
    for (i <- 0 to (size - 1)) {
      buffer.add(methods(i).name)
      buffer.add(methods(i).flags)
      buffer.add(methods(i).methodSig)
      buffer.add(methods(i).paramList)
    }
  }

  def read(buffer: IntList, offset: int): int = {
    val tableSize = buffer.read(offset)
    size = 0
    ensureCapacity(tableSize)
    size = tableSize

    val recordSize = 4

    for (i <- 0 to (size - 1)) {
      val recordOffset = offset + 1 + i * recordSize
      val name = buffer.read(recordOffset + 0)
      val flags = buffer.read(recordOffset + 1)
      val methodSig = buffer.read(recordOffset + 2)
      val paramList = buffer.read(recordOffset + 3)
      methods(i) = MethodMetadata(name, flags, methodSig, paramList)
    }

    // return offset after reading the table
    offset + 1 + size * recordSize
  }
}

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

  def addTypeDef(name: int, namespace: int, flags: int, fieldList: int, methodList: int): int = {
    // ensure typeDefs capacity
    ensureCapacity(size + 1)

    // add TypeDefMetadata to typeDefs
    typeDefs(size) = TypeDefMetadata(name, namespace, flags, fieldList, methodList)
    size = size + 1
    size - 1
  }

  def write(buffer: IntList): unit = {
    buffer.add(size)
    for (i <- 0 to (size - 1)) {
      buffer.add(typeDefs(i).name)
      buffer.add(typeDefs(i).namespace)
      buffer.add(typeDefs(i).flags)
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
      val name = buffer.read(recordOffset + 0)
      val namespace = buffer.read(recordOffset + 1)
      val flags = buffer.read(recordOffset + 2)
      val fieldList = buffer.read(recordOffset + 3)
      val methodList = buffer.read(recordOffset + 4)
      typeDefs(i) = TypeDefMetadata(name, namespace, flags, fieldList, methodList)
    }

    // return offset after reading the table
    offset + 1 + size * recordSize
  }
}

case class StringTable() {
  var strings = new Array[string](0)
  var capacity = 0
  var size = 0

  def ensureSpace(n: int): unit = {
    if (size + n > capacity) {
      var newCapacity = if (capacity == 0) 16 else capacity * 2
      while (size + n > newCapacity) {
        newCapacity = newCapacity * 2
      }
      val newStrings = new Array[string](newCapacity)
      for (i <- 0 to (size - 1)) {
        newStrings(i) = strings(i)
      }

      strings = newStrings
      capacity = newCapacity
    } else ()
  }

  def addBlob(value: string): int = {
    // ensure strings capacity
    ensureSpace(1)

    // add string to strings
    strings(size) = value
    size = size + 1
    size - 1
  }
}

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

case class BlobTable() {
  var intList = new IntList()

  def addBlob(value: Array[int], blobSize: int): int = {
    intList.ensureSpace(blobSize + 1)

    val blobId = intList.size
    intList.add(blobSize)
    for (i <- 0 to (blobSize - 1)) {
      intList.add(value(i))
    }

    blobId
  }

  def write(buffer: IntList): unit = {
    val array = intList.toArray()
    buffer.add(array.length)
    for (i <- 0 to (array.length - 1)) {
      buffer.add(array(i))
    }
  }

  def read(buffer: IntList, offset: int): int = {
    val blobSize = buffer.read(offset)
    intList.size = 0
    intList.ensureSpace(blobSize)
    intList.size = blobSize

    for (i <- 0 to (blobSize - 1)) {
      intList.array(i) = buffer.read(offset + 1 + i)
    }

    offset + 1 + blobSize
  }
}