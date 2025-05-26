import panther._

case class FieldToken(token: int)

case class ParamToken(token: int)

case class MethodToken(token: int)

case class TypeDefToken(token: int)

case class StringToken(token: int)

case class SignatureToken(token: int)

case class Metadata() {
  var fields = FieldTable()
  var params = ParamTable()
  var methods = MethodTable()
  var typeDefs = TypeDefTable()
  var strings = StringTable()
  var signatures = BlobTable()

  var lastField = 0
  var lastParam = 0
  var lastMethod = MethodToken(0)

  def addString(value: string): StringToken =
    strings.addBlob(value)

  def getString(token: StringToken): string =
    strings.get(token)

  def addField(name: string, flags: MetadataFlags, sigId: int): FieldToken = {
    val nameId = strings.addBlob(name)
    lastField = fields.addField(nameId, flags, sigId)
    FieldToken(lastField)
  }

  def addSignature(signature: Signature): int =
    signatures.addBlob(signature.value, signature.value.length)

  def addParam(name: string, flags: MetadataFlags, sigId: int): ParamToken = {
    val nameId = strings.addBlob(name)
    lastParam = params.addParam(nameId, flags, sigId)
    ParamToken(lastParam)
  }

  def addMethod(
      name: string,
      flags: MetadataFlags,
      sigId: int,
      locals: int,
      address: int
  ): MethodToken = {
    val nameId = strings.addBlob(name)
    lastMethod =
      methods.addMethod(nameId, flags, sigId, lastParam, locals, address)
    lastMethod
  }

  def addTypeDef(
      name: string,
      ns: string,
      flags: MetadataFlags
  ): TypeDefToken = {
    val nameId = strings.addBlob(name)
    val namespaceId = strings.addBlob(ns)
    val typeDef =
      typeDefs.addTypeDef(nameId, namespaceId, flags, lastField, lastMethod)
    TypeDefToken(typeDef)
  }

  def findTypeDefForMethod(method: MethodToken): TypeDefToken = {
    val token = _findTypeDefForMethod(method.token, 0, typeDefs.size - 1)
    TypeDefToken(token)
  }

  def _findTypeDefForMethod(
      method: int,
      minTypeDef: int,
      maxTypeDef: int
  ): int = {
    // method should be a number between the methodList of two typeDefs
    // do a binary search to find the typeDef that contains the method
    val minMethod = typeDefs.typeDefs(minTypeDef).methodList.token
    val maxMethod = typeDefs.typeDefs(maxTypeDef).methodList.token

    if (minTypeDef == maxTypeDef) {
      minTypeDef
    } else if (method >= maxMethod) {
      maxTypeDef
    } else {
      val midTypeDef = (minTypeDef + maxTypeDef) / 2
      val midMethod = typeDefs.typeDefs(midTypeDef).methodList.token

      if (method < midMethod) {
        _findTypeDefForMethod(method, minTypeDef, midTypeDef)
      } else if (method > midMethod) {
        _findTypeDefForMethod(method, midTypeDef, maxTypeDef)
      } else {
        midTypeDef
      }
    }
  }

  def getMethodName(method: MethodToken): string = {
    val m = methods.get(method)
    val name = strings.get(m.name)
    val typeDef = findTypeDefForMethod(method)
    val typeDefName = strings.get(typeDefs.get(typeDef).name)

    typeDefName + "." + name
  }

  def getMethodAddress(method: MethodToken): int =
    methods.get(method).address

  def getMethodLocals(method: MethodToken): int =
    methods.get(method).locals

  def getMethodParameterCount(method: MethodToken): int = {
    val methodId = method.token
    val paramList = methods.get(method).paramList
    if (methodId + 1 >= methods.size) {
      params.size - 1 - paramList
    } else {
      val nextMethod = methods.methods(methodId + 1)
      nextMethod.paramList - paramList
    }
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
