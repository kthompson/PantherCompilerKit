import panther._

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
