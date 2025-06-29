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

  def addString(value: string): StringToken =
    strings.addBlob(value)

  def getString(token: StringToken): string =
    strings.get(token)

  def addField(
      name: string,
      flags: MetadataFlags,
      index: int,
      sigId: int
  ): FieldToken = {
    val nameId = strings.addBlob(name)
    FieldToken(fields.addField(nameId, flags, index, sigId))
  }

  def addSignature(signature: Signature): int =
    signatures.addBlob(signature.value, signature.value.length)

  def addParam(name: string, flags: MetadataFlags, sigId: int): ParamToken = {
    val nameId = strings.addBlob(name)
    ParamToken(params.addParam(nameId, flags, sigId))
  }

  def addMethod(
      name: string,
      flags: MetadataFlags,
      sigId: int,
      locals: int,
      address: int
  ): MethodToken = {
    val nameId = strings.addBlob(name)
    methods.addMethod(nameId, flags, sigId, params.size, locals, address)
  }

  def addTypeDef(
      name: string,
      ns: string,
      flags: MetadataFlags
  ): TypeDefToken = {
    val nameId = strings.addBlob(name)
    val namespaceId = strings.addBlob(ns)
    val typeDef =
      typeDefs.addTypeDef(nameId, namespaceId, flags, fields.size, methods.size)
    TypeDefToken(typeDef)
  }

  def findTypeDefForMethod(method: MethodToken): TypeDefToken = {
    val token = _findTypeDefForMethod(
      method.token,
      0,
      typeDefs.size - 1,
      typeDefs.typeDefs(0).methodList,
      typeDefs.typeDefs(typeDefs.size - 1).methodList
    )
    TypeDefToken(token)
  }

  def findTypeDefForField(field: FieldToken): TypeDefToken = {
    val token = _findTypeDefForField(
      field.token,
      0,
      typeDefs.size - 1,
      typeDefs.typeDefs(0).fieldList,
      typeDefs.typeDefs(typeDefs.size - 1).fieldList
    )
    TypeDefToken(token)
  }

  def getTypeDefSize(token: TypeDefToken): int = {
    val typeId = token.token
    val fieldList = typeDefs.get(token).fieldList
    val nextFieldList = if (typeId + 1 >= typeDefs.size) {
      // field count is lastField - fieldList
      fields.size - 1
    } else {
      val nextTypeDef = typeDefs.typeDefs(typeId + 1)
      nextTypeDef.fieldList
    }

    nextFieldList - fieldList
  }

  def _findTypeDefForField(
      field: int,
      minTypeDef: int,
      maxTypeDef: int,
      minField: int,
      maxField: int
  ): int = {

    if (maxTypeDef == minTypeDef) {
      minTypeDef
    } else if (maxTypeDef == minTypeDef + 1) {
      if (field >= maxField) {
        maxTypeDef
      } else {
        minTypeDef
      }
    } else {
      // method should be a number between the methodList of two typeDefs
      // do a binary search to find the typeDef that contains the method
      val midTypeDef = (minTypeDef + maxTypeDef) / 2
      val midField = typeDefs.typeDefs(midTypeDef).fieldList
      if (field < midField) {
        _findTypeDefForField(
          field,
          minTypeDef,
          midTypeDef,
          minField,
          midField
        )
      } else {
        _findTypeDefForField(
          field,
          midTypeDef,
          maxTypeDef,
          midField,
          maxField
        )
      }
    }
  }

  def _findTypeDefForMethod(
      method: int,
      minTypeDef: int,
      maxTypeDef: int,
      minMethod: int,
      maxMethod: int
  ): int = {

    if (maxTypeDef == minTypeDef) {
      minTypeDef
    } else if (maxTypeDef == minTypeDef + 1) {
      if (method >= maxMethod) {
        maxTypeDef
      } else {
        minTypeDef
      }
    } else {
      // method should be a number between the methodList of two typeDefs
      // do a binary search to find the typeDef that contains the method
      val midTypeDef = (minTypeDef + maxTypeDef) / 2
      val midMethod = typeDefs.typeDefs(midTypeDef).methodList
      if (method < midMethod) {
        _findTypeDefForMethod(
          method,
          minTypeDef,
          midTypeDef,
          minMethod,
          midMethod
        )
      } else {
        _findTypeDefForMethod(
          method,
          midTypeDef,
          maxTypeDef,
          midMethod,
          maxMethod
        )
      }
    }
  }

  def getTypeName(token: TypeDefToken): string = {
    val typeDef = typeDefs.get(token)
    val name = strings.get(typeDef.name)
    val ns = strings.get(typeDef.ns)

    if (ns == "") {
      name
    } else {
      ns + "." + name
    }
  }

  def getFieldName(field: FieldToken): string = {
    val m = fields.get(field)
    val name = strings.get(m.name)
    val typeDef = findTypeDefForField(field)
    val typeDefName = getTypeName(typeDef)

    typeDefName + "." + name
  }

  def getMethodName(method: MethodToken): string = {
    val m = methods.get(method)
    val name = strings.get(m.name)
    val typeDef = findTypeDefForMethod(method)
    val typeDefName = getTypeName(typeDef)

    typeDefName + "." + name
  }

  def getMethodAddress(method: MethodToken): int =
    methods.get(method).address

  def getMethodLocals(method: MethodToken): int =
    methods.get(method).locals

  def getMethodParameterCount(method: MethodToken): int = {
    val methodId = method.token
    val paramList = methods.get(method).paramList
    val nextParamList = if (methodId + 1 >= methods.size) {
      params.size
    } else {
      val nextMethod = methods.methods(methodId + 1)
      nextMethod.paramList
    }

    nextParamList - paramList
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
