import panther._

case class TypeDefMetadata(
    name: StringToken,
    ns: StringToken,
    flags: MetadataFlags,
    fieldList: int,
    methodList: MethodToken
)
