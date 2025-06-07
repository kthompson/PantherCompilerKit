import panther._

case class FieldMetadata(
    name: StringToken,
    flags: MetadataFlags,
    var fieldSig: int
)
