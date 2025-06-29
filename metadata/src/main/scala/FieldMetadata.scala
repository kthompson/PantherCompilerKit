import panther._

case class FieldMetadata(
    name: StringToken,
    flags: MetadataFlags,
    index: int,
    var fieldSig: int
)
