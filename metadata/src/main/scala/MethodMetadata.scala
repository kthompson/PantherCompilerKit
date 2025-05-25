import panther._

case class MethodMetadata(
    name: StringToken,
    flags: MetadataFlags,
    var methodSig: int,
    paramList: int,
    locals: int,
    var address: int
)
