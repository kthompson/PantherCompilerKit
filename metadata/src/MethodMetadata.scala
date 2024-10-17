import panther._

case class MethodMetadata(name: StringToken, flags: int, var methodSig: int, paramList: int, locals: int, address: int)
