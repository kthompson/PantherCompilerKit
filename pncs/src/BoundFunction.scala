import MemberSyntax.FunctionDeclarationSyntax

case class BoundFunction(symbol: Symbol, function: FunctionDeclarationSyntax, parameters: Array[BoundParameter])