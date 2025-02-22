enum SymbolKind {
  case Namespace
  case Object
  case Class
  case Enum

  case TypeParameter(variance: Variance)

  case Block

  // Typed symbols
  case Field
  case Method
  case Constructor
  case Parameter
  case Local
}
