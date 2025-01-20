enum SymbolKind {
  case Namespace
  case Object
  case Class
  case Enum

  case TypeParameter(variance: Variance)

  // Typed symbols
  case Field
  case Method
  case Constructor
  case Parameter
  case Local
}
