enum SymbolKind {
  case Namespace
  case Object
  case Class
  case Alias

  case TypeParameter(variance: Variance)

  case Block

  // Typed symbols
  case Field
  case Method
  case Constructor
  case Parameter
  case Local

  /** The receiver of an instance method. One per class or enum, defined on the
    * type's own symbol so that methods, blocks and field initialisers all
    * resolve to it through the ordinary scope walk.
    */
  case This
}
