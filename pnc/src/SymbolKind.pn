enum SymbolKind {
  case Namespace
  case Object
  case Class

  /** A declared capability. Distinct from `Class` because a trait is never
    * instantiated and is the only thing a context bound or a `given` may name.
    */
  case Trait

  /** Evidence that a type satisfies a trait. Distinct from `Class` so that
    * metadata emission and `getMethodParameterMap` can tell evidence apart from
    * an ordinary type — the lesson from `This` below.
    */
  case Given
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
