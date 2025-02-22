enum Conversion {
  case Implicit
  case Explicit
  case Identity
  case None

  val isImplicit: Boolean = this == Implicit
  val isExplicit: Boolean = this == Explicit
  val isIdentity: Boolean = this == Identity
  val Exists: Boolean = this != None
}

class ConversionClassifier(binder: Binder) {
  def classify(from: Type, toType: Type): Conversion = {
    if (from == toType) {
      Conversion.Identity
    } else if (toType == binder.anyType || toType == binder.unitType) {
      Conversion.Implicit
    } else if (from == binder.neverType) {
      Conversion.Implicit
    } else if (from == binder.anyType) {
      Conversion.Explicit
    } else if (toType == binder.stringType) {
      Conversion.Explicit
    } else {
      Conversion.None
    }
  }
}
