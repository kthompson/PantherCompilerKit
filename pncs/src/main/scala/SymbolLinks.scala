import panther._

/** SymbolLinks is a class that represents the links between symbols.
  *
  * @param typ
  *   specifies the type of the symbol
  * @param base
  *   specifies the base class for the symbol. currently only used in Enums
  */
case class SymbolLinks(typ: Type, base: Option[Symbol])
