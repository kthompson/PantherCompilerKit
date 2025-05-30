import panther._

object SymbolChain {

  def fromOne(symbol: Symbol): Chain[Symbol] = {
    if (symbol.kind == SymbolKind.Block) {
      fromList(symbol.members())
    } else {
      fromList(symbol.members()).prepend(symbol)
    }
  }

  def fromList(symbols: List[Symbol]): Chain[Symbol] = {
    symbols match {
      case List.Nil => Chain.Empty()
      case List.Cons(head, tail) =>
        fromOne(head).concat(fromList(tail))
    }
  }
}
