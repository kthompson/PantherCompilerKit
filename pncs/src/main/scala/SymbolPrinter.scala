import panther._

case class SymbolPrinter(binder: Binder) {
  val printer = new AstPrinter(true, true)

  def printSymbols(symbols: List[Symbol], indent: string): unit = {
    symbols match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        _printSymbol(head, indent, tail.isEmpty)
        if (!tail.isEmpty) {
          printSymbols(tail, indent)
        }
    }
  }

  def printSymbol(symbol: Symbol): unit = _printSymbol(symbol, "", true)

  def _printSymbol(symbol: Symbol, indent: string, last: bool): unit = {
    val marker = if (last) "└──" else "├──"

    printer.writeColor(ColorPalette.Comment)
    print(indent)
    print(marker)
    print(ANSI.Clear)

    val kind = symbol.kind
    printer.printSymbolKind(kind)
    print(" ")

    printSimpleSymbol(symbol)
    printSymbolBodyType(symbol)
    println("")

    val members = symbol.members()
    if (!members.isEmpty) {
      val end = if (last) "    " else "│   "
      val childIndent = indent + end
      printSymbols(members, childIndent)
    }
  }

  private def printSymbolBodyType(symbol: Symbol): Unit = {
    if (
      symbol.kind == SymbolKind.Constructor || symbol.kind == SymbolKind.Method
    ) {
      printer.writeColor(ColorPalette.Punctuation)
      print(" = ")
      if (symbol.extern) {
        printer.writeColor(ColorPalette.Keyword)
        print("[extern]")
      } else if (binder.functionBodies.contains(symbol)) {
        printer.writeColor(ColorPalette.Keyword)
        print("[body]")
      } else {
        printer.writeColor(ColorPalette.Error)
        print("[no body]")
      }
      print(ANSI.Clear)
    }
  }

  def printSimpleSymbol(symbol: Symbol): unit = {
    if (
      symbol.kind == SymbolKind.Class || symbol.kind == SymbolKind.Object || symbol.kind == SymbolKind.Alias
    ) {
      val typ = binder.getSymbolType(symbol)
      typ match {
        case Option.Some(value) =>
          printer.printType(value)
        case Option.None =>
          print(symbol.qualifiedName())
      }
    } else {
      print(symbol.name)
    }
    printer.writeColor(ColorPalette.Punctuation)

    if (
      symbol.kind != SymbolKind.Namespace &&
      symbol.kind != SymbolKind.Block &&
      symbol.kind != SymbolKind.Alias &&
      symbol.kind != SymbolKind.Object
    ) {
      val typ = binder.getSymbolType(symbol)
      typ match {
        case Option.Some(value) =>
          symbol.kind match {
            case SymbolKind.Class            =>
            case SymbolKind.TypeParameter(_) =>
            case _ =>
              print(": ")
              printer.printType(value)
          }
        case Option.None =>
          print(": ")
          printer.writeColor(ColorPalette.Error)
          print("[missing type]")
          print(ANSI.Clear)
      }
    }
  }
}
