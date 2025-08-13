import panther._

case class SymbolPrinter(
    binder: Binder,
    ast: AstPrinter
) {

  def printSymbols(symbols: List[Symbol], indent: string): unit = {
    symbols match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        _printSymbolTree(head, indent, tail.isEmpty)
        if (!tail.isEmpty) {
          printSymbols(tail, indent)
        }
    }
  }

  def printSymbolTree(symbol: Symbol): unit = _printSymbolTree(symbol, "", true)

  def _printSymbolTree(symbol: Symbol, indent: string, last: bool): unit = {
    val marker = if (last) "└──" else "├──"

    ast.writeColor(ColorPalette.Comment)
    ast.append(indent)
    ast.append(marker)
    ast.append(ANSI.Clear)

    val kind = symbol.kind
    ast.printSymbolKind(kind)
    ast.append(" ")

    printSimpleSymbol(symbol)
    printSymbolBodyType(symbol)
    ast.appendLine("")

    val members = symbol.members()
    if (!members.isEmpty) {
      val end = if (last) "    " else "│   "
      val childIndent = indent + end
      printSymbols(members, childIndent)
    }
  }

  def printSymbolBodyType(symbol: Symbol): Unit = {
    if (
      symbol.kind == SymbolKind.Constructor || symbol.kind == SymbolKind.Method
    ) {
      ast.writeColor(ColorPalette.Punctuation)
      ast.append(" = ")
      if (symbol.extern) {
        ast.writeColor(ColorPalette.Keyword)
        ast.append("[extern]")
      } else if (binder.functionBodies.contains(symbol)) {
        ast.writeColor(ColorPalette.Keyword)
        ast.append("[body]")
      } else {
        ast.writeColor(ColorPalette.Error)
        ast.append("[no body]")
      }
      ast.append(ANSI.Clear)
    }
  }

  def printSimpleSymbol(symbol: Symbol): unit = {
    if (isTypeDef(symbol)) {
      val typ = binder.tryGetSymbolType(symbol)
      typ match {
        case Option.Some(value) =>
          ast.printType(value)
        case Option.None =>
          ast.printSymbolQualifiedName(symbol)
      }
    } else {
      ast.printSymbolQualifiedName(symbol)
    }
    ast.writeColor(ColorPalette.Punctuation)

    if (canHaveType(symbol)) {
      val typ = binder.tryGetSymbolType(symbol)
      typ match {
        case Option.Some(value) =>
          symbol.kind match {
            case SymbolKind.Class            =>
            case SymbolKind.TypeParameter(_) =>
            case _ =>
              ast.append(": ")
              ast.printType(value)
          }
        case Option.None =>
          ast.append(": ")
          ast.writeWithColor(ColorPalette.Error, "[missing type]")
      }
    }
  }

  def isTypeDef(symbol: Symbol): bool = {
    symbol.kind == SymbolKind.Class || symbol.kind == SymbolKind.Object || symbol.kind == SymbolKind.Alias
  }

  def canHaveType(symbol: Symbol): bool = {
    symbol.kind != SymbolKind.Namespace &&
    symbol.kind != SymbolKind.Block &&
    symbol.kind != SymbolKind.Alias &&
    symbol.kind != SymbolKind.Object
  }
}
