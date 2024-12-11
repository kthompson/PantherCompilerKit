import AstPrinter._
import panther._

object ScopeParentKind {
  val Symbol = 0
  val Scope = 1
}

enum ScopeParent {
  case OfSymbol(value: Symbol)
  case OfScope(value: Scope)
  
  def getScope(): Scope = this match {
    case ScopeParent.OfSymbol(value) => value.members
    case ScopeParent.OfScope(value) => value
  }

  def get(name: string): Option[Symbol] = this match {
    case ScopeParent.OfScope(value) => value.get(name)
    case ScopeParent.OfSymbol(value) =>
      // if we are a Symbol, we already searched this symbols scope so go to our parent
      if (value.hasParent()) {
        val parent = value.parent()
        parent.members.get(name)
      } else {
        None
      }
  }
}

object SymbolKind {
  val Root = 0 // maps to the SymbolTable
  val Class = 1 // maps to a specific TypeSymbol
  val Method = 2 // maps to a specific MethodSymbol (includes parameters)
  val Function = 3
  val Constructor = 4

  val Field = 5
  val Parameter = 6
  val Local = 7
}

//object ScopeKind {
//    val Root = 0 // maps to the SymbolTable
//    val Type = 1 // maps to a specific TypeSymbol
//    val Method = 2 // maps to a specific MethodSymbol (includes parameters)
//    val Block = 3 // maps to the body of a MethodSymbol
//}

case class Scope(parent: ScopeParent, note: string) {

  var _symbolCount = 0
  var _symbols: Array[Symbol] = new Array[Symbol](0)

  var _scopeCount = 0
  var _scopes: Array[Scope] = new Array[Scope](0)

  def empty(): bool = _symbolCount == 0 && _scopeCount == 0

  def openScope(note: string): Scope = {
    val scope = new Scope(new ScopeParent.OfScope(this), note)

    if (_scopeCount + 1 >= _scopes.length) {
      val newItems = new Array[Scope]((_scopeCount + 1) * 2)
      for (i <- 0 to (_scopeCount - 1)) {
        newItems(i) = _scopes(i)
      }
      _scopes = newItems
    } else {
      ()
    }
    _scopes(_scopeCount) = scope
    _scopeCount = _scopeCount + 1
    scope
  }

  def scopes(): Array[Scope] = {
    val newItems = new Array[Scope](_scopeCount)
    for (i <- 0 to (_scopeCount - 1)) {
      newItems(i) = _scopes(i)
    }
    newItems
  }

  def getScopeByName(name: string): Option[Scope] =
    getScopeByNameAndIndex(name, 0)

  def getScopeByNameAndIndex(name: string, index: int): Option[Scope] = {
    if (index >= _scopeCount) {
      None
    } else if (_scopes(index).note == name) {
      Some(_scopes(index))
    } else {
      getScopeByNameAndIndex(name, index + 1)
    }
  }

  def addSymbol(symbol: Symbol): Symbol = {
    if (_symbolCount + 1 >= _symbols.length) {
      val newItems = new Array[Symbol]((_symbolCount + 1) * 2)
      for (i <- 0 to (_symbolCount - 1)) {
        newItems(i) = _symbols(i)
      }
      _symbols = newItems
    } else {
      ()
    }
    _symbols(_symbolCount) = symbol
    _symbolCount = _symbolCount + 1
    symbol
  }

  def symbols(): Array[Symbol] = {
    val newItems = new Array[Symbol](_symbolCount)
    for (i <- 0 to (_symbolCount - 1)) {
      newItems(i) = _symbols(i)
    }
    newItems
  }

  def getParentSymbol(): Symbol =
    parent match {
      case ScopeParent.OfSymbol(value) => value
      case ScopeParent.OfScope(value) => value.getParentSymbol()
    }

  // returns true if we are not in a type or method
  def isGlobalScope(): bool = getParentSymbol().kind == SymbolKind.Root

  def getFromLocals(name: string, index: int): Option[Symbol] = {
    if (index >= _symbolCount) {
      None
    } else if (_symbols(index).name == name) {
      Some(_symbols(index))
    } else {
      getFromLocals(name, index + 1)
    }
  }

  def get(name: string): Option[Symbol] = {
    val result = getFromLocals(name, 0)
    result match {
      case Some(value) => Some(value)
      case None => parent.get(name)
    }
  }

  def name(): string = if (isGlobalScope()) "" else getParentSymbol().fullName()
}

object SymbolFlags {
  val None = 0
  val Static = 1
}

/**
 * Symbols can have 0 or 1 Scope.
 * Block scopes can have multiple levels of inner scopes
 * Scopes hold symbols
 * Scopes can have child scopes
 * Scopes have a parent. Either another Scope or a Symbol
 * For block scopes, another block scope or method will be the parent
 */
case class Symbol(kind: int, flags: int, name: string, location: TextLocation, _parent: Option[Symbol]) {
  var _declarations: Array[Declaration] = new Array[Declaration](0)
  var _declarationCount = 0
  var id = -1 // used in type checking
  val ns = if (_parent.isDefined) _parent.get.fullName() else ""
  val members: Scope = new Scope(ScopeParent.OfSymbol(this), name)

  def parent(): Symbol = _parent.get

  def hasParent(): bool = _parent.isDefined

  def fullName(): string = {
    val parentName = ns
    if (parentName == "") name else parentName + "." + name
  }

  def hasDeclaration(): bool = _declarationCount > 0

  def addDeclaration(declaration: Declaration): Declaration = {
    if (_declarationCount + 1 >= _declarations.length) {
      val newItems = new Array[Declaration]((_declarationCount + 1) * 2)
      for (i <- 0 to (_declarationCount - 1)) {
        newItems(i) = _declarations(i)
      }
      _declarations = newItems
    } else {
      ()
    }
    _declarations(_declarationCount) = declaration
    _declarationCount = _declarationCount + 1
    declaration
  }

  def declaration(): Declaration = _declarations(0)

  def declarations(): Array[Declaration] = {
    var newItems = new Array[Declaration](_declarationCount)
    for (i <- 0 to (_declarationCount - 1)) {
      newItems(i) = _declarations(i)
    }
    newItems
  }
}

case class SymbolTreePrinter(checker: Checker) {
  def printScope(scope: Scope, indent: string, last: bool): unit = {
    val symbols = scope.symbols()
    val scopes = scope.scopes()
    val numSymbols = symbols.length
    val numScopes = scopes.length

    val marker = if (last) "└──" else "├──"
    var childIndent = indent
    scope.parent match {
      case ScopeParent.OfSymbol(value) =>
        printColor(ColorPalette.Comments)
        print(indent)
        print(marker)
        print(ANSI.Clear)
        if (scope.note == "") {
          println("Scope")
        } else {
          println("Scope: " + scope.note)
        }

        val end = if (last) "    " else "│   "
        childIndent = childIndent + end
      case ScopeParent.OfScope(value) => ()
    }


    if (numSymbols > 0) {
      for (i <- 0 to (numSymbols - 2)) {
        printSymbol(symbols(i), childIndent, false)
      }
      printSymbol(symbols(numSymbols - 1), childIndent, numScopes == 0)
    } else {}

    if (numScopes > 0) {
      for (s <- 0 to (numScopes - 2)) {
        printScope(scopes(s), childIndent, false)
      }
      printScope(scopes(numScopes - 1), childIndent, true)
    } else {}
  }

  def printSymbol(symbol: Symbol): unit = printSymbol(symbol, "", true)

  def printSymbol(symbol: Symbol, indent: string, last: bool): unit = {
    val marker = if (last) "└──" else "├──"

    printColor(ColorPalette.Comments)
    print(indent)
    print(marker)
    print(ANSI.Clear)

    val kind = symbol.kind
    AstPrinter.printSymbolKind(kind)
    print(" ")

    print(symbol.name)
    printColor(ColorPalette.Punctuation)

    print(": ")
    val typ = checker.getSymbolType(symbol)
    typ match {
      case Some(value) =>
        printType(value)
      case None =>
        printColor(ColorPalette.Error)
        print("[missing type]")
        print(ANSI.Clear)

    }

    if (symbol.hasDeclaration()) {
    } else {
      printColor(ColorPalette.Error)
      print(" [missing declaration]")
      print(ANSI.Clear)
    }

    println()

    if (!symbol.members.empty()) {
      val end = if (last) "    " else "│   "
      val childIndent = indent + end
      printScope(symbol.members, childIndent, true)
    } else {}
  }

}