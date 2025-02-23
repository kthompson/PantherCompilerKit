import Helpers._
import panther._

object BinderTests {
  def run(): unit = {
    builtinSymbols()
    topLevelFields()
    methods()
    classes()
    enums()
  }

  def builtinSymbols(): unit = {
    val comp = mkCompilation("")
    val symbols = enumSymbols(comp)
    assertSymbol(symbols, SymbolKind.Class, "any")
    assertSymbol(symbols, SymbolKind.Class, "int")
    assertSymbol(symbols, SymbolKind.Class, "string")
    assertSymbol(symbols, SymbolKind.Field, "length")
    assertSymbol(symbols, SymbolKind.Class, "bool")
    assertSymbol(symbols, SymbolKind.Class, "char")
    assertSymbol(symbols, SymbolKind.Class, "unit")
    assertSymbol(symbols, SymbolKind.Class, "Array")
    assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
    assertSymbol(symbols, SymbolKind.Object, "predef")
    assertSymbol(symbols, SymbolKind.Method, "println")
    assertSymbol(symbols, SymbolKind.Parameter, "message")
    assertSymbol(symbols, SymbolKind.Method, "print")
    assertSymbol(symbols, SymbolKind.Parameter, "message")
    assertNoSymbol(symbols)
  }

  def topLevelFields(): unit = {
    val comp = mkCompilation("val x = 12")
    val symbols = enumNonBuiltinSymbols(comp)
    assertSymbol(symbols, SymbolKind.Field, "x")
    assertNoSymbol(symbols)
  }

  def methods(): unit = {
    val comp = mkCompilation("def foo() = 12")
    val symbols = enumNonBuiltinSymbols(comp)
    val foo = assertSymbol(symbols, SymbolKind.Method, "foo")
    assertNoSymbol(symbols)
  }

  def classes(): unit = {
    classWithoutArgs()
    classWithArgs()
  }

  def classWithoutArgs(): Unit = {
    val comp = mkCompilation("class Foo()")
    val symbols = enumNonBuiltinSymbols(comp)
    assertSymbol(symbols, SymbolKind.Class, "Foo")
    assertSymbol(symbols, SymbolKind.Constructor, ".ctor")

    assertNoSymbol(symbols)
  }

  def classWithArgs(): unit = {
    val comp = mkCompilation("class Foo(x: int, y: int)")
    val symbols = enumNonBuiltinSymbols(comp)
    assertSymbol(symbols, SymbolKind.Class, "Foo")
//    assertSymbol(symbols, SymbolKind.Field, "x")
//    assertSymbol(symbols, SymbolKind.Field, "y")
    assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
    assertSymbol(symbols, SymbolKind.Parameter, "x")
    assertSymbol(symbols, SymbolKind.Parameter, "y")

    assertNoSymbol(symbols)
  }

  def enums(): unit = {
    enumWithoutArgs()
    enumWithArgs()
  }

  def enumWithoutArgs(): Unit = {
    val comp = mkCompilation(
      "enum Foo {\n" +
        "  case Bar\n" +
        "  case Baz\n" +
        "}"
    )
    val symbols = enumNonBuiltinSymbols(comp)
    assertSymbol(symbols, SymbolKind.Enum, "Foo")
    assertSymbol(symbols, SymbolKind.Class, "Bar")
    assertSymbol(symbols, SymbolKind.Class, "Baz")
    assertNoSymbol(symbols)
  }

  def enumWithArgs(): Unit = {
    val comp = mkCompilation(
      "enum Foo {\n" +
        "  case Bar(x: int)\n" +
        "  case Baz(y: int)\n" +
        "}"
    )
    val symbols = enumNonBuiltinSymbols(comp)
    assertSymbol(symbols, SymbolKind.Enum, "Foo")
    assertSymbol(symbols, SymbolKind.Class, "Bar")
    assertSymbol(symbols, SymbolKind.Field, "x")
    assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
    assertSymbol(symbols, SymbolKind.Parameter, "x")

    assertSymbol(symbols, SymbolKind.Class, "Baz")
    assertSymbol(symbols, SymbolKind.Field, "y")
    assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
    assertSymbol(symbols, SymbolKind.Parameter, "y")
    assertNoSymbol(symbols)
  }
}
