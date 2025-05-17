import Helpers._
import SymbolKind.Parameter
import panther._
import TestFramework._

object BinderTests {
  def run(): unit = {
    suite("Binder Tests")
    builtinSymbols()
    topLevelFields()
    methods()
    classes()
    enums()
  }

  def builtinSymbols(): unit = {
    test("builtin symbols")
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
    assertSymbol(symbols, SymbolKind.Parameter, "size")
//    assertSymbol(symbols, SymbolKind.Object, "predef")
    assertSymbol(symbols, SymbolKind.Method, "println")
    assertSymbol(symbols, SymbolKind.Parameter, "message")
    assertSymbol(symbols, SymbolKind.Method, "print")
    assertSymbol(symbols, SymbolKind.Parameter, "message")
    assertSymbol(symbols, SymbolKind.Object, "$Program")
    assertSymbol(symbols, SymbolKind.Method, "main")
    assertNoSymbols(symbols)
  }

  def topLevelFields(): unit = {
    test("top level fields")
    val comp = mkCompilation("val x = 12")
    val symbols = enumNonBuiltinSymbols(comp)
    assertProgramSymbol(symbols)
    assertSymbol(symbols, SymbolKind.Field, "x")
    assertSymbol(symbols, SymbolKind.Method, "main")
    assertNoSymbols(symbols)
  }

  def methods(): unit = {
    test("methods")
    val comp = mkCompilation("def foo() = 12")
    val symbols = enumNonBuiltinSymbols(comp)

    assertProgramSymbol(symbols)
    assertSymbol(symbols, SymbolKind.Method, "foo")
    assertMainSymbol(symbols)
    assertNoSymbols(symbols)
  }

  def classes(): unit = {
    classWithoutArgs()
    classWithArgs()
  }

  def classWithoutArgs(): Unit = {
    test("class without args")
    val comp = mkCompilation("class Foo()")
    val symbols = enumNonBuiltinSymbols(comp)
    assertSymbol(symbols, SymbolKind.Class, "Foo")
    assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
    assertProgramSymbol(symbols)
    assertMainSymbol(symbols)
    assertNoSymbols(symbols)
  }

  def classWithArgs(): unit = {
    test("class with args")
    val comp = mkCompilation("class Foo(x: int, y: int)")
    val symbols = enumNonBuiltinSymbols(comp)
    assertSymbol(symbols, SymbolKind.Class, "Foo")
//    assertSymbol(symbols, SymbolKind.Field, "x")
//    assertSymbol(symbols, SymbolKind.Field, "y")
    assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
    assertSymbol(symbols, SymbolKind.Parameter, "x")
    assertSymbol(symbols, SymbolKind.Parameter, "y")

    assertProgramSymbol(symbols)
    assertMainSymbol(symbols)

    assertNoSymbols(symbols)
  }

  def enums(): unit = {
    enumWithoutArgs()
    enumWithArgs()
  }

  def enumWithoutArgs(): Unit = {
    test("enum without args")
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

    assertProgramSymbol(symbols)
    assertMainSymbol(symbols)

    assertNoSymbols(symbols)
  }

  def enumWithArgs(): Unit = {
    test("enum with args")
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

    assertProgramSymbol(symbols)
    assertMainSymbol(symbols)

    assertNoSymbols(symbols)
  }
}
