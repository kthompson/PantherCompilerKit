import TestHelpers._
import utest._

object BinderTests extends TestSuite {
  val tests = Tests {
    test("builtin symbols") {
      val comp = mkCompilation("")
      val symbols = enumSymbols(comp)
      assertSymbol(symbols, SymbolKind.Class, "any")
      assertSymbol(symbols, SymbolKind.Class, "int")
      assertConversionMethod(symbols)
      assertSymbol(symbols, SymbolKind.Class, "string")
      assertSymbol(symbols, SymbolKind.Field, "length")
      assertConversionMethod(symbols)
      assertSymbol(symbols, SymbolKind.Class, "bool")
      assertConversionMethod(symbols)
      assertSymbol(symbols, SymbolKind.Class, "char")
      assertConversionMethod(symbols)
      assertSymbol(symbols, SymbolKind.Class, "unit")
      assertSymbol(symbols, SymbolKind.Class, "Array")
      assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Invariant), "T")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertSymbol(symbols, SymbolKind.Parameter, "size")
      assertSymbol(symbols, SymbolKind.Field, "length")
      assertSymbol(symbols, SymbolKind.Method, "apply")
      assertSymbol(symbols, SymbolKind.Parameter, "index")
      //    assertSymbol(symbols, SymbolKind.Object, "predef")
      assertSymbol(symbols, SymbolKind.Method, "println")
      assertSymbol(symbols, SymbolKind.Parameter, "message")
      assertSymbol(symbols, SymbolKind.Method, "print")
      assertSymbol(symbols, SymbolKind.Parameter, "message")
      assertSymbol(symbols, SymbolKind.Object, "$Program")
      assertSymbol(symbols, SymbolKind.Method, "$runtimeInit")
      assertSymbol(symbols, SymbolKind.Method, "main")
      assertNoSymbols(symbols)
    }

    test("top level fields") {
      val comp = mkCompilation("val x = 12")
      val symbols = enumNonBuiltinSymbols(comp)
      assertProgramSymbol(symbols)
      assertSymbol(symbols, SymbolKind.Field, "x")
      assertSymbol(symbols, SymbolKind.Method, "main")
      assertNoSymbols(symbols)
    }

    test("methods") {
      val comp = mkCompilation("def foo() = 12")
      val symbols = enumNonBuiltinSymbols(comp)

      assertProgramSymbol(symbols)
      assertSymbol(symbols, SymbolKind.Method, "foo")
      assertMainSymbol(symbols)
      assertNoSymbols(symbols)
    }

    test("classes") {
      test("without args") {
        val comp = mkCompilation("class Foo()")
        val symbols = enumNonBuiltinSymbols(comp)
        assertSymbol(symbols, SymbolKind.Class, "Foo")
        assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
        assertProgramSymbol(symbols)
        assertMainSymbol(symbols)
        assertNoSymbols(symbols)
      }

      test("with args") {
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

      test("fields") {
        val comp = mkCompilation(
          "class Foo() {\n" +
            "  var z = 0\n" +
            "}"
        )

        val symbols = enumNonBuiltinSymbols(comp)
        assertSymbol(symbols, SymbolKind.Class, "Foo")
        assertSymbol(symbols, SymbolKind.Field, "z")
        assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      }
    }

    test("enums") {
      test("without args") {
        val comp = mkCompilation(
          "enum Foo {\n" +
            "  case Bar\n" +
            "  case Baz\n" +
            "}"
        )
        val symbols = enumNonBuiltinSymbols(comp)
        assertSymbol(symbols, SymbolKind.Alias, "Foo")
        assertSymbol(symbols, SymbolKind.Class, "Bar")
        assertSymbol(symbols, SymbolKind.Class, "Baz")

        assertProgramSymbol(symbols)
        assertMainSymbol(symbols)

        assertNoSymbols(symbols)
      }

      test("with args") {
        val comp = mkCompilation(
          "enum Foo {\n" +
            "  case Bar(x: int)\n" +
            "  case Baz(y: int)\n" +
            "}"
        )
        val symbols = enumNonBuiltinSymbols(comp)
        assertSymbol(symbols, SymbolKind.Alias, "Foo")
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
  }
}
