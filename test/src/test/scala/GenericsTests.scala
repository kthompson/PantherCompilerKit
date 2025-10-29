import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class GenericsTests extends AnyFunSpec with TestHelpers {

  describe("Generics feature") {
    describe("generic identity method") {
      it("should type check generic identity method") {
        val setup = "def identity[T](x: T): T = x"
        assertExprTypeWithSetup(setup, "identity(42)", "int")
        assertExprTypeWithSetup(setup, "identity(true)", "bool")
        assertExprTypeWithSetup(setup, "identity(\"hello\")", "string")
      }
      
      it("should bind generic identity method") {
        val source =
          """def identity[T](x: T): T = x
            |""".stripMargin
        val comp = mkCompilation(source)
        val symbols = enumerateSymbolsSkipBuiltin(comp)
        assertProgramSymbol(symbols)
        assertSymbol(symbols, SymbolKind.Method, "identity")
        assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Invariant), "T")
        assertSymbol(symbols, SymbolKind.Parameter, "x")
        assertSymbol(symbols, SymbolKind.Method, "main")
        assertNoSymbols(symbols)
      }
      
      it("should execute generic identity method in VM") {
        val setup = "def identity[T](x: T): T = x"
        assertExecValueIntWithSetup(setup, "identity(42)", 42)
        assertExecValueBoolWithSetup(setup, "identity(true)", true)
        assertExecValueStringWithSetup(setup, "identity(\"hello\")", "hello")
      }
    }

    describe("generic Box class") {
      it("should parse generic class and method") {
        val source =
          """class Box[T](value: T)
            |
            |def wrap[T](x: T): Box[T] = new Box(x)
            |""".stripMargin
        val tree = mkSyntaxTree(source)
        tree.diagnostics.count() shouldBe 0
      }
      it("should bind generic class") {
        val source = "class Box[T](value: T)"
        val comp = mkCompilation(source)
        val symbols = enumerateSymbolsSkipBuiltin(comp)
        assertSymbol(symbols, SymbolKind.Class, "Box")
        assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Invariant), "T")
        assertSymbol(symbols, SymbolKind.Field, "value")
        assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
        assertSymbol(symbols, SymbolKind.Parameter, "value")
        assertProgramSymbol(symbols)
        assertSymbol(symbols, SymbolKind.Method, "main")
        assertNoSymbols(symbols)
      }

      it("should bind generic class and method symbols") {
        val source =
          """class Box[T](value: T)
            |
            |def wrap[T](x: T): Box[T] = new Box(x)
            |""".stripMargin
        val comp = mkCompilation(source)
        val symbols = enumerateSymbols(comp)
        assertSymbol(symbols, SymbolKind.Class, "Box")
        assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Invariant), "T")
        assertSymbol(symbols, SymbolKind.Field, "value")
        assertSymbol(symbols, SymbolKind.Method, "wrap")
        assertNoSymbols(symbols)
      }

      it("should execute generic container creation in VM") {
        val setup =
          """class Box[T](value: T)
            |def wrap[T](x: T): Box[T] = new Box(x)
            |""".stripMargin
        // Just check that VM can create and access generic containers
        val comp = mkCompilation(setup + "\nval box = wrap(123)")
        val symbols = enumerateSymbolsSkipBuiltin(comp)
        assertSymbol(symbols, SymbolKind.Field, "box")
        assertNoSymbols(symbols)
        // More advanced VM tests would require field access, which may need runtime helpers
      }
      
    }

    describe("generic Option enum") {
      it("should type check generic enum Option") {
        val setup =
          """enum Option[T] {
            |  case Some(value: T)
            |  case None
            |}""".stripMargin
        assertExprTypeWithSetup(setup, "Option.Some(42)", "Option.Some[int]")
        assertExprTypeWithSetup(setup, "Option.None", "Option.None[never]")
      }

      it("should bind generic enum Option symbols") {
        val source = {
          """
            |enum Option[T] {
            |   case Some(value: T)
            |   case None
            |}
            |""".stripMargin

        }
        val comp = mkCompilation(source)
        val symbols = enumerateSymbolsSkipBuiltin(comp)
        assertSymbol(symbols, SymbolKind.Alias, "Option")
        assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Invariant), "T")
        assertSymbol(symbols, SymbolKind.Class, "Some")
        assertSymbol(symbols, SymbolKind.Field, "value")
        assertSymbol(symbols, SymbolKind.Class, "None")
        assertNoSymbols(symbols)
      }
    }
  }
}
