import panther._
import utest._

object CompilationTests extends TestSuite {
  val tests = Tests {
    def makeCompilation(text: string): Compilation = {
      val syntaxTree = MakeSyntaxTree.parseContent(text)
      MakeCompilation.create(List.Cons(syntaxTree, List.Nil))
    }

    val predefinedSymbols = 8

    test("Compilation") {
//      test("create") {
//        val compilation = makeCompilation(
//          "def test<T>(a: T): T = a\n" +
//            "val x = test<int>(10)"
//        )
//        compilation.printSymbols()
//        assert(compilation.diagnostics.count() == 0)
//        compilation.root.lookup("test") match {
//          case Option.Some(symbol) =>
//            assert(symbol.kind == SymbolKind.Method)
//            assert(symbol.name == "test")
//            assert(symbol.members().length == 2)
//          case Option.None =>
//            assert(false)
//        }
//      }

//      test("generic type") {
//        val compilation = makeCompilation(
//          "enum Option<T> {\n" +
//            "  case Some(value: T)\n" +
//            "  case None\n" +
//            "}\n" +
//            "\n" +
//            "def test<T>(a: Option<T>) = a\n" +
//            "\n" +
//            "def x() = test(new Option.Some(10))"
//        )
//        compilation.printSymbols()
//        compilation.root.lookup("x") match {
//          case Option.Some(symbol) =>
//            assert(symbol.kind == SymbolKind.Method)
//            assert(symbol.name == "x")
//          case Option.None =>
//            assert(false)
//        }
//      }

      test("generic fields") {
        val compilation = makeCompilation(
          "enum Option<T> {\n" +
            "  case Some(value: T)\n" +
            "  case None\n" +
            "}\n" +
            "\n" +
            "val x = Option.None\n" +
            "\n" +
            "val y = Option.Some(10)"
        )
        compilation.printSymbols()
        compilation.diagnostics.printDiagnostics(20)
        assert(compilation.diagnostics.count() == 0)
        compilation.root.lookup("x") match {
          case Option.Some(symbol) =>
            assert(symbol.kind == SymbolKind.Field)
            assert(symbol.name == "x")
            compilation.binder.tryGetSymbolType(symbol) match {
              case Option.Some(t) =>
                assert(t.toString() == "Option<never>")
              case Option.None =>
                assert(false)
            }
          case Option.None =>
            assert(false)
        }

        compilation.root.lookup("y") match {
          case Option.Some(symbol) =>
            assert(symbol.kind == SymbolKind.Field)
            assert(symbol.name == "y")
            compilation.binder.tryGetSymbolType(symbol) match {
              case Option.Some(t) =>
                assert(t.toString() == "Option<int>")
              case Option.None =>
                assert(false)
            }
          case Option.None =>
            assert(false)
        }
      }
    }
  }
}
