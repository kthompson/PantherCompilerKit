import panther.{assert => _, *}
import TestHelpers._
import utest._

object TypeTests extends TestSuite {
  val tests = Tests {
    test("primitives") {
      assertExprTypeTest("12", "int")
      assertExprTypeTest("0", "int")
      assertExprTypeTest("true", "bool")
      assertExprTypeTest("false", "bool")
      assertExprTypeTest("\"hello\"", "string")
      assertExprTypeTest("'a'", "char")
      assertExprTypeTest("()", "unit")
    }

    test("binary") {
      assertExprTypeTest("1 + 2", "int")
      assertExprTypeTest("1 - 2", "int")
      assertExprTypeTest("1 * 2", "int")
      assertExprTypeTest("1 / 2", "int")
      assertExprTypeTest("1 % 2", "int")
      assertExprTypeTest("1 == 2", "bool")
      assertExprTypeTest("1 != 2", "bool")
      assertExprTypeTest("1 < 2", "bool")
      assertExprTypeTest("1 <= 2", "bool")
      assertExprTypeTest("1 > 2", "bool")
      assertExprTypeTest("1 >= 2", "bool")
      assertExprTypeTest("true && false", "bool")
      assertExprTypeTest("true || false", "bool")
    }

    test("unary") {
      assertExprTypeTest("-1", "int")
      assertExprTypeTest("+1", "int")
      // FIXME: assertExprType("~7", "int")
      assertExprTypeTest("!true", "bool")
    }

    test("groups") {
      assertExprTypeTest("(12)", "int")
      assertExprTypeTest("(true)", "bool")
    }

    test("variables") {
      test("int variable") {
        assertExprTypeWithSetup("val x = 12", "x", "int")
      }

      test("bool variable") {
        assertExprTypeWithSetup("val x = true", "x", "bool")
      }

      test("string variable") {
        assertExprTypeWithSetup("val x = \"hello\"", "x", "string")
      }

      test("char variable") {
        assertExprTypeWithSetup("val x = 'a'", "x", "char")
      }

      test("variable addition") {
        assertExprTypeWithSetup("val x = 12", "x + 12", "int")
      }

      test("variable equality") {
        assertExprTypeWithSetup("val x = 12", "12 == x", "bool")
      }

      test("variable assignment") {
        assertExprTypeWithSetup("var x = 12", "x = 10", "unit")
      }
    }

    test("conversions") {
      assertAssignableTo("12", "any")
      assertAssignableTo("true", "any")
      assertAssignableTo("\"hello\"", "any")
      assertAssignableTo("'a'", "any")
    }

    test("calls") {
      assertExprTypeTest("println(12)", "unit")
      assertExprTypeTest("print(12)", "unit")
      assertExprTypeTest("print(\"hello\")", "unit")
      assertExprTypeTest("print('a')", "unit")
      assertExprTypeTest("print(true)", "unit")
      assertExprTypeTest("print(12 + 12)", "unit")
      assertExprTypeTest("print(12 == 12)", "unit")
      assertExprTypeTest("print(12 < 12)", "unit")
      assertExprTypeTest("print(true && false)", "unit")
      assertExprTypeTest("print(true || false)", "unit")
    }

    test("casts") {
      assertExprTypeTest("'a' as char", "char")
      assertExprTypeTest("12 as char", "char")
      assertExprTypeTest("'a' as int", "int")
      assertExprTypeTest("12 as int", "int")
      assertExprTypeTest("'a' as string", "string")
      assertExprTypeTest("12 as string", "string")
      assertExprTypeTest("\"hello\" as string", "string")
      assertExprTypeTest("true as string", "string")
    }

    test("blocks") {
      assertExprTypeTest("{ 1 }", "int")
      assertExprTypeTest("{ true }", "bool")
      assertExprTypeTest("{  }", "unit")
      assertExprTypeTest(
        "{\n" +
          "  1\n" +
          "  2\n" +
          "}",
        "int"
      )
      assertExprTypeTest(
        "{\n" +
          "true\n" +
          "false\n" +
          "}",
        "bool"
      )
      assertExprTypeTest(
        "{\n" +
          "  1\n" +
          "  true\n" +
          "}",
        "bool"
      )
      assertExprTypeTest(
        "{\n" +
          "  true\n" +
          "  1\n" +
          "}",
        "int"
      )
    }

    test("ifs") {
      assertExprTypeTest("if (true) 1 else 2", "int")
      assertExprTypeTest("if (false) 1 else 2", "int")
      assertExprTypeTest("if (true) true else false", "bool")
      assertExprTypeTest("if (false) true else false", "bool")
      assertExprTypeTest("if (false) true", "unit")
    }

    test("whiles") {
      assertExprTypeTest("while (true) 1", "unit")
      assertExprTypeTest("while (false) 1", "unit")
    }

// FIXME:
//    test("matches") {
//      assertExprTypeTest("1 match { case 1 => 1 }", "int")
//      assertExprTypeTest("1 match { case 2 => 1 }", "int")
//      assertExprTypeTest("1 match { case 1 => true }", "bool")
//      assertExprTypeTest("1 match { case 2 => true }", "bool")
//      assertExprTypeTest("1 match { case 1 => }", "unit")
//    }

    test("methods") {
      test("without return type") {
        val comp = mkCompilation("def foo() = 12")
        val symbols = enumNonBuiltinSymbols(comp)
        assertProgramSymbol(symbols)
        val foo = assertSymbol(symbols, SymbolKind.Method, "foo")
        assertSymbolType(comp, foo, "() -> int")
        assertMainSymbol(symbols)
        assertNoSymbols(symbols)
      }

      test("with parameters") {
        val comp = mkCompilation("def foo(x: int, y: int) = 12")
        val symbols = enumNonBuiltinSymbols(comp)
        assertProgramSymbol(symbols)
        val foo = assertSymbol(symbols, SymbolKind.Method, "foo")
        assertSymbolType(comp, foo, "(x: int, y: int) -> int")
        val x = assertSymbol(symbols, SymbolKind.Parameter, "x")
        assertSymbolType(comp, x, "int")
        val y = assertSymbol(symbols, SymbolKind.Parameter, "y")
        assertSymbolType(comp, y, "int")
        assertMainSymbol(symbols)
        assertNoSymbols(symbols)
      }

//      test("with generic arguments") {
//        // FIXME: this is throwing an error atm
//        val comp = mkCompilation("def foo[T](x: T) = 12")
//        val symbols = enumNonBuiltinSymbols(comp)
//        val foo = assertSymbol(symbols, SymbolKind.Method, "foo")
//        assertSymbolType(comp, foo, "[T](x: T) -> int")
//        val x = assertSymbol(symbols, SymbolKind.Parameter, "x")
//        assertSymbolType(comp, x, "T")
//        assertNoSymbols(symbols)
//      }
    }

    test("enums") {
      test("without args") {
        val setup = "enum Foo {\n" +
          "  case Bar\n" +
          "  case Baz\n" +
          "}"
        assertExprTypeWithSetup(setup, "Foo.Bar", "Foo.Bar")
        assertExprTypeWithSetup(setup, "Foo.Baz", "Foo.Baz")
      }

      test("with args") {
        val setup = "enum Foo {\n" +
          "  case Bar(x: int)\n" +
          "  case Baz(y: string)\n" +
          "}"
        assertExprTypeWithSetup(setup, "Foo.Bar(12)", "Foo.Bar")
        assertExprTypeWithSetup(setup, "Foo.Baz(\"taco\")", "Foo.Baz")
        assertExprTypeWithSetup(setup, "new Foo.Bar(12)", "Foo.Bar")
        assertExprTypeWithSetup(setup, "new Foo.Baz(\"taco\")", "Foo.Baz")
      }

      test("check assignments") {
        val setup = "enum Foo {\n" +
          "  case Bar(x: int)\n" +
          "  case Baz(y: string)\n" +
          "}"
        assertAssignableToWithSetup(setup, "Foo.Bar(12)", "Foo")
        assertAssignableToWithSetup(setup, "Foo.Baz(\"taco\")", "Foo")
      }
// FIXME:
//      test("with generic type") {
//        val setup = "enum Option<T> {\n" +
//          "  case Some(value: T)\n" +
//          "  case None\n" +
//          "}"
//        assertExprTypeWithSetup(setup, "new Option.Some(12)", "Option<int>")
//        assertExprTypeWithSetup(setup, "Option.None", "Option<never>")
//      }
    }

    test("classes") {
      test("without args") {
        val setup = "class Foo()"
        assertExprTypeWithSetup(setup, "new Foo()", "Foo")
      }

      test("with args") {
        val setup = "class Foo(x: int, y: string)"
        assertExprTypeWithSetup(setup, "new Foo(12, \"taco\")", "Foo")
      }
    }

    test("arrays") {
// FIXME:
//      test("type") {
//        val setup = "val array = new Array[int](0)"
//        assertExprTypeWithSetup(setup, "array", "Array[int]")
//      }

      test("length type") {
        val setup = "val array = new Array[int](0)"
        assertExprTypeWithSetup(setup, "array.length", "int")
      }

//      test("apply") {
//        val setup = "val array = new Array[int](1)"
//        assertExprTypeWithSetup(setup, "array.apply(0)", "int")
//      }
//
//      test("apply with indexer") {
//        val setup = "val array = new Array[int](1)"
//        assertExprTypeWithSetup(setup, "array(0)", "int")
//      }
    }
  }
}
