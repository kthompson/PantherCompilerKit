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

    test("is expressions") {
      assertExprTypeTest("12 is int", "bool")
      assertExprTypeTest("'a' is char", "bool")
      assertExprTypeTest("\"hello\" is string", "bool")
      assertExprTypeTest("true is bool", "bool")
      assertExprTypeTest("12 is bool", "bool") // should return false at runtime
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

    test("matches") {
      test("literal patterns") {
        assertExprTypeTest("1 match { case 1 => 2 }", "int")
        assertExprTypeTest("1 match { case 2 => 3 }", "int")
        assertExprTypeTest("true match { case true => false }", "bool")
        assertExprTypeTest(
          "\"hello\" match { case \"world\" => \"hi\" }",
          "string"
        )
        assertExprTypeTest("'a' match { case 'b' => 'c' }", "char")
      }

      test("wildcard patterns") {
        assertExprTypeTest("1 match { case _ => 2 }", "int")
        assertExprTypeTest("true match { case _ => false }", "bool")
        assertExprTypeTest("\"hello\" match { case _ => \"world\" }", "string")
      }

//      test("variable patterns") {
//        assertExprTypeTest("1 match { case x: int => x + 1 }", "int")
//        assertExprTypeTest("true match { case b: bool => !b }", "bool")
//        assertExprTypeTest("\"hello\" match { case s: string => s }", "string")
//      }

      test("multiple cases with same type") {
        assertExprTypeTest(
          "1 match { case 1 => 10\n case 2 => 20 }",
          "int"
        )
        assertExprTypeTest(
          "true match { case true => 1\n case false => 0 }",
          "int"
        )
      }

//      test("multiple cases with different types") {
//        // When cases have different types, result should be 'any' (least upper bound)
//        assertExprTypeTest(
//          "1 match { case 1 => 42\n case 2 => \"hello\" }",
//          "any"
//        )
//        assertExprTypeTest(
//          "1 match { case 1 => true\n case 2 => 123 }",
//          "any"
//        )
//      }

      test("unit result cases") {
//        assertExprTypeTest("1 match { case x: int => () }", "unit")
        assertExprTypeTest("1 match { case 1 => println(\"test\") }", "unit")
      }

//      test("nested matches") {
//        assertExprTypeTest(
//          "1 match { case x: int => x match { case y: int => y * 2 } }",
//          "int"
//        )
//      }

//      test("match in expressions") {
//        assertExprTypeTest(
//          "(1 match { case x: int => x }) + 5",
//          "int"
//        )
//        assertExprTypeTest(
//          "!(true match { case b: bool => b })",
//          "bool"
//        )
//      }

//      test("match with blocks") {
//        assertExprTypeTest(
//          "1 match { case x: int => { val y = x + 1\n y * 2 } }",
//          "int"
//        )
//        assertExprTypeTest(
//          "true match { case b: bool => { println(\"test\")\n b } }",
//          "bool"
//        )
//      }
    }

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

      test("apply with method call") {
        val setup = "val array = new Array[int](1)"
        assertExprTypeWithSetup(setup, "array.apply(0)", "int")
      }

      test("apply with indexer syntax - basic types") {
        val setup = "val intArray = new Array[int](1)"
        assertExprTypeWithSetup(setup, "intArray(0)", "int")

        val boolSetup = "val boolArray = new Array[bool](1)"
        assertExprTypeWithSetup(boolSetup, "boolArray(0)", "bool")

        val stringSetup = "val stringArray = new Array[string](1)"
        assertExprTypeWithSetup(stringSetup, "stringArray(0)", "string")

        val charSetup = "val charArray = new Array[char](1)"
        assertExprTypeWithSetup(charSetup, "charArray(0)", "char")
      }

      test("array indexing in expressions") {
        val setup = "val array = new Array[int](5)"
        assertExprTypeWithSetup(setup, "array(0) + array(1)", "int")
        assertExprTypeWithSetup(setup, "array(2) * 3", "int")
        assertExprTypeWithSetup(setup, "array(0) == array(1)", "bool")
      }

      test("array indexing with computed indices") {
        val setup = "val array = new Array[int](10)\nval i = 5"
        assertExprTypeWithSetup(setup, "array(i)", "int")
        assertExprTypeWithSetup(setup, "array(1 + 2)", "int")
        assertExprTypeWithSetup(setup, "array(i * 2)", "int")
      }

      test("array indexing assignment - LHS binding fix") {
        // This tests the specific fix for "NewExpression in bindLHS" error
        val setup = "var array = new Array[int](5)"
        assertExprTypeWithSetup(setup, "array(0) = 42", "unit")
        assertExprTypeWithSetup(setup, "array(1) = array(0) + 1", "unit")
      }
    }

    test("casting") {
      test("primitive casts") {
        // Basic numeric casting
        assertExprTypeTest("42 as int", "int")
        assertExprTypeTest("42 as bool", "bool")
        assertExprTypeTest("42 as char", "char")
        assertExprTypeTest("42 as string", "string")

        // Boolean casting
        assertExprTypeTest("true as int", "int")
        assertExprTypeTest("true as bool", "bool")
        assertExprTypeTest("false as string", "string")

        // Character casting
        assertExprTypeTest("'a' as int", "int")
        assertExprTypeTest("'a' as char", "char")
        assertExprTypeTest("'a' as string", "string")

        // String casting
        assertExprTypeTest("\"hello\" as string", "string")
        assertExprTypeTest("\"hello\" as any", "any")
      }

      test("variable casts") {
        val setup =
          "val x = 42\nval flag = true\nval ch = 'a'\nval text = \"hello\""

        assertExprTypeWithSetup(setup, "x as bool", "bool")
        assertExprTypeWithSetup(setup, "x as char", "char")
        assertExprTypeWithSetup(setup, "x as string", "string")

        assertExprTypeWithSetup(setup, "flag as int", "int")
        assertExprTypeWithSetup(setup, "flag as string", "string")

        assertExprTypeWithSetup(setup, "ch as int", "int")
        assertExprTypeWithSetup(setup, "ch as string", "string")

        assertExprTypeWithSetup(setup, "text as any", "any")
      }

      test("cast to any type") {
        assertExprTypeTest("42 as any", "any")
        assertExprTypeTest("true as any", "any")
        assertExprTypeTest("\"hello\" as any", "any")
        assertExprTypeTest("'a' as any", "any")
        assertExprTypeTest("() as any", "any")
      }

      test("cast from any type") {
        val setup = "val obj: any = 42"

        assertExprTypeWithSetup(setup, "obj as int", "int")
        assertExprTypeWithSetup(setup, "obj as bool", "bool")
        assertExprTypeWithSetup(setup, "obj as char", "char")
        assertExprTypeWithSetup(setup, "obj as string", "string")
        assertExprTypeWithSetup(setup, "obj as unit", "unit")
      }

      test("expression casts") {
        // Cast results of binary operations
        assertExprTypeTest("(1 + 2) as bool", "bool")
        assertExprTypeTest("(true && false) as int", "int")
        assertExprTypeTest("(1 == 2) as string", "string")

        // Cast results of unary operations
        assertExprTypeTest("(!true) as int", "int")
        assertExprTypeTest("(-42) as bool", "bool")
      }

      test("cast with parentheses") {
        assertExprTypeTest("(42) as string", "string")
        assertExprTypeTest("(true) as int", "int")
        assertExprTypeTest("(\"hello\") as any", "any")
      }

      test("chained operations with casts") {
        val setup = "val x = 42"

        // Cast should have appropriate precedence
        assertExprTypeWithSetup(setup, "x as bool == true", "bool")
        assertExprTypeWithSetup(setup, "(x as bool) == true", "bool")
      }
    }

    test("string conversions") {
      test("basic string conversions") {
        // Convert literals to string
        assertExprTypeTest("string(42)", "string")
        assertExprTypeTest("string(0)", "string")
        assertExprTypeTest("string(-123)", "string")
        assertExprTypeTest("string(true)", "string")
        assertExprTypeTest("string(false)", "string")
        assertExprTypeTest("string('a')", "string")
        assertExprTypeTest("string('z')", "string")
        assertExprTypeTest("string(())", "string")
      }

      test("string conversions with variables") {
        val intSetup = "val num = 123"
        assertExprTypeWithSetup(intSetup, "string(num)", "string")

        val boolSetup = "val flag = true"
        assertExprTypeWithSetup(boolSetup, "string(flag)", "string")

        val charSetup = "val ch = 'A'"
        assertExprTypeWithSetup(charSetup, "string(ch)", "string")

        val unitSetup = "val nothing = ()"
        assertExprTypeWithSetup(unitSetup, "string(nothing)", "string")
      }

      test("string conversions with expressions") {
        // Arithmetic expressions
        assertExprTypeTest("string(1 + 2)", "string")
        assertExprTypeTest("string(10 - 5)", "string")
        assertExprTypeTest("string(3 * 4)", "string")
        assertExprTypeTest("string(15 / 3)", "string")
        assertExprTypeTest("string(17 % 5)", "string")

        // Boolean expressions
        assertExprTypeTest("string(true && false)", "string")
        assertExprTypeTest("string(true || false)", "string")
        assertExprTypeTest("string(!true)", "string")
        assertExprTypeTest("string(5 > 3)", "string")
        assertExprTypeTest("string(5 == 5)", "string")
        assertExprTypeTest("string(5 != 3)", "string")

        // Unary expressions
        assertExprTypeTest("string(-42)", "string")
        assertExprTypeTest("string(+42)", "string")
      }

      test("string conversions in complex expressions") {
        val setup = "val x = 42\nval y = true"

        // String conversions in comparisons
        assertExprTypeWithSetup(setup, "string(x) == \"42\"", "bool")
        assertExprTypeWithSetup(setup, "string(y) == \"true\"", "bool")
        assertExprTypeWithSetup(setup, "string(x) != \"0\"", "bool")

        // String conversions in arithmetic context
        assertExprTypeWithSetup(setup, "string(x + 10)", "string")
        assertExprTypeWithSetup(setup, "string(x * 2)", "string")
      }

      test("string conversions with method calls") {
        // Convert results of method calls to string
        assertExprTypeTest("string(println(\"test\"))", "string")
        assertExprTypeTest("string(print(42))", "string")
      }

      test("string conversions with control flow") {
        // String conversions with if expressions
        assertExprTypeTest("string(if (true) 1 else 2)", "string")
        assertExprTypeTest("string(if (false) true else false)", "string")

        // String conversions with block expressions
        assertExprTypeTest("string({ 42 })", "string")
        assertExprTypeTest("string({ true })", "string")
      }

      test("nested string conversions") {
        val setup = "val x = 42"

        // String conversion of string (should still work)
        assertExprTypeTest("string(\"hello\")", "string")
        assertExprTypeWithSetup(setup, "string(string(x))", "string")

        // String conversions in nested expressions
        assertExprTypeWithSetup(setup, "string(string(x) == \"42\")", "string")
      }
    }

    test("other type conversions") {
      test("int conversions") {
        // Convert various types to int
        assertExprTypeTest("int(42)", "int")
        assertExprTypeTest("int(true)", "int")
        assertExprTypeTest("int(false)", "int")
        assertExprTypeTest("int('a')", "int")

        // Int conversions with variables and expressions
        val setup = "val flag = true\nval ch = 'A'"
        assertExprTypeWithSetup(setup, "int(flag)", "int")
        assertExprTypeWithSetup(setup, "int(ch)", "int")
        assertExprTypeTest("int(1 + 2)", "int")
      }

      test("bool conversions") {
        // Convert various types to bool
        assertExprTypeTest("bool(true)", "bool")
        assertExprTypeTest("bool(false)", "bool")
        assertExprTypeTest("bool(42)", "bool")
        assertExprTypeTest("bool(0)", "bool")

        // Bool conversions with variables and expressions
        val setup = "val num = 123"
        assertExprTypeWithSetup(setup, "bool(num)", "bool")
        assertExprTypeTest("bool(5 > 3)", "bool")
      }

      test("char conversions") {
        // Convert various types to char
        assertExprTypeTest("char('a')", "char")
        assertExprTypeTest("char(65)", "char")

        // Char conversions with variables
        val setup = "val ascii = 97"
        assertExprTypeWithSetup(setup, "char(ascii)", "char")
      }
    }
  }
}
