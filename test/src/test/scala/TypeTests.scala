import panther.*
import TestHelpers.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class TypeTests extends AnyFunSpec with Matchers {

  describe("Type checker") {
    it("should infer primitive types") {
      assertInferExprType("12", "int")
      assertInferExprType("0", "int")
      assertInferExprType("true", "bool")
      assertInferExprType("false", "bool")
      assertInferExprType("\"hello\"", "string")
      assertInferExprType("'a'", "char")
      assertInferExprType("()", "unit")
    }

    it("should check primitive types") {
      assertCheckExprType("12", "int")
      assertCheckExprType("0", "int")
      assertCheckExprType("true", "bool")
      assertCheckExprType("false", "bool")
      assertCheckExprType("\"hello\"", "string")
      assertCheckExprType("'a'", "char")
      assertCheckExprType("()", "unit")
    }

    it("should infer binary expressions") {
      assertInferExprType("1 + 2", "int")
      assertInferExprType("1 - 2", "int")
      assertInferExprType("1 * 2", "int")
      assertInferExprType("1 / 2", "int")
      assertInferExprType("1 % 2", "int")
      assertInferExprType("1 == 2", "bool")
      assertInferExprType("1 != 2", "bool")
      assertInferExprType("1 < 2", "bool")
      assertInferExprType("1 <= 2", "bool")
      assertInferExprType("1 > 2", "bool")
      assertInferExprType("1 >= 2", "bool")
      assertInferExprType("true && false", "bool")
      assertInferExprType("true || false", "bool")
    }

    it("should check binary expressions") {
      assertCheckExprType("1 + 2", "int")
      assertCheckExprType("1 - 2", "int")
      assertCheckExprType("1 * 2", "int")
      assertCheckExprType("1 / 2", "int")
      assertCheckExprType("1 % 2", "int")
      assertCheckExprType("1 == 2", "bool")
      assertCheckExprType("1 != 2", "bool")
      assertCheckExprType("1 < 2", "bool")
      assertCheckExprType("1 <= 2", "bool")
      assertCheckExprType("1 > 2", "bool")
      assertCheckExprType("1 >= 2", "bool")
      assertCheckExprType("true && false", "bool")
      assertCheckExprType("true || false", "bool")
    }

    it("should infer unary expressions") {
      assertInferExprType("-1", "int")
      assertInferExprType("+1", "int")
      // FIXME: assertInferExprType("~7", "int")
      assertInferExprType("!true", "bool")
    }

    it("should check unary expressions") {
      assertCheckExprType("-1", "int")
      assertCheckExprType("+1", "int")
      // FIXME: assertCheckExprType("~7", "int")
      assertCheckExprType("!true", "bool")
    }

    it("should infer grouped expressions") {
      assertInferExprType("(12)", "int")
      assertInferExprType("(true)", "bool")
    }

    it("should check grouped expressions") {
      assertCheckExprType("(12)", "int")
      assertCheckExprType("(true)", "bool")
    }

    it("should infer int variables") {
      assertInferExprTypeWithSetup("val x = 12", "x", "int")
    }

    it("should check int variables") {
      assertCheckExprTypeWithSetup("val x = 12", "x", "int")
    }

    it("should infer bool variables") {
      assertInferExprTypeWithSetup("val x = true", "x", "bool")
    }

    it("should check bool variables") {
      assertCheckExprTypeWithSetup("val x = true", "x", "bool")
    }

    it("should infer string variables") {
      assertInferExprTypeWithSetup("val x = \"hello\"", "x", "string")
    }

    it("should check string variables") {
      assertCheckExprTypeWithSetup("val x = \"hello\"", "x", "string")
    }

    it("should infer char variables") {
      assertInferExprTypeWithSetup("val x = 'a'", "x", "char")
    }

    it("should check char variables") {
      assertCheckExprTypeWithSetup("val x = 'a'", "x", "char")
    }

    it("should infer variable addition") {
      assertInferExprTypeWithSetup("val x = 12", "x + 12", "int")
    }

    it("should check variable addition") {
      assertCheckExprTypeWithSetup("val x = 12", "x + 12", "int")
    }

    it("should infer variable equality") {
      assertInferExprTypeWithSetup("val x = 12", "12 == x", "bool")
    }

    it("should check variable equality") {
      assertCheckExprTypeWithSetup("val x = 12", "12 == x", "bool")
    }

    it("should infer variable assignment") {
      assertInferExprTypeWithSetup("var x = 12", "x = 10", "unit")
    }

    it("should check variable assignment") {
      assertCheckExprTypeWithSetup("var x = 12", "x = 10", "unit")
    }

    it("should infer conversions to any") {
      assertAssignableTo("12", "any")
      assertAssignableTo("true", "any")
      assertAssignableTo("\"hello\"", "any")
      assertAssignableTo("'a'", "any")
    }

    it("should infer function calls") {
      assertInferExprType("println(12)", "unit")
      assertInferExprType("print(12)", "unit")
      assertInferExprType("print(\"hello\")", "unit")
      assertInferExprType("print('a')", "unit")
      assertInferExprType("print(true)", "unit")
      assertInferExprType("print(12 + 12)", "unit")
      assertInferExprType("print(12 == 12)", "unit")
      assertInferExprType("print(12 < 12)", "unit")
      assertInferExprType("print(true && false)", "unit")
      assertInferExprType("print(true || false)", "unit")
    }

    it("should check function calls") {
      assertCheckExprType("println(12)", "unit")
      assertCheckExprType("print(12)", "unit")
      assertCheckExprType("print(\"hello\")", "unit")
      assertCheckExprType("print('a')", "unit")
      assertCheckExprType("print(true)", "unit")
      assertCheckExprType("print(12 + 12)", "unit")
      assertCheckExprType("print(12 == 12)", "unit")
      assertCheckExprType("print(12 < 12)", "unit")
      assertCheckExprType("print(true && false)", "unit")
      assertCheckExprType("print(true || false)", "unit")
    }

    it("should infer casts") {
      assertInferExprType("'a' as char", "char")
      assertInferExprType("12 as char", "char")
      assertInferExprType("'a' as int", "int")
      assertInferExprType("12 as int", "int")
      assertInferExprType("'a' as string", "string")
      assertInferExprType("12 as string", "string")
      assertInferExprType("\"hello\" as string", "string")
      assertInferExprType("true as string", "string")
    }

    it("should check casts") {
      assertCheckExprType("'a' as char", "char")
      assertCheckExprType("12 as char", "char")
      assertCheckExprType("'a' as int", "int")
      assertCheckExprType("12 as int", "int")
      assertCheckExprType("'a' as string", "string")
      assertCheckExprType("12 as string", "string")
      assertCheckExprType("\"hello\" as string", "string")
      assertCheckExprType("true as string", "string")
    }

    it("should infer is expressions") {
      assertInferExprType("12 is int", "bool")
      assertInferExprType("'a' is char", "bool")
      assertInferExprType("\"hello\" is string", "bool")
      assertInferExprType("true is bool", "bool")
      assertInferExprType(
        "12 is bool",
        "bool"
      ) // should return false at runtime
    }

    it("should check is expressions") {
      assertCheckExprType("12 is int", "bool")
      assertCheckExprType("'a' is char", "bool")
      assertCheckExprType("\"hello\" is string", "bool")
      assertCheckExprType("true is bool", "bool")
      assertCheckExprType("12 is bool", "bool")
    }

    it("should infer block expressions") {
      assertInferExprType("{ 1 }", "int")
      assertInferExprType("{ true }", "bool")
      assertInferExprType("{  }", "unit")
      assertInferExprType(
        "{\n" +
          "  1\n" +
          "  2\n" +
          "}",
        "int"
      )
      assertInferExprType(
        "{\n" +
          "true\n" +
          "false\n" +
          "}",
        "bool"
      )
      assertInferExprType(
        "{\n" +
          "  1\n" +
          "  true\n" +
          "}",
        "bool"
      )
      assertInferExprType(
        "{\n" +
          "  true\n" +
          "  1\n" +
          "}",
        "int"
      )
    }

    it("should check block expressions") {
      assertCheckExprType("{ 1 }", "int")
      assertCheckExprType("{ true }", "bool")
      assertCheckExprType("{  }", "unit")
      assertCheckExprType(
        "{\n" +
          "  1\n" +
          "  2\n" +
          "}",
        "int"
      )
      assertCheckExprType(
        "{\n" +
          "true\n" +
          "false\n" +
          "}",
        "bool"
      )
      assertCheckExprType(
        "{\n" +
          "  1\n" +
          "  true\n" +
          "}",
        "bool"
      )
      assertCheckExprType(
        "{\n" +
          "  true\n" +
          "  1\n" +
          "}",
        "int"
      )
    }

    it("should infer if expressions") {
      assertInferExprType("if (true) 1 else 2", "int")
      assertInferExprType("if (false) 1 else 2", "int")
      assertInferExprType("if (true) true else false", "bool")
      assertInferExprType("if (false) true else false", "bool")
      assertInferExprType("if (false) true", "unit")
    }

    it("should check if expressions") {
      assertCheckExprType("if (true) 1 else 2", "int")
      assertCheckExprType("if (false) 1 else 2", "int")
      assertCheckExprType("if (true) true else false", "bool")
      assertCheckExprType("if (false) true else false", "bool")
      assertCheckExprType("if (false) true", "unit")
    }

    it("should infer while expressions") {
      assertInferExprType("while (true) 1", "unit")
      assertInferExprType("while (false) 1", "unit")
    }

    it("should check while expressions") {
      assertCheckExprType("while (true) 1", "unit")
      assertCheckExprType("while (false) 1", "unit")
    }

    it("should infer for expressions with literal bounds") {
      assertInferExprType("for (i <- 0 to 10) i", "unit")
      assertInferExprType("for (x <- 1 to 5) x", "unit")
      assertInferExprType("for (n <- 0 to 0) n", "unit")
    }

    it("should check for expressions with literal bounds") {
      assertCheckExprType("for (i <- 0 to 10) i", "unit")
      assertCheckExprType("for (x <- 1 to 5) x", "unit")
      assertCheckExprType("for (n <- 0 to 0) n", "unit")
    }

    it("should infer for expressions with variable bounds") {
      val setup = "val start = 0\nval end = 10"
      assertInferExprTypeWithSetup(setup, "for (i <- start to end) i", "unit")

      val setup2 = "val n = 5"
      assertInferExprTypeWithSetup(setup2, "for (i <- 0 to n) i", "unit")
      assertInferExprTypeWithSetup(setup2, "for (i <- n to 10) i", "unit")
    }

    it("should check for expressions with variable bounds") {
      val setup = "val start = 0\nval end = 10"
      assertCheckExprTypeWithSetup(setup, "for (i <- start to end) i", "unit")

      val setup2 = "val n = 5"
      assertCheckExprTypeWithSetup(setup2, "for (i <- 0 to n) i", "unit")
      assertCheckExprTypeWithSetup(setup2, "for (i <- n to 10) i", "unit")
    }

    it("should infer for expressions with computed bounds") {
      assertInferExprType("for (i <- 1 + 2 to 10 - 3) i", "unit")
      assertInferExprType("for (i <- 0 to 5 * 2) i", "unit")

      val setup = "val n = 10"
      assertInferExprTypeWithSetup(setup, "for (i <- 0 to n * 2) i", "unit")
    }

    it("should check for expressions with computed bounds") {
      assertCheckExprType("for (i <- 1 + 2 to 10 - 3) i", "unit")
      assertCheckExprType("for (i <- 0 to 5 * 2) i", "unit")

      val setup = "val n = 10"
      assertCheckExprTypeWithSetup(setup, "for (i <- 0 to n * 2) i", "unit")
    }

    it("should infer for expressions with different body types") {
      assertInferExprType("for (i <- 0 to 5) true", "unit")
      assertInferExprType("for (i <- 0 to 5) \"hello\"", "unit")
      assertInferExprType("for (i <- 0 to 5) 'a'", "unit")
      assertInferExprType("for (i <- 0 to 5) ()", "unit")
    }

    it("should check for expressions with different body types") {
      assertCheckExprType("for (i <- 0 to 5) true", "unit")
      assertCheckExprType("for (i <- 0 to 5) \"hello\"", "unit")
      assertCheckExprType("for (i <- 0 to 5) 'a'", "unit")
      assertCheckExprType("for (i <- 0 to 5) ()", "unit")
    }

    it("should infer for expressions with loop variable usage") {
      assertInferExprType("for (i <- 0 to 10) i + 1", "unit")
      assertInferExprType("for (i <- 0 to 10) i * 2", "unit")
      assertInferExprType("for (i <- 1 to 5) i == 3", "unit")
    }

    it("should check for expressions with loop variable usage") {
      assertCheckExprType("for (i <- 0 to 10) i + 1", "unit")
      assertCheckExprType("for (i <- 0 to 10) i * 2", "unit")
      assertCheckExprType("for (i <- 1 to 5) i == 3", "unit")
    }

    it("should infer for expressions accessing outer scope") {
      val setup = "val x = 10"
      assertInferExprTypeWithSetup(setup, "for (i <- 0 to 5) x", "unit")
      assertInferExprTypeWithSetup(setup, "for (i <- 0 to 5) i + x", "unit")
    }

    it("should check for expressions accessing outer scope") {
      val setup = "val x = 10"
      assertCheckExprTypeWithSetup(setup, "for (i <- 0 to 5) x", "unit")
      assertCheckExprTypeWithSetup(setup, "for (i <- 0 to 5) i + x", "unit")
    }

    it("should infer nested for expressions") {
      assertInferExprType("for (i <- 0 to 3) for (j <- 0 to 3) i + j", "unit")
      assertInferExprType("for (x <- 1 to 2) for (y <- 1 to 2) x * y", "unit")
    }

    it("should check nested for expressions") {
      assertCheckExprType("for (i <- 0 to 3) for (j <- 0 to 3) i + j", "unit")
      assertCheckExprType("for (x <- 1 to 2) for (y <- 1 to 2) x * y", "unit")
    }

    it("should infer for expressions with block bodies") {
      assertInferExprType("for (i <- 0 to 5) { i }", "unit")
      assertInferExprType("for (i <- 0 to 5) { val x = i\n x * 2 }", "unit")
    }

    it("should check for expressions with block bodies") {
      assertCheckExprType("for (i <- 0 to 5) { i }", "unit")
      assertCheckExprType("for (i <- 0 to 5) { val x = i\n x * 2 }", "unit")
    }

    it("should infer literal patterns") {
      assertInferExprType("1 match { case 1 => 2 }", "int")
      assertInferExprType("1 match { case 2 => 3 }", "int")
      assertInferExprType("true match { case true => false }", "bool")
      assertInferExprType(
        "\"hello\" match { case \"world\" => \"hi\" }",
        "string"
      )
      assertInferExprType("'a' match { case 'b' => 'c' }", "char")
    }

    it("should check literal patterns") {
      assertCheckExprType("1 match { case 1 => 2 }", "int")
      assertCheckExprType("1 match { case 2 => 3 }", "int")
      assertCheckExprType("true match { case true => false }", "bool")
      assertCheckExprType(
        "\"hello\" match { case \"world\" => \"hi\" }",
        "string"
      )
      assertCheckExprType("'a' match { case 'b' => 'c' }", "char")
    }

    it("should infer wildcard patterns") {
      assertInferExprType("1 match { case _ => 2 }", "int")
      assertInferExprType("true match { case _ => false }", "bool")
      assertInferExprType("\"hello\" match { case _ => \"world\" }", "string")
    }

    it("should check wildcard patterns") {
      assertCheckExprType("1 match { case _ => 2 }", "int")
      assertCheckExprType("true match { case _ => false }", "bool")
      assertCheckExprType("\"hello\" match { case _ => \"world\" }", "string")
    }

    it("should infer multiple cases with same type") {
      assertInferExprType(
        "1 match { case 1 => 10\n case 2 => 20 }",
        "int"
      )
      assertInferExprType(
        "true match { case true => 1\n case false => 0 }",
        "int"
      )
    }

    it("should check multiple cases with same type") {
      assertCheckExprType(
        "1 match { case 1 => 10\n case 2 => 20 }",
        "int"
      )
      assertCheckExprType(
        "true match { case true => 1\n case false => 0 }",
        "int"
      )
    }

    it("should infer multiple cases with different types") {
      // When cases have different types, result should be 'any' (least upper bound)
      assertInferExprType(
        "1 match { case 1 => 42\n case 2 => \"hello\" }",
        "int | string"
      )
      assertInferExprType(
        "1 match { case 1 => true\n case 2 => 123 }",
        "bool | int"
      )
    }

//    it("should check multiple cases with different types") {
//      assertCheckExprType(
//        "1 match { case 1 => 42\n case 2 => \"hello\" }",
//        "int | string"
//      )
//      assertCheckExprType(
//        "1 match { case 1 => true\n case 2 => 123 }",
//        "bool | int"
//      )
//    }

    it("should infer unit result cases") {
      assertInferExprType("1 match { case x: int => () }", "unit")
      assertInferExprType("1 match { case 1 => println(\"test\") }", "unit")
    }

    it("should check unit result cases") {
      assertCheckExprType("1 match { case x: int => () }", "unit")
      assertCheckExprType("1 match { case 1 => println(\"test\") }", "unit")
    }

    it("should infer nested matches") {
      assertInferExprType(
        "1 match { case x: int => x match { case y: int => y * 2 } }",
        "int"
      )
    }

    it("should check nested matches") {
      assertCheckExprType(
        "1 match { case x: int => x match { case y: int => y * 2 } }",
        "int"
      )
    }

    it("should infer match with blocks") {
      assertInferExprType(
        "1 match { case x: int => { val y = x + 1\n y * 2 } }",
        "int"
      )
      assertInferExprType(
        "true match { case b: bool => { println(\"test\")\n b } }",
        "bool"
      )
    }

    it("should check match with blocks") {
      assertCheckExprType(
        "1 match { case x: int => { val y = x + 1\n y * 2 } }",
        "int"
      )
      assertCheckExprType(
        "true match { case b: bool => { println(\"test\")\n b } }",
        "bool"
      )
    }

    it("should infer methods without return type") {
      val comp = mkCompilation("def foo() = 12")
      val symbols = enumNonBuiltinSymbols(comp)
      assertProgramSymbol(symbols)
      val foo = assertSymbol(symbols, SymbolKind.Method, "foo")
      assertSymbolType(comp, foo, "() -> int")
      assertMainSymbol(symbols)
      assertNoSymbols(symbols)
    }

    it("should infer methods with parameters") {
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

    it("should infer simple identity generic method") {
      val setup = "def identity[T](x: T): T = x"

      assertInferExprTypeWithSetup(setup, "identity(42)", "int")
      assertInferExprTypeWithSetup(setup, "identity(true)", "bool")
      assertInferExprTypeWithSetup(setup, "identity(\"hello\")", "string")
      assertInferExprTypeWithSetup(setup, "identity('a')", "char")
    }

    it("should check simple identity generic method") {
      val setup = "def identity[T](x: T): T = x"

      assertCheckExprTypeWithSetup(setup, "identity(42)", "int")
      assertCheckExprTypeWithSetup(setup, "identity(true)", "bool")
      assertCheckExprTypeWithSetup(setup, "identity(\"hello\")", "string")
      assertCheckExprTypeWithSetup(setup, "identity('a')", "char")
    }

    it("should infer generic method returning concrete type") {
      val setup = "def getValue[T](x: T): int = 42"

      // These should work because the return type is concrete
      assertInferExprTypeWithSetup(setup, "getValue(42)", "int")
      assertInferExprTypeWithSetup(setup, "getValue(\"hello\")", "int")
      assertInferExprTypeWithSetup(setup, "getValue(true)", "int")
    }

    it("should check generic method returning concrete type") {
      val setup = "def getValue[T](x: T): int = 42"

      // These should work because the return type is concrete
      assertCheckExprTypeWithSetup(setup, "getValue(42)", "int")
      assertCheckExprTypeWithSetup(setup, "getValue(\"hello\")", "int")
      assertCheckExprTypeWithSetup(setup, "getValue(true)", "int")
    }

    it("should infer generic parameter with concrete return") {
      // Test methods that accept generic parameters but return concrete types
      val setup = "def stringify[T](x: T): string = string(x)"

      assertInferExprTypeWithSetup(setup, "stringify(42)", "string")
      assertInferExprTypeWithSetup(setup, "stringify(true)", "string")
      assertInferExprTypeWithSetup(setup, "stringify('a')", "string")
    }

    it("should check generic parameter with concrete return") {
      // Test methods that accept generic parameters but return concrete types
      val setup = "def stringify[T](x: T): string = string(x)"

      assertCheckExprTypeWithSetup(setup, "stringify(42)", "string")
      assertCheckExprTypeWithSetup(setup, "stringify(true)", "string")
      assertCheckExprTypeWithSetup(setup, "stringify('a')", "string")
    }

    it("should infer generic container creation") {
      val containerSetup = "class Container[T](value: T)\n" +
        "def wrap[T](x: T): Container[T] = new Container(x)"

      assertInferExprTypeWithSetup(containerSetup, "wrap(42)", "Container<int>")
      assertInferExprTypeWithSetup(
        containerSetup,
        "wrap(true)",
        "Container<bool>"
      )
      assertInferExprTypeWithSetup(
        containerSetup,
        "wrap(\"test\")",
        "Container<string>"
      )
    }

//    it("should check generic container creation") {
//      val containerSetup = "class Container[T](value: T)\n" +
//        "def wrap[T](x: T): Container[T] = new Container(x)"
//
//      assertCheckExprTypeWithSetup(containerSetup, "wrap(42)", "Container<int>")
//      assertCheckExprTypeWithSetup(containerSetup, "wrap(true)", "Container<bool>")
//      assertCheckExprTypeWithSetup(
//        containerSetup,
//        "wrap(\"test\")",
//        "Container<string>"
//      )
//    }

    it("should infer enums without args") {
      val setup = "enum Foo {\n" +
        "  case Bar\n" +
        "  case Baz\n" +
        "}"
      assertInferExprTypeWithSetup(setup, "Foo.Bar", "Foo.Bar")
      assertInferExprTypeWithSetup(setup, "Foo.Baz", "Foo.Baz")
    }

    it("should check enums without args") {
      val setup = "enum Foo {\n" +
        "  case Bar\n" +
        "  case Baz\n" +
        "}"
      assertCheckExprTypeWithSetup(setup, "Foo.Bar", "Foo.Bar")
      assertCheckExprTypeWithSetup(setup, "Foo.Baz", "Foo.Baz")
    }

    it("should infer enums with args") {
      val setup = "enum Foo {\n" +
        "  case Bar(x: int)\n" +
        "  case Baz(y: string)\n" +
        "}"
      assertInferExprTypeWithSetup(setup, "Foo.Bar(12)", "Foo.Bar")
      assertInferExprTypeWithSetup(setup, "Foo.Baz(\"taco\")", "Foo.Baz")
      assertInferExprTypeWithSetup(setup, "new Foo.Bar(12)", "Foo.Bar")
      assertInferExprTypeWithSetup(setup, "new Foo.Baz(\"taco\")", "Foo.Baz")
    }

    it("should check enums with args") {
      val setup = "enum Foo {\n" +
        "  case Bar(x: int)\n" +
        "  case Baz(y: string)\n" +
        "}"
      assertCheckExprTypeWithSetup(setup, "Foo.Bar(12)", "Foo.Bar")
      assertCheckExprTypeWithSetup(setup, "Foo.Baz(\"taco\")", "Foo.Baz")
      assertCheckExprTypeWithSetup(setup, "new Foo.Bar(12)", "Foo.Bar")
      assertCheckExprTypeWithSetup(setup, "new Foo.Baz(\"taco\")", "Foo.Baz")
    }

    it("should check enum assignments") {
      val setup = "enum Foo {\n" +
        "  case Bar(x: int)\n" +
        "  case Baz(y: string)\n" +
        "}"
      assertAssignableToWithSetup(setup, "Foo.Bar(12)", "Foo")
      assertAssignableToWithSetup(setup, "Foo.Baz(\"taco\")", "Foo")
    }

    it("should infer enums with generic type") {
      val setup = "enum Option[T] {\n" +
        "  case Some(value: T)\n" +
        "  case None\n" +
        "}"
      assertAssignableToWithSetup(setup, "new Option.Some(12)", "Option[int]")
      assertAssignableToWithSetup(
        setup,
        "new Option.Some(12)",
        "Option.Some[int]"
      )
      assertAssignableToWithSetup(setup, "Option.Some(12)", "Option[int]")
      assertAssignableToWithSetup(setup, "Option.None", "Option[int]")
      assertAssignableToWithSetup(setup, "Option.None", "Option[never]")
    }

    it("should infer list examples") {
      val setup = "enum List[T] {\n" +
        "  case Cons(head: T, tail: List[T])\n" +
        "  case Nil\n" +
        "}"
      assertAssignableToWithSetup(
        setup,
        "new List.Cons(1, List.Nil)",
        "List[int]"
      )
      assertAssignableToWithSetup(
        setup,
        "List.Cons(1, List.Nil)",
        "List[int]"
      )
      assertAssignableToWithSetup(
        setup,
        "new List.Cons(\"hello\", List.Nil)",
        "List[string]"
      )
      assertAssignableToWithSetup(
        setup,
        "List.Cons(\"hello\", List.Nil)",
        "List[string]"
      )
      assertAssignableToWithSetup(setup, "List.Nil", "List[int]")
      assertAssignableToWithSetup(setup, "List.Nil", "List[string]")
    }

    it("should infer classes without args") {
      val setup = "class Foo()"
      assertInferExprTypeWithSetup(setup, "new Foo()", "Foo")
    }

    it("should check classes without args") {
      val setup = "class Foo()"
      assertCheckExprTypeWithSetup(setup, "new Foo()", "Foo")
    }

    it("should infer classes with args") {
      val setup = "class Foo(x: int, y: string)"
      assertInferExprTypeWithSetup(setup, "new Foo(12, \"taco\")", "Foo")
    }

    it("should check classes with args") {
      val setup = "class Foo(x: int, y: string)"
      assertCheckExprTypeWithSetup(setup, "new Foo(12, \"taco\")", "Foo")
    }

    it("should infer array creation for basic types") {
      assertInferExprType("new Array[int](0)", "Array<int>")
      assertInferExprType("new Array[bool](5)", "Array<bool>")
      assertInferExprType("new Array[string](10)", "Array<string>")
      assertInferExprType("new Array[char](3)", "Array<char>")
    }

    it("should check array creation for basic types") {
      assertAssignableTo("new Array[int](0)", "Array[int]")
      assertAssignableTo("new Array[bool](5)", "Array[bool]")
      assertAssignableTo("new Array[string](10)", "Array[string]")
      assertAssignableTo("new Array[char](3)", "Array[char]")
    }

    it("should infer array creation with computed size") {
      assertInferExprTypeWithSetup(
        "val n = 10",
        "new Array[int](n)",
        "Array<int>"
      )
      assertInferExprTypeWithSetup(
        "val x = 5",
        "new Array[int](x * 2)",
        "Array<int>"
      )
      assertInferExprTypeWithSetup("", "new Array[int](5 + 3)", "Array<int>")
    }

    it("should check array creation with computed size") {
      assertAssignableToWithSetup(
        "val n = 10",
        "new Array[int](n)",
        "Array[int]"
      )
      assertAssignableToWithSetup(
        "val x = 5",
        "new Array[int](x * 2)",
        "Array[int]"
      )
      assertAssignableToWithSetup("", "new Array[int](5 + 3)", "Array[int]")
    }

    it("should infer nested array types") {
      assertInferExprType("new Array[Array[int]](3)", "Array<Array<int>>")
      assertInferExprType("new Array[Array[bool]](2)", "Array<Array<bool>>")
    }

    it("should check nested array types") {
      assertAssignableTo("new Array[Array[int]](3)", "Array[Array[int]]")
      assertAssignableTo("new Array[Array[bool]](2)", "Array[Array[bool]]")
    }

    it("should infer array creation with class types") {
      val setup = "class Foo(x: int)"
      assertInferExprTypeWithSetup(setup, "new Array[Foo](5)", "Array<Foo>")
    }

    it("should check array creation with class types") {
      val setup = "class Foo(x: int)"
      assertAssignableToWithSetup(setup, "new Array[Foo](5)", "Array[Foo]")
    }

    it("should infer array creation in variable declarations") {
      assertInferExprType("new Array[int](0)", "Array<int>")
      assertInferExprTypeWithSetup(
        "val arr = new Array[int](5)",
        "arr",
        "Array<int>"
      )
    }

    it("should check array creation subsumes to expected type") {
      // Check mode should allow array creation to subsume to the expected type
      assertAssignableTo("new Array[int](5)", "Array[int]")
      assertAssignableToWithSetup("", "new Array[string](3)", "Array[string]")
    }

    it("should infer array length type") {
      val setup = "val array = new Array[int](0)"
      assertInferExprTypeWithSetup(setup, "array.length", "int")
    }

    it("should check array length type") {
      val setup = "val array = new Array[int](0)"
      assertCheckExprTypeWithSetup(setup, "array.length", "int")
    }

    it("should infer array apply with method call") {
      val setup = "val array = new Array[int](1)"
      assertInferExprTypeWithSetup(setup, "array.apply(0)", "int")
    }

    it("should check array apply with method call") {
      val setup = "val array = new Array[int](1)"
      assertCheckExprTypeWithSetup(setup, "array.apply(0)", "int")
    }

    it("should infer array apply with indexer syntax for basic types") {
      val setup = "val intArray = new Array[int](1)"
      assertInferExprTypeWithSetup(setup, "intArray(0)", "int")

      val boolSetup = "val boolArray = new Array[bool](1)"
      assertInferExprTypeWithSetup(boolSetup, "boolArray(0)", "bool")

      val stringSetup = "val stringArray = new Array[string](1)"
      assertInferExprTypeWithSetup(stringSetup, "stringArray(0)", "string")

      val charSetup = "val charArray = new Array[char](1)"
      assertInferExprTypeWithSetup(charSetup, "charArray(0)", "char")
    }

    it("should check array apply with indexer syntax for basic types") {
      val setup = "val intArray = new Array[int](1)"
      assertCheckExprTypeWithSetup(setup, "intArray(0)", "int")

      val boolSetup = "val boolArray = new Array[bool](1)"
      assertCheckExprTypeWithSetup(boolSetup, "boolArray(0)", "bool")

      val stringSetup = "val stringArray = new Array[string](1)"
      assertCheckExprTypeWithSetup(stringSetup, "stringArray(0)", "string")

      val charSetup = "val charArray = new Array[char](1)"
      assertCheckExprTypeWithSetup(charSetup, "charArray(0)", "char")
    }

    it("should infer array indexing in expressions") {
      val setup = "val array = new Array[int](5)"
      assertInferExprTypeWithSetup(setup, "array(0) + array(1)", "int")
      assertInferExprTypeWithSetup(setup, "array(2) * 3", "int")
      assertInferExprTypeWithSetup(setup, "array(0) == array(1)", "bool")
    }

    it("should check array indexing in expressions") {
      val setup = "val array = new Array[int](5)"
      assertCheckExprTypeWithSetup(setup, "array(0) + array(1)", "int")
      assertCheckExprTypeWithSetup(setup, "array(2) * 3", "int")
      assertCheckExprTypeWithSetup(setup, "array(0) == array(1)", "bool")
    }

    it("should infer array indexing with computed indices") {
      val setup = "val array = new Array[int](10)\nval i = 5"
      assertInferExprTypeWithSetup(setup, "array(i)", "int")
      assertInferExprTypeWithSetup(setup, "array(1 + 2)", "int")
      assertInferExprTypeWithSetup(setup, "array(i * 2)", "int")
    }

    it("should check array indexing with computed indices") {
      val setup = "val array = new Array[int](10)\nval i = 5"
      assertCheckExprTypeWithSetup(setup, "array(i)", "int")
      assertCheckExprTypeWithSetup(setup, "array(1 + 2)", "int")
      assertCheckExprTypeWithSetup(setup, "array(i * 2)", "int")
    }

    it("should infer array indexing assignment - LHS binding fix") {
      // This tests the specific fix for "NewExpression in bindLHS" error
      val setup = "var array = new Array[int](5)"
      assertInferExprTypeWithSetup(setup, "array(0) = 42", "unit")
      assertInferExprTypeWithSetup(setup, "array(1) = array(0) + 1", "unit")
    }

    it("should check array indexing assignment - LHS binding fix") {
      val setup = "var array = new Array[int](5)"
      assertCheckExprTypeWithSetup(setup, "array(0) = 42", "unit")
      assertCheckExprTypeWithSetup(setup, "array(1) = array(0) + 1", "unit")
    }

    it("should infer primitive casts") {
      // Basic numeric casting
      assertInferExprType("42 as int", "int")
      assertInferExprType("42 as bool", "bool")
      assertInferExprType("42 as char", "char")
      assertInferExprType("42 as string", "string")

      // Boolean casting
      assertInferExprType("true as int", "int")
      assertInferExprType("true as bool", "bool")
      assertInferExprType("false as string", "string")

      // Character casting
      assertInferExprType("'a' as int", "int")
      assertInferExprType("'a' as char", "char")
      assertInferExprType("'a' as string", "string")

      // String casting
      assertInferExprType("\"hello\" as string", "string")
      assertInferExprType("\"hello\" as any", "any")
    }

    it("should check primitive casts") {
      // Basic numeric casting
      assertCheckExprType("42 as int", "int")
      assertCheckExprType("42 as bool", "bool")
      assertCheckExprType("42 as char", "char")
      assertCheckExprType("42 as string", "string")

      // Boolean casting
      assertCheckExprType("true as int", "int")
      assertCheckExprType("true as bool", "bool")
      assertCheckExprType("false as string", "string")

      // Character casting
      assertCheckExprType("'a' as int", "int")
      assertCheckExprType("'a' as char", "char")
      assertCheckExprType("'a' as string", "string")

      // String casting
      assertCheckExprType("\"hello\" as string", "string")
      assertCheckExprType("\"hello\" as any", "any")
    }

    it("should infer variable casts") {
      val setup =
        "val x = 42\nval flag = true\nval ch = 'a'\nval text = \"hello\""

      assertInferExprTypeWithSetup(setup, "x as bool", "bool")
      assertInferExprTypeWithSetup(setup, "x as char", "char")
      assertInferExprTypeWithSetup(setup, "x as string", "string")

      assertInferExprTypeWithSetup(setup, "flag as int", "int")
      assertInferExprTypeWithSetup(setup, "flag as string", "string")

      assertInferExprTypeWithSetup(setup, "ch as int", "int")
      assertInferExprTypeWithSetup(setup, "ch as string", "string")

      assertInferExprTypeWithSetup(setup, "text as any", "any")
    }

    it("should check variable casts") {
      val setup =
        "val x = 42\nval flag = true\nval ch = 'a'\nval text = \"hello\""

      assertCheckExprTypeWithSetup(setup, "x as bool", "bool")
      assertCheckExprTypeWithSetup(setup, "x as char", "char")
      assertCheckExprTypeWithSetup(setup, "x as string", "string")

      assertCheckExprTypeWithSetup(setup, "flag as int", "int")
      assertCheckExprTypeWithSetup(setup, "flag as string", "string")

      assertCheckExprTypeWithSetup(setup, "ch as int", "int")
      assertCheckExprTypeWithSetup(setup, "ch as string", "string")

      assertCheckExprTypeWithSetup(setup, "text as any", "any")
    }

    it("should infer cast to any type") {
      assertInferExprType("42 as any", "any")
      assertInferExprType("true as any", "any")
      assertInferExprType("\"hello\" as any", "any")
      assertInferExprType("'a' as any", "any")
      assertInferExprType("() as any", "any")
    }

    it("should check cast to any type") {
      assertCheckExprType("42 as any", "any")
      assertCheckExprType("true as any", "any")
      assertCheckExprType("\"hello\" as any", "any")
      assertCheckExprType("'a' as any", "any")
      assertCheckExprType("() as any", "any")
    }

    it("should infer cast from any type") {
      val setup = "val obj: any = 42"

      assertInferExprTypeWithSetup(setup, "obj as int", "int")
      assertInferExprTypeWithSetup(setup, "obj as bool", "bool")
      assertInferExprTypeWithSetup(setup, "obj as char", "char")
      assertInferExprTypeWithSetup(setup, "obj as string", "string")
      assertInferExprTypeWithSetup(setup, "obj as unit", "unit")
    }

    it("should check cast from any type") {
      val setup = "val obj: any = 42"

      assertCheckExprTypeWithSetup(setup, "obj as int", "int")
      assertCheckExprTypeWithSetup(setup, "obj as bool", "bool")
      assertCheckExprTypeWithSetup(setup, "obj as char", "char")
      assertCheckExprTypeWithSetup(setup, "obj as string", "string")
      assertCheckExprTypeWithSetup(setup, "obj as unit", "unit")
    }

    it("should infer expression casts") {
      // Cast results of binary operations
      assertInferExprType("(1 + 2) as bool", "bool")
      assertInferExprType("(true && false) as int", "int")
      assertInferExprType("(1 == 2) as string", "string")

      // Cast results of unary operations
      assertInferExprType("(!true) as int", "int")
      assertInferExprType("(-42) as bool", "bool")
    }

    it("should check expression casts") {
      // Cast results of binary operations
      assertCheckExprType("(1 + 2) as bool", "bool")
      assertCheckExprType("(true && false) as int", "int")
      assertCheckExprType("(1 == 2) as string", "string")

      // Cast results of unary operations
      assertCheckExprType("(!true) as int", "int")
      assertCheckExprType("(-42) as bool", "bool")
    }

    it("should infer cast with parentheses") {
      assertInferExprType("(42) as string", "string")
      assertInferExprType("(true) as int", "int")
      assertInferExprType("(\"hello\") as any", "any")
    }

    it("should check cast with parentheses") {
      assertCheckExprType("(42) as string", "string")
      assertCheckExprType("(true) as int", "int")
      assertCheckExprType("(\"hello\") as any", "any")
    }

    it("should infer chained operations with casts") {
      val setup = "val x = 42"

      // Cast should have appropriate precedence
      assertInferExprTypeWithSetup(setup, "x as bool == true", "bool")
      assertInferExprTypeWithSetup(setup, "(x as bool) == true", "bool")
    }

    it("should check chained operations with casts") {
      val setup = "val x = 42"

      // Cast should have appropriate precedence
      assertCheckExprTypeWithSetup(setup, "x as bool == true", "bool")
      assertCheckExprTypeWithSetup(setup, "(x as bool) == true", "bool")
    }

    it("should infer basic string conversions") {
      // Convert literals to string
      assertInferExprType("string(42)", "string")
      assertInferExprType("string(0)", "string")
      assertInferExprType("string(-123)", "string")
      assertInferExprType("string(true)", "string")
      assertInferExprType("string(false)", "string")
      assertInferExprType("string('a')", "string")
      assertInferExprType("string('z')", "string")
      assertInferExprType("string(())", "string")
    }

    it("should check basic string conversions") {
      // Convert literals to string
      assertCheckExprType("string(42)", "string")
      assertCheckExprType("string(0)", "string")
      assertCheckExprType("string(-123)", "string")
      assertCheckExprType("string(true)", "string")
      assertCheckExprType("string(false)", "string")
      assertCheckExprType("string('a')", "string")
      assertCheckExprType("string('z')", "string")
      assertCheckExprType("string(())", "string")
    }

    it("should infer string conversions with variables") {
      val intSetup = "val num = 123"
      assertInferExprTypeWithSetup(intSetup, "string(num)", "string")

      val boolSetup = "val flag = true"
      assertInferExprTypeWithSetup(boolSetup, "string(flag)", "string")

      val charSetup = "val ch = 'A'"
      assertInferExprTypeWithSetup(charSetup, "string(ch)", "string")

      val unitSetup = "val nothing = ()"
      assertInferExprTypeWithSetup(unitSetup, "string(nothing)", "string")
    }

    it("should check string conversions with variables") {
      val intSetup = "val num = 123"
      assertCheckExprTypeWithSetup(intSetup, "string(num)", "string")

      val boolSetup = "val flag = true"
      assertCheckExprTypeWithSetup(boolSetup, "string(flag)", "string")

      val charSetup = "val ch = 'A'"
      assertCheckExprTypeWithSetup(charSetup, "string(ch)", "string")

      val unitSetup = "val nothing = ()"
      assertCheckExprTypeWithSetup(unitSetup, "string(nothing)", "string")
    }

    it("should infer string conversions with expressions") {
      // Arithmetic expressions
      assertInferExprType("string(1 + 2)", "string")
      assertInferExprType("string(10 - 5)", "string")
      assertInferExprType("string(3 * 4)", "string")
      assertInferExprType("string(15 / 3)", "string")
      assertInferExprType("string(17 % 5)", "string")

      // Boolean expressions
      assertInferExprType("string(true && false)", "string")
      assertInferExprType("string(true || false)", "string")
      assertInferExprType("string(!true)", "string")
      assertInferExprType("string(5 > 3)", "string")
      assertInferExprType("string(5 == 5)", "string")
      assertInferExprType("string(5 != 3)", "string")

      // Unary expressions
      assertInferExprType("string(-42)", "string")
      assertInferExprType("string(+42)", "string")
    }

    it("should check string conversions with expressions") {
      // Arithmetic expressions
      assertCheckExprType("string(1 + 2)", "string")
      assertCheckExprType("string(10 - 5)", "string")
      assertCheckExprType("string(3 * 4)", "string")
      assertCheckExprType("string(15 / 3)", "string")
      assertCheckExprType("string(17 % 5)", "string")

      // Boolean expressions
      assertCheckExprType("string(true && false)", "string")
      assertCheckExprType("string(true || false)", "string")
      assertCheckExprType("string(!true)", "string")
      assertCheckExprType("string(5 > 3)", "string")
      assertCheckExprType("string(5 == 5)", "string")
      assertCheckExprType("string(5 != 3)", "string")

      // Unary expressions
      assertCheckExprType("string(-42)", "string")
      assertCheckExprType("string(+42)", "string")
    }

    it("should infer string conversions in complex expressions") {
      val setup = "val x = 42\nval y = true"

      // String conversions in comparisons
      assertInferExprTypeWithSetup(setup, "string(x) == \"42\"", "bool")
      assertInferExprTypeWithSetup(setup, "string(y) == \"true\"", "bool")
      assertInferExprTypeWithSetup(setup, "string(x) != \"0\"", "bool")

      // String conversions in arithmetic context
      assertInferExprTypeWithSetup(setup, "string(x + 10)", "string")
      assertInferExprTypeWithSetup(setup, "string(x * 2)", "string")
    }

    it("should check string conversions in complex expressions") {
      val setup = "val x = 42\nval y = true"

      // String conversions in comparisons
      assertCheckExprTypeWithSetup(setup, "string(x) == \"42\"", "bool")
      assertCheckExprTypeWithSetup(setup, "string(y) == \"true\"", "bool")
      assertCheckExprTypeWithSetup(setup, "string(x) != \"0\"", "bool")

      // String conversions in arithmetic context
      assertCheckExprTypeWithSetup(setup, "string(x + 10)", "string")
      assertCheckExprTypeWithSetup(setup, "string(x * 2)", "string")
    }

    it("should infer string conversions with method calls") {
      // Convert results of method calls to string
      assertInferExprType("string(println(\"test\"))", "string")
      assertInferExprType("string(print(42))", "string")
    }

    it("should check string conversions with method calls") {
      // Convert results of method calls to string
      assertCheckExprType("string(println(\"test\"))", "string")
      assertCheckExprType("string(print(42))", "string")
    }

    it("should infer string conversions with control flow") {
      // String conversions with if expressions
      assertInferExprType("string(if (true) 1 else 2)", "string")
      assertInferExprType("string(if (false) true else false)", "string")

      // String conversions with block expressions
      assertInferExprType("string({ 42 })", "string")
      assertInferExprType("string({ true })", "string")
    }

    it("should check string conversions with control flow") {
      // String conversions with if expressions
      assertCheckExprType("string(if (true) 1 else 2)", "string")
      assertCheckExprType("string(if (false) true else false)", "string")

      // String conversions with block expressions
      assertCheckExprType("string({ 42 })", "string")
      assertCheckExprType("string({ true })", "string")
    }

    it("should infer nested string conversions") {
      val setup = "val x = 42"

      // String conversion of string (should still work)
      assertInferExprType("string(\"hello\")", "string")
      assertInferExprTypeWithSetup(setup, "string(string(x))", "string")

      // String conversions in nested expressions
      assertInferExprTypeWithSetup(
        setup,
        "string(string(x) == \"42\")",
        "string"
      )
    }

    it("should check nested string conversions") {
      val setup = "val x = 42"

      // String conversion of string (should still work)
      assertCheckExprType("string(\"hello\")", "string")
      assertCheckExprTypeWithSetup(setup, "string(string(x))", "string")

      // String conversions in nested expressions
      assertCheckExprTypeWithSetup(
        setup,
        "string(string(x) == \"42\")",
        "string"
      )
    }

    it("should infer int conversions") {
      // Convert various types to int
      assertInferExprType("int(42)", "int")
      assertInferExprType("int(true)", "int")
      assertInferExprType("int(false)", "int")
      assertInferExprType("int('a')", "int")

      // Int conversions with variables and expressions
      val setup = "val flag = true\nval ch = 'A'"
      assertInferExprTypeWithSetup(setup, "int(flag)", "int")
      assertInferExprTypeWithSetup(setup, "int(ch)", "int")
      assertInferExprType("int(1 + 2)", "int")
    }

    it("should check int conversions") {
      // Convert various types to int
      assertCheckExprType("int(42)", "int")
      assertCheckExprType("int(true)", "int")
      assertCheckExprType("int(false)", "int")
      assertCheckExprType("int('a')", "int")

      // Int conversions with variables and expressions
      val setup = "val flag = true\nval ch = 'A'"
      assertCheckExprTypeWithSetup(setup, "int(flag)", "int")
      assertCheckExprTypeWithSetup(setup, "int(ch)", "int")
      assertCheckExprType("int(1 + 2)", "int")
    }

    it("should infer bool conversions") {
      // Convert various types to bool
      assertInferExprType("bool(true)", "bool")
      assertInferExprType("bool(false)", "bool")
      assertInferExprType("bool(42)", "bool")
      assertInferExprType("bool(0)", "bool")

      // Bool conversions with variables and expressions
      val setup = "val num = 123"
      assertInferExprTypeWithSetup(setup, "bool(num)", "bool")
      assertInferExprType("bool(5 > 3)", "bool")
    }

    it("should check bool conversions") {
      // Convert various types to bool
      assertCheckExprType("bool(true)", "bool")
      assertCheckExprType("bool(false)", "bool")
      assertCheckExprType("bool(42)", "bool")
      assertCheckExprType("bool(0)", "bool")

      // Bool conversions with variables and expressions
      val setup = "val num = 123"
      assertCheckExprTypeWithSetup(setup, "bool(num)", "bool")
      assertCheckExprType("bool(5 > 3)", "bool")
    }

    it("should infer char conversions") {
      // Convert various types to char
      assertInferExprType("char('a')", "char")
      assertInferExprType("char(65)", "char")

      // Char conversions with variables
      val setup = "val ascii = 97"
      assertInferExprTypeWithSetup(setup, "char(ascii)", "char")
    }

    it("should check char conversions") {
      // Convert various types to char
      assertCheckExprType("char('a')", "char")
      assertCheckExprType("char(65)", "char")

      // Char conversions with variables
      val setup = "val ascii = 97"
      assertCheckExprTypeWithSetup(setup, "char(ascii)", "char")
    }
  }
}
