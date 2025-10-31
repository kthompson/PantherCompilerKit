import TestHelpers._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class VmTests extends AnyFunSpec with Matchers {

  describe("VM") {
    it("should execute constants") {
      assertExecValueInt("12", 12)
      assertExecValueInt("0", 0)
      assertExecValueInt("-5", -5)
      assertExecValueBool("true", true)
      assertExecValueBool("false", false)
      assertExecValueString("\"hello\"", "hello")
    }

    it("should execute unary operations") {
      assertExecValueInt("-12", -12)
      assertExecValueInt("+12", 12)
      assertExecValueBool("!true", false)
      assertExecValueBool("!false", true)
    }

    it("should execute binary operations") {
      assertExecValueInt("1 + 2", 3)
      assertExecValueInt("5 - 3", 2)
      assertExecValueInt("4 * 3", 12)
      assertExecValueInt("15 / 3", 5)
      assertExecValueInt("17 % 5", 2)
      assertExecValueInt("17 & 5", 1)
      assertExecValueInt("17 | 5", 21)

      assertExecValueBool("5 == 5", true)
      assertExecValueBool("5 == 3", false)
      assertExecValueBool("5 != 3", true)
      assertExecValueBool("5 != 5", false)
      assertExecValueBool("5 < 10", true)
      assertExecValueBool("10 < 5", false)
      assertExecValueBool("5 <= 5", true)
      assertExecValueBool("5 <= 3", false)
      assertExecValueBool("10 > 5", true)
      assertExecValueBool("5 > 10", false)
      assertExecValueBool("5 >= 5", true)
      assertExecValueBool("3 >= 5", false)

      assertExecValueBool("true && true", true)
      assertExecValueBool("true && false", false)
      assertExecValueBool("false && true", false)
      assertExecValueBool("false && false", false)
      assertExecValueBool("true || true", true)
      assertExecValueBool("true || false", true)
      assertExecValueBool("false || true", true)
      assertExecValueBool("false || false", false)
    }

    it("should handle binary precedence correctly") {
      assertExecValueInt("1 + 2 * 3", 7) // 1 + (2 * 3)
      assertExecValueInt("2 * 3 + 1", 7) // (2 * 3) + 1
      assertExecValueInt("10 - 4 / 2", 8) // 10 - (4 / 2)
    }

    it("should handle unary precedence correctly") {
      assertExecValueInt("-2 + 3", 1) // (-2) + 3
      assertExecValueInt("-(2 + 3)", -5) // -(2 + 3)
    }

    it("should execute local variable declarations and usage") {
      assertExecValueIntWithSetup("val x = 12", "x", 12)
      assertExecValueIntWithSetup("val x = 12", "x + 7", 19)
      assertExecValueIntWithSetup("val x = 12", "7 + x", 19)
      assertExecValueIntWithSetup("val x = 12\nval y = x + 1", "x", 12)
      assertExecValueIntWithSetup("val x = 12\nval y = x + 1", "y", 13)
    }

    it("should execute if expressions") {
      assertExecValueInt("if (true) 1 else 2", 1)
      assertExecValueInt("if (false) 1 else 2", 2)
      assertExecValueInt("if (5 > 3) 10 else 20", 10)
      assertExecValueInt("if (3 > 5) 10 else 20", 20)
    }

    it("should execute function calls") {
      assertExecValueIntWithSetup(
        "def noargs(): int = 7",
        "noargs()",
        7
      )
      assertExecValueIntWithSetup(
        "def onearg(x: int): int = x + 1",
        "onearg(12)",
        13
      )
      assertExecValueIntWithSetup(
        "def twoargs(x: int, y: int): int = x + y",
        "twoargs(12, 13)",
        25
      )
      assertExecValueIntWithSetup(
        "def threeargs(x: int, y: int, z: int): int = x + y + z",
        "threeargs(12, 13, 14)",
        39
      )
    }

    it("should execute object methods") {
      val setup = "object TestObject {\n" +
        "  def testMethod(x: int): int = x + 1\n" +
        "  def testMethod2(x: int, y: int): int = x + y\n" +
        "  def noArgs(): int = 42\n" +
        "  def threeArgs(a: int, b: int, c: int): int = a * b + c\n" +
        "  def multiply(x: int, y: int): int = x * y\n" +
        "  def subtract(x: int, y: int): int = x - y\n" +
        "  def negate(x: int): int = -x\n" +
        "}\n"

      assertExecValueIntWithSetup(setup, "TestObject.testMethod(0)", 1)
      assertExecValueIntWithSetup(setup, "TestObject.testMethod(11)", 12)
      assertExecValueIntWithSetup(setup, "TestObject.testMethod(12)", 13)
      assertExecValueIntWithSetup(setup, "TestObject.testMethod2(11, 12)", 23)
      assertExecValueIntWithSetup(setup, "TestObject.testMethod2(12, 13)", 25)

      // No arguments
      assertExecValueIntWithSetup(setup, "TestObject.noArgs()", 42)

      // Three arguments
      assertExecValueIntWithSetup(
        setup,
        "TestObject.threeArgs(2, 3, 4)",
        10
      ) // 2 * 3 + 4
      assertExecValueIntWithSetup(
        setup,
        "TestObject.threeArgs(5, 6, 7)",
        37
      ) // 5 * 6 + 7
      assertExecValueIntWithSetup(
        setup,
        "TestObject.threeArgs(0, 10, 5)",
        5
      ) // 0 * 10 + 5

      // More arithmetic operations
      assertExecValueIntWithSetup(setup, "TestObject.multiply(7, 8)", 56)
      assertExecValueIntWithSetup(setup, "TestObject.multiply(-3, 4)", -12)
      assertExecValueIntWithSetup(setup, "TestObject.subtract(10, 3)", 7)
      assertExecValueIntWithSetup(setup, "TestObject.subtract(5, 8)", -3)
      assertExecValueIntWithSetup(setup, "TestObject.negate(42)", -42)
      assertExecValueIntWithSetup(setup, "TestObject.negate(-15)", 15)

      // Nested calls
      assertExecValueIntWithSetup(
        setup,
        "TestObject.testMethod(TestObject.noArgs())",
        43
      ) // testMethod(42) = 43
      assertExecValueIntWithSetup(
        setup,
        "TestObject.multiply(TestObject.testMethod(2), 5)",
        15
      ) // multiply(3, 5) = 15
    }

    it("should access object fields") {
      val setup = "object Taco {\n" +
        "  val field1 = 12\n" +
        "  val field2 = 13\n" +
        "}\n" +
        "object Calculator {\n" +
        "  val zero = 0\n" +
        "  val one = 1\n" +
        "  val negativeValue = -42\n" +
        "  val largeValue = 9999\n" +
        "}\n" +
        "object MathConstants {\n" +
        "  val pi = 3\n" + // Simplified for int
        "  val e = 2\n" + // Simplified for int
        "}\n"

      assertExecValueIntWithSetup(setup, "Taco.field1", 12)
      assertExecValueIntWithSetup(setup, "Taco.field2", 13)

      // Calculator object tests
      assertExecValueIntWithSetup(setup, "Calculator.zero", 0)
      assertExecValueIntWithSetup(setup, "Calculator.one", 1)
      assertExecValueIntWithSetup(setup, "Calculator.negativeValue", -42)
      assertExecValueIntWithSetup(setup, "Calculator.largeValue", 9999)

      // MathConstants tests
      assertExecValueIntWithSetup(setup, "MathConstants.pi", 3)
      assertExecValueIntWithSetup(setup, "MathConstants.e", 2)

      // Field access in expressions
      assertExecValueIntWithSetup(
        setup,
        "Taco.field1 + Taco.field2",
        25
      ) // 12 + 13
      assertExecValueIntWithSetup(
        setup,
        "Calculator.zero + Calculator.one",
        1
      ) // 0 + 1
      assertExecValueIntWithSetup(
        setup,
        "MathConstants.pi * MathConstants.e",
        6
      ) // 3 * 2
      assertExecValueIntWithSetup(
        setup,
        "Taco.field1 * Calculator.one",
        12
      ) // 12 * 1
      assertExecValueIntWithSetup(
        setup,
        "Calculator.largeValue - Taco.field1",
        9987
      ) // 9999 - 12

      // More complex expressions
      assertExecValueIntWithSetup(
        setup,
        "Taco.field1 + Taco.field2 * Calculator.one",
        25
      ) // 12 + 13 * 1
      assertExecValueIntWithSetup(
        setup,
        "(Taco.field1 + Taco.field2) * MathConstants.e",
        50
      ) // (12 + 13) * 2
      assertExecValueIntWithSetup(
        setup,
        "Calculator.largeValue + Calculator.negativeValue",
        9957
      ) // 9999 + (-42)

      // Test with unary operations
      assertExecValueIntWithSetup(setup, "-Taco.field1", -12)
      assertExecValueIntWithSetup(setup, "-Calculator.negativeValue", 42)
    }

    it("should execute classes without args") {
      assertExecValueIntWithSetup(
        "class Foo() {\n" +
          " def bar() = 12\n" +
          "}",
        "new Foo().bar()",
        12
      )
    }

    //    it should "execute classes with args" in {
    //      assertExecValueIntWithSetup(
    //        "class Foo(x: int, y: int) {\n" +
    //          " def add() = x + y\n" +
    //          "}",
    //        "new Foo(12, 13).add()",
    //        25
    //      )
    //    }
    //
    //    it should "access class fields via constructor" in {
    //      assertExecValueIntWithSetup(
    //        "class Foo(x: int, y: int)",
    //        "new Foo(1, 2).x + new Foo(3,5).y",
    //        6
    //      )
    //    }

    it("should access class fields via field declaration") {
      assertExecValueIntWithSetup(
        "class Foo() {\n" +
          "  var x = 12\n" +
          "  var y = 13\n" +
          "}",
        "new Foo().x + new Foo().y",
        25
      )
    }

    it("should execute pattern matching with integer cases") {
      assertExecValueIntWithSetup(
        "val x = 1",
        "x match {\n  case 1 => 10\n  case 2 => 20\n  case _ => 0\n}",
        10
      )
      assertExecValueIntWithSetup(
        "val x = 2",
        "x match {\n  case 1 => 10\n  case 2 => 20\n  case _ => 0\n}",
        20
      )
      assertExecValueIntWithSetup(
        "val x = 5",
        "x match {\n  case 1 => 10\n  case 2 => 20\n  case _ => 0\n}",
        0
      )
    }

    it("should execute pattern matching with wildcard only") {
      assertExecValueIntWithSetup(
        "val x = 42",
        "x match {\n  case _ => 99\n}",
        99
      )
    }

    it("should execute is expression basic functionality") {
      // Test that is expressions execute properly and return correct boolean values
      assertExecValueBool("12 is int", true)
      assertExecValueBool("12 is bool", false)
      assertExecValueBool("true is bool", true)
      assertExecValueBool("true is int", false)
      assertExecValueBool("\"hello\" is string", true)
      assertExecValueBool("\"hello\" is int", false)

      // Test with variables
      assertExecValueIntWithSetup("val x = 12", "if (x is int) 1 else 0", 1)
      assertExecValueIntWithSetup("val x = 12", "if (x is bool) 1 else 0", 0)
      assertExecValueIntWithSetup("val y = true", "if (y is bool) 1 else 0", 1)
      assertExecValueIntWithSetup(
        "val y = true",
        "if (y is string) 1 else 0",
        0
      )
    }

    it("should execute cast expressions - identity casts") {
      // Test identity casts (casting to the same type)
      assertExecValueInt("12 as int", 12)
      assertExecValueBool("true as bool", true)
      assertExecValueBool("false as bool", false)
      assertExecValueString("\"hello\" as string", "hello")

      // Test with variables
      assertExecValueIntWithSetup("val x = 42", "x as int", 42)
      assertExecValueBoolWithSetup("val flag = true", "flag as bool", true)
      assertExecValueStringWithSetup(
        "val text = \"world\"",
        "text as string",
        "world"
      )
    }

    it("should execute cast expressions - valid conversions") {
      // Test casts that should succeed at runtime
      // Skip char literals for now since emitCharacterLiteral is not implemented
      assertExecValueString("42 as string", "42") // int to string
      assertExecValueString("true as string", "true") // bool to string
      assertExecValueString("false as string", "false") // bool to string

      // Test with expressions
      assertExecValueInt("(1 + 2) as int", 3)
      assertExecValueString("(5 * 6) as string", "30")
      assertExecValueBool("(10 > 5) as bool", true)
    }

    //    it should "execute cast expressions - cast to any type" in {
    //      // Test casting various types to 'any' type
    //      // Note: The actual value should remain the same, just the type changes
    //      assertExecValueInt("42 as any as int", 42)
    //      assertExecValueBool("true as any as bool", true)
    //      assertExecValueString("\"test\" as any as string", "test")
    //
    //      // Test with variables
    //      assertExecValueIntWithSetup("val x = 100", "x as any as int", 100)
    //      assertExecValueBoolWithSetup("val flag = false", "flag as any as bool", false)
    //    }
    //
    //    it should "execute cast expressions - cast from any type" in {
    //      // Test casting from 'any' type to specific types
    //      val anySetup = "val obj: any = 42"
    //      assertExecValueIntWithSetup(anySetup, "obj as int", 42)
    //
    //      val anyBoolSetup = "val obj: any = true"
    //      assertExecValueBoolWithSetup(anyBoolSetup, "obj as bool", true)
    //
    //      val anyStringSetup = "val obj: any = \"hello\""
    //      assertExecValueStringWithSetup(anyStringSetup, "obj as string", "hello")
    //    }
    //
    //    it should "execute cast expressions - chained casts" in {
    //      // Test multiple casts in sequence
    //      assertExecValueInt("42 as any as int", 42)
    //      assertExecValueString("42 as string as any as string", "42")
    //      assertExecValueBool("true as any as bool as any as bool", true)
    //
    //      // Test with expressions in between
    //      assertExecValueInt("(20 + 22) as any as int", 42)
    //      assertExecValueString("(\"hel\" + \"lo\") as any as string", "hello")
    //    }

    it("should execute cast expressions - cast with operators") {
      // Test that cast has correct precedence with other operators
      assertExecValueBool("42 as int == 42", true)
      assertExecValueBool("42 as string == \"42\"", true)
      assertExecValueBool("true as bool && false", false)
      assertExecValueBool("false as bool || true", true)

      // Test cast in arithmetic expressions
      assertExecValueInt("(40 + 2) as int", 42)
      assertExecValueInt("40 + 2 as int", 42) // Should be: 40 + (2 as int)
    }

    it("should execute cast expressions - cast in control flow") {
      // Test cast expressions in if conditions
      assertExecValueInt("if (42 as int == 42) 1 else 0", 1)
      assertExecValueInt("if (true as bool) 100 else 200", 100)
      assertExecValueInt("if (false as bool) 100 else 200", 200)

      // Test cast in if branches
      assertExecValueInt("if (true) 42 as int else 0", 42)
      assertExecValueString("if (false) \"no\" else \"yes\" as string", "yes")

      // Test with variables
      assertExecValueIntWithSetup(
        "val x = 50",
        "if (x as int > 40) 1 else 0",
        1
      )
    }

    it("should execute array indexing - basic access") {
      // Test basic array indexing with literal indices
      val setup = "val array = new Array[int](3)"
      // Note: These tests assume array elements are initialized to 0
      // If that's not the case, we may need to modify the setup
      assertExecValueIntWithSetup(setup, "array(0)", 0)
      assertExecValueIntWithSetup(setup, "array(1)", 0)
      assertExecValueIntWithSetup(setup, "array(2)", 0)
    }

    it("should execute array indexing - with assignment") {
      // Test the specific "NewExpression in bindLHS" fix
      val setup = "var array = new Array[int](5)"
      // First test assignment returns unit
      assertExecValueIntWithSetup(
        setup + "\narray(0) = 42",
        "0",
        0
      ) // dummy assertion to ensure compilation

      // Test assignment followed by access
      val assignAndRead = setup + "\narray(0) = 42\narray(1) = 13"
      assertExecValueIntWithSetup(assignAndRead, "array(0)", 42)
      assertExecValueIntWithSetup(assignAndRead, "array(1)", 13)
    }

    it("should execute array indexing - computed indices") {
      val setup = "var array = new Array[int](10)\nval i = 5\narray(i) = 99"
      assertExecValueIntWithSetup(setup, "array(i)", 99)
      assertExecValueIntWithSetup(setup, "array(5)", 99)

      // Test with arithmetic expressions as indices
      val complexSetup = setup + "\narray(2 * 3) = 77\narray(1 + 4) = 55"
      assertExecValueIntWithSetup(complexSetup, "array(6)", 77)
      assertExecValueIntWithSetup(
        complexSetup,
        "array(5)",
        55
      ) // This overwrites the previous value at index 5
    }

    it("should execute array indexing - in expressions") {
      val setup =
        "var array = new Array[int](5)\narray(0) = 10\narray(1) = 20\narray(2) = 30"

      // Test array indexing in arithmetic expressions
      assertExecValueIntWithSetup(setup, "array(0) + array(1)", 30)
      assertExecValueIntWithSetup(setup, "array(2) - array(0)", 20)
      assertExecValueIntWithSetup(setup, "array(1) * 2", 40)

      // Test array indexing in boolean expressions
      assertExecValueBoolWithSetup(setup, "array(0) == 10", true)
      assertExecValueBoolWithSetup(setup, "array(1) > array(0)", true)
      assertExecValueBoolWithSetup(setup, "array(2) < array(1)", false)
    }

    it("should execute array indexing - different types") {
      // Test array indexing with different element types
      val boolSetup =
        "var boolArray = new Array[bool](3)\nboolArray(0) = true\nboolArray(1) = false"
      assertExecValueBoolWithSetup(boolSetup, "boolArray(0)", true)
      assertExecValueBoolWithSetup(boolSetup, "boolArray(1)", false)

      val stringSetup =
        "var stringArray = new Array[string](2)\nstringArray(0) = \"hello\"\nstringArray(1) = \"world\""
      assertExecValueStringWithSetup(stringSetup, "stringArray(0)", "hello")
      assertExecValueStringWithSetup(stringSetup, "stringArray(1)", "world")
    }

    it("should execute string conversions - basic literals") {
      // Test string conversion of basic literals
      assertExecValueString("string(42)", "42")
      assertExecValueString("string(0)", "0")
      assertExecValueString("string(-123)", "-123")
      assertExecValueString("string(true)", "true")
      assertExecValueString("string(false)", "false")

      // String conversion of string (identity)
      assertExecValueString("string(\"hello\")", "hello")
      assertExecValueString("string(\"\")", "")
    }

    it("should execute string conversions - with variables") {
      // Test string conversion of variables
      assertExecValueStringWithSetup("val num = 123", "string(num)", "123")
      assertExecValueStringWithSetup("val flag = true", "string(flag)", "true")
      assertExecValueStringWithSetup(
        "val flag2 = false",
        "string(flag2)",
        "false"
      )
      assertExecValueStringWithSetup(
        "val text = \"world\"",
        "string(text)",
        "world"
      )

      // Negative numbers
      assertExecValueStringWithSetup("val neg = -42", "string(neg)", "-42")
    }

    it("should execute string conversions - with expressions") {
      // Test string conversion of arithmetic expressions
      assertExecValueString("string(1 + 2)", "3")
      assertExecValueString("string(10 - 5)", "5")
      assertExecValueString("string(3 * 4)", "12")
      assertExecValueString("string(15 / 3)", "5")
      assertExecValueString("string(17 % 5)", "2")

      // Test string conversion of boolean expressions
      assertExecValueString("string(true && false)", "false")
      assertExecValueString("string(true || false)", "true")
      assertExecValueString("string(!true)", "false")
      assertExecValueString("string(!false)", "true")
      assertExecValueString("string(5 > 3)", "true")
      assertExecValueString("string(5 == 5)", "true")
      assertExecValueString("string(5 != 3)", "true")
      assertExecValueString("string(5 < 3)", "false")

      // Test string conversion of unary expressions
      assertExecValueString("string(-42)", "-42")
      assertExecValueString("string(+42)", "42")
    }

    it("should execute string conversions - complex expressions") {
      val setup = "val x = 42\nval y = true"

      // String conversions in comparisons
      assertExecValueBoolWithSetup(setup, "string(x) == \"42\"", true)
      assertExecValueBoolWithSetup(setup, "string(y) == \"true\"", true)
      assertExecValueBoolWithSetup(setup, "string(x) != \"0\"", true)
      assertExecValueBoolWithSetup(setup, "string(y) != \"false\"", true)

      // String conversions of computed values
      assertExecValueStringWithSetup(setup, "string(x + 10)", "52")
      assertExecValueStringWithSetup(setup, "string(x * 2)", "84")
      assertExecValueStringWithSetup(setup, "string(x - 10)", "32")
    }

    it("should execute string conversions - with control flow") {
      // String conversions with if expressions
      assertExecValueString("string(if (true) 1 else 2)", "1")
      assertExecValueString("string(if (false) 1 else 2)", "2")
      assertExecValueString("string(if (5 > 3) 100 else 200)", "100")
      assertExecValueString("string(if (3 > 5) true else false)", "false")

      // String conversions with block expressions
      assertExecValueString("string({ 42 })", "42")
      assertExecValueString("string({ true })", "true")
      assertExecValueString(
        "string({ val temp = 10\n temp * 2 })",
        "20"
      )
    }

    it("should execute string conversions - nested and chained") {
      val setup = "val x = 42"

      // Nested string conversions
      assertExecValueStringWithSetup(setup, "string(string(x))", "42")
      assertExecValueString("string(string(\"hello\"))", "hello")

      // String conversions in nested expressions
      assertExecValueStringWithSetup(
        setup,
        "string(string(x) == \"42\")",
        "true"
      )
      assertExecValueStringWithSetup(
        setup,
        "string(string(x + 8) == \"50\")",
        "true"
      )
    }

    it("should execute string conversions - edge cases") {
      // Large numbers
      assertExecValueString("string(999999)", "999999")
      assertExecValueString("string(-999999)", "-999999")

      // Complex arithmetic
      assertExecValueString("string((10 + 5) * (20 / 4))", "75")
      assertExecValueString("string(100 - 50 + 25)", "75")

      // Complex boolean logic
      assertExecValueString("string((5 > 3) && (10 < 20))", "true")
      assertExecValueString("string((5 < 3) || (10 > 20))", "false")
    }

    it("should execute int conversions - basic functionality") {
      // Test int conversion of basic literals
      assertExecValueInt("int(42)", 42)
      assertExecValueInt("int(0)", 0)
      assertExecValueInt("int(-123)", -123)
      assertExecValueInt("int(true)", 1)
      assertExecValueInt("int(false)", 0)

      // Int conversion with variables
      assertExecValueIntWithSetup("val flag = true", "int(flag)", 1)
      assertExecValueIntWithSetup("val flag2 = false", "int(flag2)", 0)
      assertExecValueIntWithSetup("val num = 123", "int(num)", 123)
    }

    it("should execute int conversions - string cases") {
      assertExecValueInt("int(\"42\")", 42)
      assertExecValueInt("int(\"0\")", 0)
      assertExecValueInt("int(\"-123\")", -123)
      assertExecValueIntWithSetup("val str = \"56\"", "int(str)", 56)
      assertExecValueIntWithSetup("val str = \"-78\"", "int(str)", -78)
      // Edge cases
      assertExecValueInt("int(\"00123\")", 123)
      assertExecValueInt("int(\"-00123\")", -123)
      assertExecValueInt("int(\"2147483647\")", 2147483647) // Max int
      assertExecValueInt("int(\"-2147483648\")", -2147483648) // Min int
    }

    it("should execute bool conversions - basic functionality") {
      // Test bool conversion of basic literals
      assertExecValueBool("bool(true)", true)
      assertExecValueBool("bool(false)", false)
      assertExecValueBool("bool(42)", true)
      assertExecValueBool("bool(0)", false)
      assertExecValueBool("bool(-5)", true)

      // Bool conversion with variables
      assertExecValueBoolWithSetup("val num = 123", "bool(num)", true)
      assertExecValueBoolWithSetup("val zero = 0", "bool(zero)", false)
      assertExecValueBoolWithSetup("val flag = true", "bool(flag)", true)
    }

    it("should execute conversion edge cases") {
      // Test conversion of expressions
      assertExecValueString("string(1 + 2 * 3)", "7")
      assertExecValueInt("int(5 > 3)", 1)
      assertExecValueInt("int(3 > 5)", 0)
      assertExecValueBool("bool(10 - 10)", false)
      assertExecValueBool("bool(10 - 9)", true)

      // Test nested conversions
      assertExecValueString("string(int(true))", "1")
      assertExecValueString("string(int(false))", "0")
      assertExecValueInt("int(bool(42))", 1)
      assertExecValueInt("int(bool(0))", 0)
    }

    it("should execute conversions with complex expressions") {
      val setup = "val x = 42\nval y = true"

      // Complex conversions
      assertExecValueStringWithSetup(setup, "string(int(y) + x)", "43")
      assertExecValueIntWithSetup(setup, "int(string(x) == \"42\")", 1)
      assertExecValueBoolWithSetup(setup, "bool(int(y) * x)", true)

      // Conversions in control flow
      assertExecValueStringWithSetup(
        setup,
        "string(if (bool(x)) int(y) else 0)",
        "1"
      )
    }
  }
}
