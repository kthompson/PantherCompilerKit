import TestHelpers._
import utest._

object VmTests extends TestSuite {
  val tests = Tests {
    test("constants") {
      assertExecValueInt("12", 12)
      assertExecValueInt("0", 0)
      assertExecValueInt("-5", -5)
      assertExecValueBool("true", true)
      assertExecValueBool("false", false)
      assertExecValueString("\"hello\"", "hello")
    }

    test("unary") {
      assertExecValueInt("-12", -12)
      assertExecValueInt("+12", 12)
      assertExecValueBool("!true", false)
      assertExecValueBool("!false", true)
    }

    test("binary") {
      assertExecValueInt("1 + 2", 3)
      assertExecValueInt("5 - 3", 2)
      assertExecValueInt("4 * 3", 12)
      assertExecValueInt("15 / 3", 5)
      assertExecValueInt("17 % 5", 2)

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

    test("binary precedence") {
      assertExecValueInt("1 + 2 * 3", 7) // 1 + (2 * 3)
      assertExecValueInt("2 * 3 + 1", 7) // (2 * 3) + 1
      assertExecValueInt("10 - 4 / 2", 8) // 10 - (4 / 2)
    }

    test("unary precedence") {
      assertExecValueInt("-2 + 3", 1) // (-2) + 3
      assertExecValueInt("-(2 + 3)", -5) // -(2 + 3)
    }

    test("locals") {
      assertExecValueIntWithSetup("val x = 12", "x", 12)
      assertExecValueIntWithSetup("val x = 12", "x + 7", 19)
      assertExecValueIntWithSetup("val x = 12", "7 + x", 19)
      assertExecValueIntWithSetup("val x = 12\nval y = x + 1", "x", 12)
      assertExecValueIntWithSetup("val x = 12\nval y = x + 1", "y", 13)
    }

    test("if expressions") {
      assertExecValueInt("if (true) 1 else 2", 1)
      assertExecValueInt("if (false) 1 else 2", 2)
      assertExecValueInt("if (5 > 3) 10 else 20", 10)
      assertExecValueInt("if (3 > 5) 10 else 20", 20)
    }

    test("calls") {
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

    test("object methods") {
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

    test("object fields") {
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

    // test("classes without args") {
    //   assertExecValueIntWithSetup(
    //    "class Foo() {\n" +
    //      " def bar() = 12\n" +
    //      "}",
    //    "new Foo().bar()",
    //    12
    //  )
    // }

    // test("classes with args") {
    //   assertExecValueIntWithSetup(
    //     "class Foo(x: int, y: int) {\n" +
    //       " def add() = x + y\n" +
    //       "}",
    //     "new Foo(12, 13).add()",
    //     25
    //   )
    // }

//    test("class fields via constructor") {
//      assertExecValueIntWithSetup(
//        "class Foo(x: int, y: int)",
//        "new Foo(1, 2).x + new Foo(3,5).y",
//        6
//      )
//    }

    test("class fields via field declaration") {
      assertExecValueIntWithSetup(
        "class Foo() {\n" +
          "  var x = 12\n" +
          "  var y = 13\n" +
          "}",
        "new Foo().x + new Foo().y",
        25
      )
    }
  }
}
