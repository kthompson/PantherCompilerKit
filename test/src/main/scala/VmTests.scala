import Helpers._
import panther._
import TestFramework._

object VmTests {
  def run(): unit = {
    suite("VM Tests")

    constants()
    unary()
    binary()
    binaryPrecedence()
    unaryPrecedence()
    locals()

    ifExpressions()

    calls()
    objectMethods()
    objectFields()
//    classes()

    // TODO: dup, swap, pop
    // TODO: arguments
    // TODO: call, ret
    // TODO: branch
    // TODO: conv
    // TODO: instance creation
    // TODO: instance method calls
    // TODO: instance field access
  }

  def objectMethods(): unit = {

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
//    assertExecValueIntWithSetup(setup, "TestObject.testMethod2(TestObject.multiply(2, 3), TestObject.testMethod(4))", 11)  // testMethod2(6, 5) = 11
  }

  def objectFields(): unit = {
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
    assertExecValueIntWithSetup(
      setup,
      "~Calculator.zero",
      -1
    ) // bitwise NOT of 0
  }

  def calls(): unit = {
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

  def ifExpressions(): unit = {
    val x = 12
    val setup = "val x = 12"
    assertExecValueIntWithSetup(
      setup,
      "if (x == 12) 1 else 2",
      if (x == 12) 1 else 2
    )
    assertExecValueIntWithSetup(
      setup,
      "if (x != 12) 1 else 2",
      if (x != 12) 1 else 2
    )
    assertExecValueIntWithSetup(
      setup,
      "if (x < 12) 1 else 2",
      if (x < 12) 1 else 2
    )
    assertExecValueIntWithSetup(
      setup,
      "if (x > 12) 1 else 2",
      if (x > 12) 1 else 2
    )
    assertExecValueIntWithSetup(
      setup,
      "if (x <= 12) 1 else 2",
      if (x <= 12) 1 else 2
    )
    assertExecValueIntWithSetup(
      setup,
      "if (x >= 12) 1 else 2",
      if (x >= 12) 1 else 2
    )
    assertExecValueIntWithSetup(
      setup,
      "if (x < -10) -1 else if (x > -20) -2 else -3",
      if (x < -10) -1 else if (x > -20) -2 else -3
    )
    assertExecValueIntWithSetup(
      setup,
      "if (x < -10) -1 else if (x > -20) -2 else -3 + x",
      if (x < -10) -1 else if (x > -20) -2 else -3 + x
    )
    assertExecValueIntWithSetup(
      setup,
      "if (x < -10) -1 else if (x > -20) -2 else -3 + x",
      if (x < -10) -1 else if (x > -20) -2 else -3 + x
    )
  }

  def classes(): unit = {
    classWithoutArgs()
    classWithArgs()
    classFields()
  }
  def classWithoutArgs(): Unit = {
    test("class without args")
    assertExecValueIntWithSetup(
      "class Foo() {\n" +
        " def bar() = 12\n" +
        "}",
      "new Foo().bar()",
      12
    )
  }

  def classWithArgs(): unit = {
    test("class with args")
    assertExecValueIntWithSetup(
      "class Foo(x: int, y: int) {\n" +
        " def bar() = x + y\n" +
        "}",
      "new Foo(12, 13).bar()",
      25
    )
  }

  def classFields(): unit = {
    test("class fields")
    assertExecValueIntWithSetup(
      "class Foo() {\n" +
        " val bar = 24\n" +
        "}",
      "new Foo().bar",
      24
    )
    assertExecValueIntWithSetup(
      "class Foo() {\n" +
        " var bar = 24\n" +
        "}",
      "val foo = new Foo()\n" +
        "foo.bar = 42\n" +
        "foo.bar",
      42
    )

  }

  def locals(): unit = {
    assertExecValueIntWithSetup("val x = 12", "x", 12)
    assertExecValueIntWithSetup("val x = 12", "x + 7", 19)
    assertExecValueIntWithSetup("val x = 12", "7 + x", 19)
    assertExecValueIntWithSetup("val x = 12\nval y = x + 1", "x", 12)
    assertExecValueIntWithSetup("val x = 12\nval y = x + 1", "y", 13)
  }

  def constants(): unit = {
    assertExecValueInt("1", 1)
    assertExecValueInt("0", 0)
    assertExecValueInt("178", 178)
    assertExecValueBool("true", true)
    assertExecValueBool("false", false)
    assertExecValueString("\"hello\"", "hello")
    assertExecValueString("\"hello world\"", "hello world")
    assertExecValueString("\"hello \\\"world\\\"\"", "hello \"world\"")
  }

  def unary(): unit = {
    assertExecValueInt("-(-1)", 1)
    assertExecValueInt("-1", -1)
    assertExecValueInt("~1", -2)
    assertExecValueInt("~0", -1)
    assertExecValueBool("!true", false)
    assertExecValueBool("!false", true)
  }

  def binary(): unit = {
    assertExecValueInt("1 & 2", 1 & 2)
    assertExecValueInt("1 & 1", 1 & 1)
    assertExecValueInt("1 & 0", 1 & 0)
    assertExecValueInt("23 & 42", 23 & 42)
    assertExecValueBool("true & true", true)
    assertExecValueBool("true & false", false)
    assertExecValueBool("false & true", false)

    assertExecValueInt("1 | 0", 1)
    assertExecValueInt("1 | 1", 1)
    assertExecValueInt("1 | 2", 3)
    assertExecValueBool("true | true", true)
    assertExecValueBool("true | false", true)
    assertExecValueBool("false | true", true)
    assertExecValueBool("false | false", false)

    assertExecValueBool("true || false", true)
    assertExecValueBool("false || true", true)
    assertExecValueBool("true || true", true)
    assertExecValueBool("false || false", false)

    assertExecValueBool("true && false", false)
    assertExecValueBool("false && true", false)
    assertExecValueBool("true && true", true)
    assertExecValueBool("false && false", false)

    assertExecValueInt("1 ^ 2", 3)
    assertExecValueInt("1 ^ 1", 0)
    assertExecValueInt("1 ^ 0", 1)
    assertExecValueInt("1 << 2", 4)

    assertExecValueInt("4 >> 2", 1)
    assertExecValueInt("-4 >> 2", -1)

    assertExecValueInt("1 + 2", 3)
    assertExecValueInt("22 + 24", 46)
    assertExecValueInt("3 - 2", 1)
    assertExecValueInt("2 - 3", -1)
    assertExecValueInt("2 * 3", 6)
    assertExecValueInt("3 / 2", 1)
    assertExecValueInt("3 % 2", 1)
    assertExecValueInt("2 & 3", 2)
    assertExecValueInt("-2 & 3", 2)

    assertExecValueBool("1 == 1", true)
    assertExecValueBool("1 == 2", false)
    assertExecValueBool("1 != 1", false)
    assertExecValueBool("1 != 2", true)
    assertExecValueBool("1 < 2", true)
    assertExecValueBool("1 > 2", false)
    assertExecValueBool("1 <= 2", true)
    assertExecValueBool("1 >= 2", false)
    assertExecValueBool("1 <= 1", true)
    assertExecValueBool("1 >= 1", true)

    assertExecValueString("\"\" + \"\"", "")
    assertExecValueString("\"hello\" + \"\"", "hello")
    assertExecValueString("\"\" + \"world\"", "world")

    assertExecValueString("\"hello\" + \" world\"", "hello world")
    assertExecValueString("\"hello\" + \" world\" + \"!\"", "hello world!")
  }

  def binaryPrecedence(): unit = {
    assertExecValueInt("1 + 2 + 3", 6)
    assertExecValueInt("1 + 2 * 3", 7)
    assertExecValueInt("1 + 2 * 3 - 4", 3)
    assertExecValueInt("1 + 2 * 3 - 4 + 5", 8)

    assertExecValueBool("1 < 2 && 2 < 3", true)
    assertExecValueBool("1 < 2 && 2 > 3", false)
    assertExecValueBool("1 < 2 || 2 < 3", true)
    assertExecValueBool("1 < 2 || 2 > 3", true)
    assertExecValueBool("1 < 2 || 2 > 3 && 3 < 4", true)
  }

  def unaryPrecedence(): unit = {
    assertExecValueInt("1 + -2", -1)
    assertExecValueInt("1 + ~2", -2)
    assertExecValueInt("1 + -2 * 3", -5)
    assertExecValueInt("1 + -2 / 3", 1)
    assertExecValueInt("1 + -2 % 3", -1)
    assertExecValueInt("1 + -2 & 3", 3)
    assertExecValueInt("1 + -2 | 3", -1)
    assertExecValueInt("1 + -2 ^ 3", -4)
    assertExecValueBool("!true && true", false)
    assertExecValueBool("!true || true", true)
  }
}
