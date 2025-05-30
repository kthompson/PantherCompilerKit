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

//    ifExpressions()

    // TODO: dup, swap, pop
    // TODO: arguments
    // TODO: call, ret
    // TODO: branch
    // TODO: conv
    // TODO: static field access
    // TODO: instance creation
    // TODO: instance field access

  }

  def ifExpressions(): unit = {
    val setup = "val x = 12"
    assertExecValueIntWithSetup(setup, "if (x == 12) 1 else 2", 1)
    assertExecValueIntWithSetup(setup, "if (x != 12) 1 else 2", 2)
    assertExecValueIntWithSetup(setup, "if (x < 12) 1 else 2", 2)
    assertExecValueIntWithSetup(setup, "if (x > 12) 1 else 2", 2)
    assertExecValueIntWithSetup(setup, "if (x <= 12) 1 else 2", 1)
    assertExecValueIntWithSetup(setup, "if (x >= 12) 1 else 2", 1)
    assertExecValueIntWithSetup(
      setup,
      "if (x < -10) -1 else if (x > -20) -2 else -3",
      -1
    )
    assertExecValueIntWithSetup(
      setup,
      "if (x < -10) -1 else if (x > -20) -2 else -3 + x",
      -13
    )
    assertExecValueIntWithSetup(
      setup,
      "if (x < -10) -1 else if (x > -20) -2 else -3 + x",
      -13
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
    assertExecValueInt("1 & 2", 0)
    assertExecValueInt("1 & 1", 1)
    assertExecValueInt("1 & 0", 0)
    assertExecValueInt("23 & 42", 2)
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
