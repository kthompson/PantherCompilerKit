import Helpers._
import panther._
import TestFramework._

object VmTests {
  def run(): unit = {
    suite("VM Tests")

    assertExecValueInt("1", 1)
    assertExecValueInt("1 + 2", 3)
    assertExecValueInt("1 + 2 +3", 6)
    assertExecValueInt("1 + 2 * 3", 7)
    assertExecValueInt("1 + 2 * 3 - 4", 3)
    assertExecValueInt("1 + 2 * 3 - 4 + 5", 8)

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

    assertExecValueBool("1 < 2 && 2 < 3", true)
    assertExecValueBool("1 < 2 && 2 > 3", false)
    assertExecValueBool("1 < 2 || 2 < 3", true)
    assertExecValueBool("1 < 2 || 2 > 3", true)
    assertExecValueBool("1 < 2 || 2 > 3 && 3 < 4", true)

  }
}
