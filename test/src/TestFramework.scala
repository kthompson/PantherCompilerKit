import panther._

case class TestCase(test: string, passed: bool)
case class TestSuite(name: string, tests: List[TestCase]) {

  val passed = count(true, 0, tests)
  val failed = count(false, 0, tests)

  def count(pass: bool, n: int, cases: List[TestCase]): int =
    cases match {
      case List.Nil => n
      case List.Cons(TestCase(_, p), tail) =>
        if (p == pass) count(pass, n + 1, tail)
        else count(pass, n, tail)
    }
}

object TestFramework {
  import ANSI._

  var currentSuite: TestSuite = TestSuite("", List.Nil)
  var results: List[TestSuite] = List.Nil

  val DeepSkyBlue = "00BFFF"
  val White = "FFFFFF"
  val Red = "FF5555"
  val PassText = foregroundColor("[PASS]", "00FF00")
  val FailText = foregroundColor("[FAIL]", Red)

  def foregroundColor(text: string, hex: string): string =
    s"${ANSI.foregroundColor(hex)}$text${ANSI.Clear}"

  def suite(name: string): unit = {
    if (currentSuite.name != "") {
      results = List.Cons(currentSuite, results)
    }

    currentSuite = TestSuite(name, List.Nil)
//    println(
//      "\n== Suite: " + foregroundColor(name, DeepSkyBlue) + " =="
//    )
  }

  def test(name: string): unit = {
    currentSuite = currentSuite match {
      case TestSuite(suiteName, tests) =>
        TestSuite(suiteName, List.Cons(TestCase(name, true), tests))
    }

//    println("- Running test: " + foregroundColor(name, White))
  }

  def failed(reason: string): never = {
    currentSuite = currentSuite match {
      case TestSuite(suiteName, List.Cons(TestCase(currentTest, _), tests)) =>

//        println(
//          "  " + foregroundColor(
//            "[FAIL]",
//            Red
//          ) + " " + currentTest + ": " + reason
//        )

        TestSuite(suiteName, List.Cons(TestCase(currentTest, false), tests))
      case _ =>
        panic("No current test to fail")
    }
    results = List.Cons(currentSuite, results)
    _reportSuites(results)
    panic("")
  }

  def _reportSuites(suites: List[TestSuite]): unit = {
    suites match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        _reportSuites(tail)
        _reportSuite(head)
    }
  }

  def complete(): unit = {
    if (currentSuite.name != "") {
      results = List.Cons(currentSuite, results)
    }

    _reportSuites(results)
  }

  def _reportSuite(suite: TestSuite): unit = {

    suite match {
      case TestSuite(suiteName, tests) =>

        val passed = foregroundColor(
          string(suite.passed) + " passed",
          "00FF00" // Lime
        )
        val failColor = if (suite.failed > 0) Red else "AAAAAA"
        val failed =
          foregroundColor(string(suite.failed) + " failed", failColor)

        println(
          "\nSuite: " + foregroundColor(suite.name, DeepSkyBlue) + " " +
            "(" + passed + ", " + failed + ")"
        )

        _reportTests(tests)
    }
  }

  def _reportTests(tests: List[TestCase]): unit = {
    tests match {
      case List.Nil => ()
      case List.Cons(head, tail) =>
        _reportTests(tail)
        _reportTest(head)
    }
  }

  def _reportTest(test: TestCase): unit = {
    test match {
      case TestCase(testName, passed) =>
        val result =
          if (passed)
            foregroundColor("[PASS]", "00FF00")
          else
            foregroundColor("[FAIL]", Red)

        println("  " + result + " " + testName)
    }
  }
}
