import panther.{assert => _, *}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/** Program.run returns the number of diagnostics it reported, and main turns a
  * non-zero count into a non-zero process exit code. Anything downstream — CI,
  * scripts, an editor — reads that exit code, so the count has to be accurate.
  */
class ProgramTests extends AnyFunSpec with Matchers {

  private def runOnSource(source: string): int = {
    val file = Files.createTempFile("panther-program-test", ".pn")
    try {
      Files.writeString(file, source)
      val output = Files.createTempDirectory("panther-program-out")
      Program.run(
        CompilerSettingsFactory.default,
        output.resolve("out.pnb").toString,
        List.Cons(file.toString, List.Nil)
      ) match {
        case RunResult.Diagnostics(count) => count
        case RunResult.Executed(code) =>
          throw new AssertionError("Expected diagnostics, got exit " + code)
      }
    } finally {
      Files.deleteIfExists(file)
      ()
    }
  }

  /** `--run` compiles and executes, so the result is the program's own exit
    * code rather than a count.
    */
  private def runAndExecute(source: string): int = {
    val file = Files.createTempFile("panther-program-test", ".pn")
    try {
      Files.writeString(file, source)
      val settings = CompilerSettingsFactory.default
      Program.run(
        CompilerSettings(
          settings.kindRecoveryAttempts,
          settings.diagnosticsToPrint,
          settings.stackSize,
          settings.heapSize,
          settings.debug,
          settings.enableTracing,
          settings.printSymbols,
          settings.printBoundAssembly,
          settings.printLoweredAssembly,
          settings.transpile,
          true // run
        ),
        "",
        List.Cons(file.toString, List.Nil)
      ) match {
        case RunResult.Executed(code) => code
        case RunResult.Diagnostics(count) =>
          throw new AssertionError(
            "Expected the program to run, got " + count + " diagnostics"
          )
      }
    } finally {
      Files.deleteIfExists(file)
      ()
    }
  }

  describe("Program.run") {
    it("should report no diagnostics for a source that compiles") {
      runOnSource("val x = 12") shouldEqual 0
    }

    it("should report diagnostics for a source that does not bind") {
      runOnSource("val x = someUndefinedThing") should be > 0
    }

    it("should report diagnostics for a source that does not parse") {
      runOnSource("val x = ") should be > 0
    }
  }

  /** `--run` is the whole of §3.2's first bullet: compile and execute in one
    * step, with the program's exit code reaching the process.
    */
  describe("Program.run with --run") {
    it("should execute a program and report success") {
      runAndExecute("println(\"hello\")") shouldEqual 0
    }

    it("should pass the program's own exit code through") {
      runAndExecute("exit(0)") shouldEqual 0
      runAndExecute("exit(3)") shouldEqual 3
      runAndExecute("exit(42)") shouldEqual 42
    }

    it("should report a runtime failure as 1") {
      runAndExecute("panic(\"boom\")") shouldEqual 1
    }

    /** A program that evaluates to a number succeeded; the number is not a
      * code.
      */
    it("should not mistake a result for an exit code") {
      runAndExecute("2") shouldEqual 0
    }
  }
}
