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
      )
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
}
