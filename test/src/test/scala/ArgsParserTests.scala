import TestHelpers._
import utest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArgsParserTests extends AnyFlatSpec with Matchers {

  "ArgsParser" should "show help when no args provided" in {
    val result = ArgsParser.parse(Array())
    result.showHelp shouldBe true
    result.error shouldBe Option.None
  }

  it should "handle help flags" in {
    val result1 = ArgsParser.parse(Array("--help"))
    result1.showHelp shouldBe true
    result1.error shouldBe Option.None

    val result2 = ArgsParser.parse(Array("-h"))
    result2.showHelp shouldBe true
    result2.error shouldBe Option.None
  }

  it should "parse basic compilation arguments" in {
    val result = ArgsParser.parse(Array("output.c", "source.scala"))
    result.error shouldBe Option.None
    result.showHelp shouldBe false
    result.outputFile shouldBe "output.c"

    result.sourceFiles match {
      case List.Cons("source.scala", List.Nil) => // Success
      case _ => fail("Expected single source file")
    }
  }

  it should "handle multiple source files" in {
    val result =
      ArgsParser.parse(Array("output.c", "source1.scala", "source2.scala"))
    result.error shouldBe Option.None
    result.sourceFiles match {
      case List.Cons(
            "source1.scala",
            List.Cons("source2.scala", List.Nil)
          ) => // Success
      case _ => fail("Expected two source files")
    }
  }

  it should "handle transpile flag" in {
    val result =
      ArgsParser.parse(Array("--transpile", "output_dir/", "source.scala"))
    result.error shouldBe Option.None
    result.settings.transpile shouldBe true
    result.outputFile shouldBe "output_dir/"
  }

  it should "handle debug flags" in {
    val result = ArgsParser.parse(Array("--debug", "output.c", "source.scala"))
    result.error shouldBe Option.None
    result.settings.debug shouldBe true
    result.settings.enableTracing shouldBe true
    result.settings.printSymbols shouldBe true
    result.settings.printBoundAssembly shouldBe true
    result.settings.printLoweredAssembly shouldBe true
  }

  it should "handle individual debug flags" in {
    val result = ArgsParser.parse(
      Array(
        "--trace",
        "--print-symbols",
        "--print-bound-assembly",
        "--print-lowered-assembly",
        "output.c",
        "source.scala"
      )
    )
    result.error shouldBe Option.None
    result.settings.enableTracing shouldBe true
    result.settings.printSymbols shouldBe true
    result.settings.printBoundAssembly shouldBe true
    result.settings.printLoweredAssembly shouldBe true
    // debug should remain false since it wasn't explicitly set
    result.settings.debug shouldBe false
  }

  it should "handle stack size option" in {
    val result =
      ArgsParser.parse(Array("--stack-size", "100", "output.c", "source.scala"))
    result.error shouldBe Option.None
    result.settings.stackSize shouldBe 100

    // Test invalid stack size
    val resultInvalid =
      ArgsParser.parse(Array("--stack-size", "-5", "output.c", "source.scala"))
    resultInvalid.error shouldBe Option.Some("stack size must be positive")

    // Test missing stack size value
    val resultMissing = ArgsParser.parse(Array("--stack-size"))
    resultMissing.error shouldBe Option.Some("--stack-size requires a value")
  }

  it should "handle heap size option" in {
    val result =
      ArgsParser.parse(Array("--heap-size", "2048", "output.c", "source.scala"))
    result.error shouldBe Option.None
    result.settings.heapSize shouldBe 2048

    // Test invalid heap size
    val resultInvalid =
      ArgsParser.parse(Array("--heap-size", "0", "output.c", "source.scala"))
    resultInvalid.error shouldBe Option.Some("heap size must be positive")
  }

  it should "handle recovery attempts option" in {
    val result = ArgsParser.parse(
      Array("--recovery-attempts", "10", "output.c", "source.scala")
    )
    result.error shouldBe Option.None
    result.settings.kindRecoveryAttempts shouldBe 10

    // Test zero attempts (should be valid)
    val resultZero = ArgsParser.parse(
      Array("--recovery-attempts", "0", "output.c", "source.scala")
    )
    resultZero.error shouldBe Option.None
    resultZero.settings.kindRecoveryAttempts shouldBe 0

    // Test negative attempts
    val resultNegative = ArgsParser.parse(
      Array("--recovery-attempts", "-1", "output.c", "source.scala")
    )
    resultNegative.error shouldBe Option.Some(
      "recovery attempts must be non-negative"
    )
  }

  it should "handle diagnostics limit option" in {
    val result = ArgsParser.parse(
      Array("--diagnostics-limit", "50", "output.c", "source.scala")
    )
    result.error shouldBe Option.None
    result.settings.diagnosticsToPrint shouldBe 50

    // Test invalid limit
    val resultInvalid = ArgsParser.parse(
      Array("--diagnostics-limit", "0", "output.c", "source.scala")
    )
    resultInvalid.error shouldBe Option.Some(
      "diagnostics limit must be positive"
    )
  }

  it should "handle unknown options" in {
    val result =
      ArgsParser.parse(Array("--unknown-flag", "output.c", "source.scala"))
    result.error shouldBe Option.Some("unknown option: --unknown-flag")
  }

  it should "require output file" in {
    val result = ArgsParser.parse(Array("--debug"))
    result.error shouldBe Option.Some("output file is required")
  }

  it should "require source files" in {
    val result = ArgsParser.parse(Array("output.c"))
    result.error shouldBe Option.Some("at least one source file is required")
  }

  it should "handle complex argument combinations" in {
    val result = ArgsParser.parse(
      Array(
        "--debug",
        "--stack-size",
        "200",
        "--heap-size",
        "4096",
        "--recovery-attempts",
        "3",
        "--diagnostics-limit",
        "15",
        "complex_output.c",
        "file1.scala",
        "file2.scala"
      )
    )

    result.error shouldBe Option.None
    result.settings.debug shouldBe true
    result.settings.stackSize shouldBe 200
    result.settings.heapSize shouldBe 4096
    result.settings.kindRecoveryAttempts shouldBe 3
    result.settings.diagnosticsToPrint shouldBe 15
    result.outputFile shouldBe "complex_output.c"

    // Verify source files count
    var fileCount = 0
    var current = result.sourceFiles
    while (current != List.Nil) {
      current match {
        case List.Cons(_, tail) =>
          fileCount = fileCount + 1
          current = tail
        case List.Nil => ()
      }
    }
    fileCount shouldBe 2
  }

  it should "handle transpile with debug options" in {
    val result = ArgsParser.parse(
      Array(
        "-t",
        "--trace",
        "--print-symbols",
        "output_dir/",
        "source.scala"
      )
    )

    result.error shouldBe Option.None
    result.settings.transpile shouldBe true
    result.settings.enableTracing shouldBe true
    result.settings.printSymbols shouldBe true
    result.settings.debug shouldBe false // not explicitly set
    result.outputFile shouldBe "output_dir/"
  }
}
