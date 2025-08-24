import TestHelpers._
import utest._

object ArgsParserTests extends TestSuite {
  val tests = Tests {
    test("empty args show help") {
      val result = ArgsParser.parse(Array())
      assert(result.showHelp)
      assert(result.error == Option.None)
    }

    test("help flags") {
      val result1 = ArgsParser.parse(Array("--help"))
      assert(result1.showHelp)
      assert(result1.error == Option.None)

      val result2 = ArgsParser.parse(Array("-h"))
      assert(result2.showHelp)
      assert(result2.error == Option.None)
    }

    test("basic compilation arguments") {
      val result = ArgsParser.parse(Array("output.c", "source.scala"))
      assert(result.error == Option.None)
      assert(!result.showHelp)
      assert(result.outputFile == "output.c")

      // Check source files
      result.sourceFiles match {
        case List.Cons("source.scala", List.Nil) => // Success
        case _ => throw new RuntimeException("Expected single source file")
      }

      // Check default settings
      assert(!result.settings.transpile)
      assert(!result.settings.debug)
      assert(result.settings.stackSize == 50)
      assert(result.settings.heapSize == 1024)
    }

    test("multiple source files") {
      val result = ArgsParser.parse(
        Array("output.c", "file1.scala", "file2.scala", "file3.scala")
      )
      assert(result.error == Option.None)

      // Check all source files are present
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
      assert(fileCount == 3)
    }

    test("transpile flag") {
      val result1 =
        ArgsParser.parse(Array("--transpile", "output/", "source.scala"))
      assert(result1.error == Option.None)
      assert(result1.settings.transpile)
      assert(result1.outputFile == "output/")

      val result2 = ArgsParser.parse(Array("-t", "output/", "source.scala"))
      assert(result2.error == Option.None)
      assert(result2.settings.transpile)
    }

    test("debug flag enables all debug options") {
      val result =
        ArgsParser.parse(Array("--debug", "output.c", "source.scala"))
      assert(result.error == Option.None)
      assert(result.settings.debug)
      assert(result.settings.enableTracing)
      assert(result.settings.printSymbols)
      assert(result.settings.printBoundAssembly)
      assert(result.settings.printLoweredAssembly)
    }

    test("individual debug flags") {
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
      assert(result.error == Option.None)
      assert(result.settings.enableTracing)
      assert(result.settings.printSymbols)
      assert(result.settings.printBoundAssembly)
      assert(result.settings.printLoweredAssembly)
      // debug should remain false since it wasn't explicitly set
      assert(!result.settings.debug)
    }

    test("stack size option") {
      val result = ArgsParser.parse(
        Array("--stack-size", "100", "output.c", "source.scala")
      )
      assert(result.error == Option.None)
      assert(result.settings.stackSize == 100)

      // Test invalid stack size
      val resultInvalid = ArgsParser.parse(
        Array("--stack-size", "-5", "output.c", "source.scala")
      )
      assert(resultInvalid.error == Option.Some("stack size must be positive"))

      // Test missing stack size value
      val resultMissing = ArgsParser.parse(Array("--stack-size"))
      assert(
        resultMissing.error == Option.Some("--stack-size requires a value")
      )
    }

    test("heap size option") {
      val result = ArgsParser.parse(
        Array("--heap-size", "2048", "output.c", "source.scala")
      )
      assert(result.error == Option.None)
      assert(result.settings.heapSize == 2048)

      // Test invalid heap size
      val resultInvalid =
        ArgsParser.parse(Array("--heap-size", "0", "output.c", "source.scala"))
      assert(resultInvalid.error == Option.Some("heap size must be positive"))
    }

    test("recovery attempts option") {
      val result = ArgsParser.parse(
        Array("--recovery-attempts", "10", "output.c", "source.scala")
      )
      assert(result.error == Option.None)
      assert(result.settings.kindRecoveryAttempts == 10)

      // Test zero attempts (should be valid)
      val resultZero = ArgsParser.parse(
        Array("--recovery-attempts", "0", "output.c", "source.scala")
      )
      assert(resultZero.error == Option.None)
      assert(resultZero.settings.kindRecoveryAttempts == 0)

      // Test negative attempts
      val resultNegative = ArgsParser.parse(
        Array("--recovery-attempts", "-1", "output.c", "source.scala")
      )
      assert(
        resultNegative.error == Option.Some(
          "recovery attempts must be non-negative"
        )
      )
    }

    test("diagnostics limit option") {
      val result = ArgsParser.parse(
        Array("--diagnostics-limit", "50", "output.c", "source.scala")
      )
      assert(result.error == Option.None)
      assert(result.settings.diagnosticsToPrint == 50)

      // Test invalid limit
      val resultInvalid = ArgsParser.parse(
        Array("--diagnostics-limit", "0", "output.c", "source.scala")
      )
      assert(
        resultInvalid.error == Option.Some("diagnostics limit must be positive")
      )
    }

    test("unknown option") {
      val result =
        ArgsParser.parse(Array("--unknown-flag", "output.c", "source.scala"))
      assert(result.error == Option.Some("unknown option: --unknown-flag"))
    }

    test("missing output file") {
      val result = ArgsParser.parse(Array("--debug"))
      assert(result.error == Option.Some("output file is required"))
    }

    test("missing source files") {
      val result = ArgsParser.parse(Array("output.c"))
      assert(
        result.error == Option.Some("at least one source file is required")
      )
    }

    test("complex argument combination") {
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

      assert(result.error == Option.None)
      assert(result.settings.debug)
      assert(result.settings.stackSize == 200)
      assert(result.settings.heapSize == 4096)
      assert(result.settings.kindRecoveryAttempts == 3)
      assert(result.settings.diagnosticsToPrint == 15)
      assert(result.outputFile == "complex_output.c")

      // Verify source files
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
      assert(fileCount == 2)
    }

    test("transpile with debug options") {
      val result = ArgsParser.parse(
        Array(
          "-t",
          "--trace",
          "--print-symbols",
          "output_dir/",
          "source.scala"
        )
      )

      assert(result.error == Option.None)
      assert(result.settings.transpile)
      assert(result.settings.enableTracing)
      assert(result.settings.printSymbols)
      assert(!result.settings.debug) // not explicitly set
      assert(result.outputFile == "output_dir/")
    }
  }
}
