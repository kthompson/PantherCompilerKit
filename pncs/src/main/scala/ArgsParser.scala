import panther._

case class ArgsParseResult(
    settings: CompilerSettings,
    outputFile: string,
    sourceFiles: List[string],
    showHelp: bool,
    error: Option[string]
)

object ArgsParser {
  def parse(args: Array[string]): ArgsParseResult = {
    if (args.length == 0) {
      ArgsParseResult(
        CompilerSettingsFactory.default,
        "",
        List.Nil,
        showHelp = true,
        error = Option.None
      )
    } else {

      var settings = CompilerSettingsFactory.default
      var outputFile = ""
      var sourceFiles: List[string] = List.Nil
      var currentIndex = 0
      var showHelp = false
      var error: Option[string] = Option.None

      while (currentIndex < args.length && error == Option.None) {
        val arg = args(currentIndex)

        if (arg == "--help" || arg == "-h") {
          showHelp = true
          currentIndex = currentIndex + 1
        } else if (arg == "--transpile" || arg == "-t") {
          settings = settings.copy(transpile = true)
          currentIndex = currentIndex + 1
        } else if (arg == "--debug") {
          settings = settings.copy(
            debug = true,
            enableTracing = true,
            printSymbols = true,
            printBoundAssembly = true,
            printLoweredAssembly = true
          )
          currentIndex = currentIndex + 1
        } else if (arg == "--trace") {
          settings = settings.copy(enableTracing = true)
          currentIndex = currentIndex + 1
        } else if (arg == "--print-symbols") {
          settings = settings.copy(printSymbols = true)
          currentIndex = currentIndex + 1
        } else if (arg == "--print-bound-assembly") {
          settings = settings.copy(printBoundAssembly = true)
          currentIndex = currentIndex + 1
        } else if (arg == "--print-lowered-assembly") {
          settings = settings.copy(printLoweredAssembly = true)
          currentIndex = currentIndex + 1
        } else if (arg == "--stack-size") {
          if (currentIndex + 1 >= args.length) {
            error = Option.Some("--stack-size requires a value")
          } else {
            currentIndex = currentIndex + 1
            val stackSizeStr = args(currentIndex)
            // Parse integer (simplified for Panther language)
            val stackSize = parseInt(stackSizeStr)
            if (stackSize <= 0) {
              error = Option.Some("stack size must be positive")
            } else {
              settings = settings.copy(stackSize = stackSize)
            }
            currentIndex = currentIndex + 1
          }
        } else if (arg == "--heap-size") {
          if (currentIndex + 1 >= args.length) {
            error = Option.Some("--heap-size requires a value")
          } else {
            currentIndex = currentIndex + 1
            val heapSizeStr = args(currentIndex)
            val heapSize = parseInt(heapSizeStr)
            if (heapSize <= 0) {
              error = Option.Some("heap size must be positive")
            } else {
              settings = settings.copy(heapSize = heapSize)
            }
            currentIndex = currentIndex + 1
          }
        } else if (arg == "--recovery-attempts") {
          if (currentIndex + 1 >= args.length) {
            error = Option.Some("--recovery-attempts requires a value")
          } else {
            currentIndex = currentIndex + 1
            val attemptsStr = args(currentIndex)
            val attempts = parseInt(attemptsStr)
            if (attempts < 0) {
              error = Option.Some("recovery attempts must be non-negative")
            } else {
              settings = settings.copy(kindRecoveryAttempts = attempts)
            }
            currentIndex = currentIndex + 1
          }
        } else if (arg == "--diagnostics-limit") {
          if (currentIndex + 1 >= args.length) {
            error = Option.Some("--diagnostics-limit requires a value")
          } else {
            currentIndex = currentIndex + 1
            val limitStr = args(currentIndex)
            val limit = parseInt(limitStr)
            if (limit <= 0) {
              error = Option.Some("diagnostics limit must be positive")
            } else {
              settings = settings.copy(diagnosticsToPrint = limit)
            }
            currentIndex = currentIndex + 1
          }
        } else if (startsWith(arg, "--")) {
          error = Option.Some("unknown option: " + arg)
        } else {
          // This is a positional argument (output file or source file)
          if (outputFile == "") {
            outputFile = arg
          } else {
            sourceFiles = List.Cons(arg, sourceFiles)
          }
          currentIndex = currentIndex + 1
        }
      }

      // Reverse source files list to maintain original order
      sourceFiles = reverseList(sourceFiles)

      // Validate required arguments
      if (!showHelp && error == Option.None) {
        if (outputFile == "") {
          error = Option.Some("output file is required")
        } else if (sourceFiles == List.Nil) {
          error = Option.Some("at least one source file is required")
        }
      }

      ArgsParseResult(settings, outputFile, sourceFiles, showHelp, error)
    }
  }

  def printUsage(): unit = {
    println("Usage: pncs [options] <output> <sources...>")
    println("")
    println("Options:")
    println("  -h, --help                    Show this help message")
    println(
      "  -t, --transpile              Transpile mode (output to directory)"
    )
    println("  --debug                      Enable all debug options")
    println("  --trace                      Enable execution tracing")
    println("  --print-symbols              Print symbol table")
    println("  --print-bound-assembly       Print bound assembly")
    println("  --print-lowered-assembly     Print lowered assembly")
    println("  --stack-size <size>          Set VM stack size (default: 50)")
    println("  --heap-size <size>           Set VM heap size (default: 1024)")
    println(
      "  --recovery-attempts <count>  Set parser recovery attempts (default: 5)"
    )
    println(
      "  --diagnostics-limit <count>  Set max diagnostics to print (default: 20)"
    )
    println("")
    println("Examples:")
    println("  pncs output.pnb source1.scala source2.scala")
    println("  pncs --transpile output/ source1.scala source2.scala")
    println("  pncs --debug --trace output.pnb source.scala")
  }

  // Helper functions
  def parseInt(str: string): int = {
    // Simple integer parsing for Panther language
    // This is a simplified implementation
    var result = 0
    var i = 0
    var negative = false

    if (str.length > 0 && str(0) == '-') {
      negative = true
      i = 1
    }

    while (i < str.length) {
      val c = str(i)
      if (c >= '0' && c <= '9') {
        result = result * 10 + (int(c) - int('0'))
      } else {
        return -1 // Invalid number
      }
      i = i + 1
    }

    if (negative) -result else result
  }

  def startsWith(str: string, prefix: string): bool = {
    if (prefix.length > str.length) {
      false
    } else {
      var i = 0
      while (i < prefix.length) {
        if (str(i) != prefix(i)) {
          return false
        }
        i = i + 1
      }
      true
    }
  }

  def reverseList(list: List[string]): List[string] = {
    var result: List[string] = List.Nil
    var current = list

    while (current != List.Nil) {
      current match {
        case List.Nil => ()
        case List.Cons(head, tail) =>
          result = List.Cons(head, result)
          current = tail
      }
    }

    result
  }
}
