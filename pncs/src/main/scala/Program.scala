import panther._

/** What a `pncs` invocation ended up doing, which decides the process exit
  * code.
  *
  * Two cases rather than one number, because the two numbers do not mean the
  * same thing: a diagnostic count is a tally and any non-zero tally is one
  * failure, while an exit code was chosen by the program and has to survive
  * unchanged. Collapsing them made `exit(3)` leave as 1.
  */
enum RunResult {
  case Diagnostics(count: int)
  case Executed(exitCode: int)
}

object Program {
  def main(args: Array[String]): Unit = {
    val parseResult = ArgsParser.parse(args)

    parseResult.error match {
      case Option.Some(errorMsg) =>
        println("Error: " + errorMsg)
        ArgsParser.printUsage()
        exit(1)
      case Option.None =>
        if (parseResult.showHelp) {
          ArgsParser.printUsage()
        } else {
          val result = run(
            parseResult.settings,
            parseResult.outputFile,
            parseResult.sourceFiles
          )
          // Anything downstream - CI, scripts, an editor - reads the exit
          // code, so a compile that reported diagnostics has to fail, and a
          // program that picked its own code has to keep it.
          result match {
            case RunResult.Diagnostics(0) => ()
            case RunResult.Diagnostics(_) => exit(1)
            case RunResult.Executed(0)    => ()
            case RunResult.Executed(code) => exit(code)
          }
        }
    }
  }

  def hasDiagnostics(trees: List[SyntaxTree]): bool = {
    trees match {
      case List.Nil => false
      case List.Cons(head, tail) =>
        head.diagnostics match {
          case Diagnostics.Empty => hasDiagnostics(tail)
          case _                 => true
        }
    }
  }

  def run(
      settings: CompilerSettings,
      outputFile: string,
      sourceFiles: List[string]
  ): RunResult = {
    printLogo()
    var trees: List[SyntaxTree] = List.Nil

    // Parse source files from the list
    var currentFiles = sourceFiles
    while (currentFiles != List.Nil) {
      currentFiles match {
        case List.Nil => ()
        case List.Cons(sourceFile, remainingFiles) =>
          print("parsing " + sourceFile + "...")
          trees =
            List.Cons(MakeSyntaxTree.parseFile(sourceFile, settings), trees)
          println("done")
          currentFiles = remainingFiles
      }
    }

    // verify no diagnostics from parse trees
    if (hasDiagnostics(trees)) {
      var parseErrors = 0
      while (trees != List.Nil) {
        trees match {
          case List.Nil => ()
          case List.Cons(head, tail) =>
            parseErrors = parseErrors +
              head.diagnostics.printDiagnostics(settings.diagnosticsToPrint)
            trees = tail
        }
      }
      println("found " + string(parseErrors) + " diagnostics")
      RunResult.Diagnostics(parseErrors)
    } else if (settings.transpile) {
      val transpiler = new Transpiler(trees, outputFile)
      transpiler.transpile()
      RunResult.Diagnostics(0)
    } else {
      val compilation = MakeCompilation.create(trees, settings)
      if (settings.printSymbols) {
        compilation.printSymbols()
      }

      if (settings.printBoundAssembly) {
        compilation.printBoundAssembly()
      }

      if (settings.printLoweredAssembly) {
        compilation.printLoweredAssembly()
      }

      compilation.diagnostics match {
        case Diagnostics.Empty =>
          if (settings.run) {
            RunResult.Executed(execute(compilation))
          } else {
            println("emitting to " + outputFile + "...")
            compilation.emit(outputFile)
            RunResult.Diagnostics(0)
          }

        case diags =>
          val count = diags.printDiagnostics(settings.diagnosticsToPrint)
          println("found " + string(count) + " diagnostics")
          RunResult.Diagnostics(count)
      }
    }
  }

  /** Runs a clean compilation and answers the code the process should end with.
    *
    * A program that chose its own code reports that code; one that failed at
    * runtime reports 1, there being no code to honour.
    */
  def execute(compilation: Compilation): int = {
    compilation.exec() match {
      case InterpretResult.Exit(code)   => code
      case InterpretResult.RuntimeError => 1
      case InterpretResult.CompileError => 1
      // `Ok` and `OkValue` are both a program that ran to the end. What it
      // evaluated to is not an exit code — a program ending in `2` succeeded.
      case _ => 0
    }
  }

  def printToken(token: SyntaxToken): unit =
    println(
      Pad.right(
        "TOKEN[" + SyntaxFacts.getKindName(token.kind) + "," + string(
          token.start
        ) + "]: ",
        36,
        ' '
      ) + "\"" + token.text + "\""
    )

  def printLogo(): unit = println("panther compiler - 0.0.1")
}
