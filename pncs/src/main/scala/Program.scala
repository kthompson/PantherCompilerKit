import panther._

object Program {
  def main(args: Array[String]): Unit = {
    val parseResult = ArgsParser.parse(args)

    parseResult.error match {
      case Option.Some(errorMsg) =>
        println("Error: " + errorMsg)
        ArgsParser.printUsage()
      case Option.None =>
        if (parseResult.showHelp) {
          ArgsParser.printUsage()
        } else {
          run(
            parseResult.settings,
            parseResult.outputFile,
            parseResult.sourceFiles
          )
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
  ): unit = {
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
            parseErrors =
              head.diagnostics.printDiagnostics(settings.diagnosticsToPrint)
            trees = tail
        }
      }
      println("found " + string(parseErrors) + " diagnostics")
    } else if (settings.transpile) {
      val transpiler = new Transpiler(trees, outputFile)
      transpiler.transpile()
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
          println("emitting to " + outputFile + "...")
          compilation.emit(outputFile)

        case diags =>
          val count = diags.printDiagnostics(settings.diagnosticsToPrint)
          println("found " + count + " diagnostics")
      }
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
