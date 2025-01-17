import panther._

object Program {
  def main(args: Array[String]): Unit = {
    print("pncs")
    for (i <- 0 to (args.length - 1)) {
      print(" " + args(i))
    }
    println()

    if (args.length < 2) {
      printHelp()
    } else if (args(0) == "-t") {
      run(true, args)
    } else {
      run(false, args)
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

  def run(transpile: bool, args: Array[string]): unit = {
    val firstFile = if (transpile) 2 else 1
    val numTrees = args.length - firstFile
    val outputFile = args(firstFile - 1)
    printLogo()
    var trees: List[SyntaxTree] = List.Nil
    for (x <- firstFile to (args.length - 1)) {
      print("parsing " + args(x) + "...")
      trees = List.Cons(MakeSyntaxTree.parseFile(args(x)), trees)
      println("done")
    }

    // verify no diagnostics from parse trees
    if (hasDiagnostics(trees)) {
      while (trees != List.Nil) {
        trees match {
          case List.Nil => ()
          case List.Cons(head, tail) =>
            printDiagnostics(head.diagnostics, 0)
            trees = tail
        }
      }
    } else {
      val compilation = MakeCompilation.create(trees)
      compilation.diagnostics match {
        case Diagnostics.Empty =>
          if (transpile) {
            println("transpiling to " + outputFile + "...")
            compilation.transpile(outputFile)
//            compilation.printSymbols()
          } else {
            compilation.printSymbols()
            println("emitting to " + outputFile + "...")
            compilation.emit(outputFile)
          }

        case diags =>
          compilation.printSymbols()
          val count = printDiagnostics(diags, 0)
          println("found " + count + " diagnostics")
      }
    }
  }

  def printDiagnostics(diagnostics: Diagnostics, count: int): int =
    diagnostics match {
      case Diagnostics.Empty => count
      case Diagnostics.Node(left, head, right) =>
        val leftCount = printDiagnostics(left, count)
        if (leftCount <= 20) {
          printDiagnostic(head)
        }
        printDiagnostics(right, leftCount + 1)
    }

  def printDiagnostic(diagnostic: Diagnostic): unit = {
    val location = diagnostic.location
    val span = location.span
    val sourceFile = location.sourceFile

    println(diagnostic.toString())

    for (currentLine <- location.startLine to location.endLine) {
      val line = sourceFile.getLine(currentLine)
      val startInCurrent = sourceFile.getLineIndex(span.start) == currentLine
      val endInCurrent = sourceFile.getLineIndex(span.end) == currentLine

      val prefixEnd =
        if (startInCurrent) span.start
        else line.start

      val suffixStart =
        if (endInCurrent) span.end
        else line.end

      val prefixSpan = TextSpanFactory.fromBounds(line.start, prefixEnd)
      val errorSpan = TextSpanFactory.fromBounds(prefixEnd, suffixStart)
      val suffixSpan = TextSpanFactory.fromBounds(suffixStart, line.end)

      val prefix = sourceFile.substringFromSpan(prefixSpan)
      val error = sourceFile.substringFromSpan(errorSpan)
      val suffix = sourceFile.substringFromSpan(suffixSpan)

      print(prefix)
      print(ANSI.foregroundColor("e06c75"))
      print(error)
      print(ANSI.Clear)
      println(suffix)

      for (c <- 0 to (prefixSpan.length - 2)) {
        print('-')
      }
      println('^')
    }

    println()
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

  def printHelp(): unit = {
    println("pncs output.c [sources]")
    println("pncs -t output/ [sources]")
  }
}
