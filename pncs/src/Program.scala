import panther._

object Program {
    def main(args: Array[String]): Unit = {
        print("pncs")
        for (i <- 0 to (args.length - 1)) {
            print(" " + args(i))
        }
        println()

        if(args.length < 2) {
            printHelp()
        } else if (args(0) == "-t") {
            printLogo()
            val trees = new Array[SyntaxTree](args.length - 2)
            for (x <- 2 to (args.length - 1)) {
                print("parsing " + args(x) + "...")
                trees(x - 2) = MakeSyntaxTree.parse_file(args(x))
                println("done")
            }

            val compilation = MakeCompilation.create(trees)

            if (compilation.diagnostics.length > 0) {
                for (i <- 0 to (compilation.diagnostics.length - 1)) {
                    printDiagnostic(compilation.diagnostics(i))
                }
            } else {
                println("transpiling to " + args(1) + "...")
                compilation.transpile(args(1))
            }
        } else {
            printLogo()
            val trees = new Array[SyntaxTree](args.length - 1)
            for (x <- 1 to (args.length - 1)) {
                print("parsing " + args(x) + "...")
                trees(x - 1) = MakeSyntaxTree.parse_file(args(x))
                println("done")
            }

            val compilation = MakeCompilation.create(trees)

            if (compilation.diagnostics.length > 0) {
                for (i <- 0 to (compilation.diagnostics.length - 1)) {
                    printDiagnostic(compilation.diagnostics(i))
                }
            } else {
                println("emitting to " + args(0) + "...")
                compilation.emit(args(0))
            }
        }
    }

    def printDiagnostic(diagnostic: Diagnostic): unit = {
        val location = diagnostic.location
        val span = location.span
        val source_file = location.source_file

        println(diagnostic.toString())

        for (currrent_line <- location.start_line to location.end_line) {
            val line = source_file.get_line(currrent_line)
            val start_in_current = source_file.get_line_index(span.start) == currrent_line
            val end_in_current = source_file.get_line_index(span.end) == currrent_line

            val prefix_end =
                if (start_in_current) span.start
                else line.start

            val suffix_start =
                if (end_in_current) span.end
                else line.end

            val prefix_span = TextSpanFactory.from_bounds(line.start, prefix_end)
            val error_span = TextSpanFactory.from_bounds(prefix_end, suffix_start)
            val suffix_span = TextSpanFactory.from_bounds(suffix_start, line.end)

            val prefix = source_file.to_string(prefix_span)
            val error = source_file.to_string(error_span)
            val suffix = source_file.to_string(suffix_span)

            print(prefix)
            print(ANSI.foreground_color("e06c75"))
            print(error)
            print(ANSI.Clear)
            println(suffix)

            for(c <- 0 to (prefix_span.length - 2)) {
                print('-')
            }
            println('^')
        }

        println()
    }

    def printToken(token: SyntaxToken): unit =
        println(Pad.right("TOKEN[" + SyntaxFacts.getKindName(token.kind) + "," + string(token.start) + "]: ", 36, ' ') + "\"" + token.text + "\"")

    def printLogo(): unit = println("panther compiler - 0.0.1")
    def printHelp(): unit = {
        println("pncs output.c [sources]")
        println("pncs -t output/ [sources]")
    }
}