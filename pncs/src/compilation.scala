import panther._

object MakeCompilation {
    def create(trees: Array[SyntaxTree]): Compilation = {
        val diagnosticBag = new DiagnosticBag()
        val root = new Symbol(SymbolKind.Root, "", TextLocationFactory.empty(), None)

        var num_diags = 0
        for (x <- 0 to (trees.length-1)) {
            num_diags = num_diags + trees(x).diagnostics.length
            val binder = new Binder(root, diagnosticBag)
            binder.bind_compilation_unit(trees(x).root)
        }
        val bind_diagnostics = diagnosticBag.diagnostics()
        num_diags = num_diags + bind_diagnostics.length

        val all_diags = new Array[Diagnostic](num_diags)
        var diag = 0
        for (i <- 0 to (trees.length-1)) {
            for (d <- 0 to (trees(i).diagnostics.length-1)) {
                all_diags(diag) = trees(i).diagnostics(d)
                diag = diag + 1
            }
        }

        for (b <- 0 to (bind_diagnostics.length-1)) {
            all_diags(diag) = bind_diagnostics(b)
            diag = diag + 1
        }

        // all the type inference data is in the checker so we cant
        // expose it to the compilation here
        // val checker = new Checker()
        // checker.infer_types(root)

        new Compilation(trees, root, all_diags)
    }
}

case class Compilation(syntaxTrees: Array[SyntaxTree], root: Symbol, diagnostics: Array[Diagnostic]) {
    def print_symbols(): unit = SymbolTreePrinter.print_symbol(root)

    def emit(output: string): unit = {
        val emitter = new Emitter(syntaxTrees, root, output)
        emitter.emit()
    }

    def transpile(outputPath: string): unit = {
        val transpiler = new Transpiler(syntaxTrees, root, outputPath)
        transpiler.transpile()
    }
}