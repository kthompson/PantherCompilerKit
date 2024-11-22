import panther._

object MakeCompilation {
  def create(trees: Array[SyntaxTree]): Compilation = {

    val diagnosticBag = new DiagnosticBag()
    for (i <- 0 to (trees.length - 1)) {
      for (d <- 0 to (trees(i).diagnostics.length - 1)) {
        diagnosticBag.add(trees(i).diagnostics(d))
      }
    }
    val root = new Symbol(SymbolKind.Root, SymbolFlags.None, "", TextLocationFactory.empty(), None)

    // TODO: add default symbols to root
    // int, string, char, bool, unit, any, nothing

    val binder = new Binder(diagnosticBag, root)
    val boundTree = binder.bind(trees)

    // all the type inference data is in the checker so we cant
    // expose it to the compilation here
    val checker = new Checker(diagnosticBag, root, boundTree.functions, boundTree.fields)

  //    checker.check()

    new Compilation(trees, root, checker, diagnosticBag.diagnostics())
  }
}

case class Compilation(syntaxTrees: Array[SyntaxTree], root: Symbol, checker: Checker, diagnostics: Array[Diagnostic]) {
  def print_symbols(): unit = SymbolTreePrinter(checker).print_symbol(root)

  def emit(output: string): unit = {
    val emitter = new Emitter(syntaxTrees, root, /* checker, */ output)
    //        emitter.emit()
  }

  def transpile(outputPath: string): unit = {
    val transpiler = new Transpiler(syntaxTrees, root, outputPath)
    transpiler.transpile()
  }
}