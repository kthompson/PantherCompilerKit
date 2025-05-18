import panther._

object MakeCompilation {
  def create(trees: List[SyntaxTree]): Compilation = {
    val diagnosticBag = new DiagnosticBag()
    var diagTrees = trees
    while (diagTrees != List.Nil) {
      diagTrees match {
        case List.Nil => ()
        case List.Cons(head, tail) =>
          diagnosticBag.addDiagnostics(head.diagnostics)
          diagTrees = tail
      }
    }

    val rootSymbol = Symbol(
      "",
      TextLocationFactory.empty(),
      SymbolKind.Namespace,
      Option.None
    )
    // creation of the Binder will initialize all of the builtin symbols and their types into the
    // root symbol table
    val binder = new Binder(trees, rootSymbol, diagnosticBag)

    val assembly = binder.bind()

    new Compilation(
      trees,
      diagnosticBag.diagnostics,
      rootSymbol,
      binder,
      assembly
    )
  }
}

case class Compilation(
    syntaxTrees: List[SyntaxTree],
    diagnostics: Diagnostics,
    root: Symbol,
    binder: Binder,
    assembly: BoundAssembly
) {
  def getSymbols(): Chain[Symbol] = SymbolChain.fromList(root.members())

  def printSymbols(): unit = SymbolTreePrinter(binder).printSymbol(root)

  def emit(output: string): unit = {
    val emitter = new Emitter(syntaxTrees, root, assembly /*, checker, */ )
//    emitter.emit()
  }

  def exec(): InterpretResult = {
    val emitter = new Emitter(syntaxTrees, root, assembly /*, checker */ )
    val emitResult = emitter.emit()

    val stack = new Array[Value](CompilerSettings.defaultStackSize)
    val heap = new Array[Value](CompilerSettings.defaultHeapSize)

    val vm = VM(emitResult.chunk, emitResult.metadata, stack, heap)
    vm.run()
  }

  def transpile(outputPath: string): unit = {
    val transpiler = new Transpiler(syntaxTrees, outputPath)
    transpiler.transpile()
  }
}
