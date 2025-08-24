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
    val assembly = if (diagnosticBag.count == 0) {
      binder.bind()
    } else {
      BoundAssembly(
        diagnosticBag.diagnostics,
        DictionaryModule.empty[Symbol, BoundExpression](),
        Option.None
      )
    }

    // don't bother lowering if there are any diagnostics
    val lowered = if (diagnosticBag.count == 0) {
      Lower.lower(assembly, binder)
    } else {
      LoweredAssembly(
        DictionaryModule.empty[Symbol, LoweredBlock](),
        Option.None
      )
    }

    new Compilation(
      trees,
      diagnosticBag.diagnostics,
      rootSymbol,
      binder,
      assembly,
      lowered
    )
  }
}

case class Compilation(
    syntaxTrees: List[SyntaxTree],
    diagnostics: Diagnostics,
    root: Symbol,
    binder: Binder,
    bound: BoundAssembly,
    assembly: LoweredAssembly
) {

  def getSymbols(): Chain[Symbol] = SymbolChain.fromList(root.members())

  def printSymbols(): unit = {
    val sb = IndentedStringBuilder(true)
    val ast = new AstPrinter(true, sb)
    val symbols = SymbolPrinter(binder, ast)
    symbols.printSymbolTree(root)
    println(sb.toString())
  }

  def printBoundAssembly(): unit = {
    val sb = IndentedStringBuilder(true)
    val ast = new AstPrinter(true, sb)
    val symbols = SymbolPrinter(binder, ast)
    val printer = new BoundAssemblyPrinter(binder, ast, symbols)
    printer.printAssembly(bound)
    println(sb.toString())
  }

  def printLoweredAssembly(): unit = {
    val sb = IndentedStringBuilder(true)
    val ast = new AstPrinter(true, sb)
    val printer = new LoweredAssemblyPrinter(binder, sb)
    printer.printAssembly(assembly)
    println(sb.toString())
  }

  def emit(output: string): unit = {
    val emitter = new Emitter(syntaxTrees, root, binder, assembly)
    emitter.emit()
  }

  def exec(): InterpretResult = {
    val emitter = new Emitter(syntaxTrees, root, binder, assembly)
    val emitResult = emitter.emit()
    val disassembler = new Disassembler(emitResult.chunk, emitResult.metadata)

    val stack = new Array[Value](CompilerSettings.defaultStackSize)
    val heap = new Array[Value](CompilerSettings.defaultHeapSize)

    if (CompilerSettings.enableTracing) {
      val metadata = emitResult.metadata
      for (i <- 0 to (metadata.methods.size - 1)) {
        val token = MethodToken(i)
        disassembler.disassembleMethod(token)
      }
      println()
    }

    val vm =
      VM(emitResult.chunk, emitResult.metadata, emitResult.entry, stack, heap)
    vm.run()
  }

  def transpile(outputPath: string): unit = {
    val transpiler = new Transpiler(syntaxTrees, outputPath)
    transpiler.transpile()
  }
}
