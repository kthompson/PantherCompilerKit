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

    // don't bother lowering if there are any diagnostics
    val lowered = if (diagnosticBag.count == 0) {
      val assembly = binder.bind()

      if (CompilerSettings.printBoundAssembly) {
        val boundAssembly = BoundAssembly(
          assembly.diagnostics,
          assembly.functionBodies,
          assembly.entryPoint
        )
        val sb = IndentedStringBuilder()
        val ast = new AstPrinter(true, sb)
        val sym = SymbolPrinter(binder, ast)
        val printer = new BoundAssemblyPrinter(binder, ast, sym)
        printer.printAssembly(boundAssembly)
        println(sb.toString())
      }

      if (diagnosticBag.count == 0) Lower.lower(assembly, binder)
      else emptyLoweredAssembly()
    } else {
      emptyLoweredAssembly()
    }

    if (CompilerSettings.printLoweredAssembly) {
      val sb = IndentedStringBuilder()
      val ast = new AstPrinter(true, sb)
      val sym = SymbolPrinter(binder, ast)
      val printer = new LoweredAssemblyPrinter(binder, sb)
      printer.printAssembly(lowered)
      println(sb.toString())
    }

    new Compilation(
      trees,
      diagnosticBag.diagnostics,
      rootSymbol,
      binder,
      lowered
    )
  }

  def emptyLoweredAssembly(): LoweredAssembly = {
    LoweredAssembly(
      DictionaryModule.empty[Symbol, LoweredBlock](),
      Option.None
    )
  }
}

case class Compilation(
    syntaxTrees: List[SyntaxTree],
    diagnostics: Diagnostics,
    root: Symbol,
    binder: Binder,
    assembly: LoweredAssembly
) {
  def getSymbols(): Chain[Symbol] = SymbolChain.fromList(root.members())

  def printSymbols(): unit = {
    val sb = IndentedStringBuilder()
    val printer = new AstPrinter(true, sb)
    val symbols = SymbolPrinter(binder, printer)
    symbols.printSymbol(root)
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
