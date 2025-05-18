import panther.string

object MakeSyntaxTree {
  def parseSourceFile(file: SourceFile): SyntaxTree = {
    val diagnosticBag = new DiagnosticBag()
    val parser = new Parser(file, diagnosticBag)
    val root = parser.parseCompilationUnit()

    new SyntaxTree(file, root, diagnosticBag.diagnostics)
  }

  def parseFile(filename: string): SyntaxTree =
    parseSourceFile(MakeSourceFile.fromFile(filename))

  def parseContent(content: string): SyntaxTree =
    parseSourceFile(MakeSourceFile.fromContent(content))
}
