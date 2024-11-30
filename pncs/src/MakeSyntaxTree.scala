import panther.string

object MakeSyntaxTree {
  def parse_source_file(file: SourceFile): SyntaxTree = {
    val diagnosticBag = new DiagnosticBag()
    val parser = new Parser(file, diagnosticBag)
    val root = parser.parse_compilation_unit()

    new SyntaxTree(file, root, diagnosticBag.diagnostics)
  }

  def parse_file(filename: string): SyntaxTree =
    parse_source_file(MakeSourceFile.from_file(filename))

  def parse_content(content: string): SyntaxTree =
    parse_source_file(MakeSourceFile.from_content(content))
}
