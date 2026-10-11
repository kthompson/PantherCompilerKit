import panther._
import system.io._

/** User compilation sources, kept separate from raw compiler-stage tests. */
object Stdlib {
  def load(settings: CompilerSettings): Result[string, List[SyntaxTree]] = {
    var trees: List[SyntaxTree] = List.Nil
    var error = ""
    var index = 0
    if (settings.useStdlib && !settings.transpile) {
      while (index < BundledStdlib.count && error == "") {
        val name = BundledStdlib.name(index)
        if (settings.stdlibDirectory == "") {
          val source = new SourceFile(
            BundledStdlib.source(index),
            "<stdlib>/" + name + ".pn"
          )
          trees =
            List.Cons(MakeSyntaxTree.parseSourceFile(source, settings), trees)
        } else {
          val path = Path.combine(settings.stdlibDirectory, name + ".pn")
          if (File.exists(path)) {
            trees = List.Cons(MakeSyntaxTree.parseFile(path, settings), trees)
          } else {
            error = "Standard library source not found: " + path
          }
        }
        index = index + 1
      }
    }
    if (error == "") Result.Success(trees.reverse())
    else Result.Error(error)
  }

  def withSources(
      trees: List[SyntaxTree],
      settings: CompilerSettings
  ): Result[string, List[SyntaxTree]] = load(settings) match {
    case Result.Error(message) => Result.Error(message)
    case Result.Success(library) =>
      Result.Success(ListModule.concat(library, trees))
  }
}
