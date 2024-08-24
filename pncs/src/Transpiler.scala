import panther._
import system.io._

case class Transpiler(syntaxTrees: Array[SyntaxTree], root: Symbol, outputPath: string) {
    def transpile(): unit = {
        for (x <- 0 to (syntaxTrees.length - 1)) {
          val tree = syntaxTrees(x)
          val sourceFile = tree.file

          val name = Path.nameWithoutExtension(sourceFile.fileName) + ".pn"
          val filePath = Path.combine(outputPath, name)

          if(!sourceFile.isScala()) {
            // skip transpiling as we are already in the target language
            File.write_all_text(filePath, sourceFile.content)
          } else {
            // transpile
            transpileRoot(tree.root, filePath)
          }
        }
    }

  def transpileRoot(root: CompilationUnitSyntax, filePath: string) = ???
}
