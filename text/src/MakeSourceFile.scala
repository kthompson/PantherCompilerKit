import panther.string
import system.io.File

object MakeSourceFile {
  def fromFile(filename: string): SourceFile = {
    val contents = File.readAllText(filename)
    new SourceFile(contents, filename)
  }

  def fromContent(contents: string): SourceFile =
    new SourceFile(contents, "")

  def empty(): SourceFile = fromContent("")
}
