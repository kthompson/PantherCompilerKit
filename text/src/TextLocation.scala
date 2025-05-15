import panther._

case class TextLocation(sourceFile: SourceFile, span: TextSpan) {
  val fileName: string = sourceFile.fileName
  val startLine: int = sourceFile.getLineIndex(span.start)
  val startCharacter: int = span.start - sourceFile.lineToOffset(startLine)
  val endLine: int = sourceFile.getLineIndex(span.end)
  val endCharacter: int = span.end - sourceFile.lineToOffset(endLine)

  override def toString(): string =
    fileName + "(" + string(startLine + 1) + "," + string(
      startCharacter + 1
    ) + ")"

  def compareTo(other: TextLocation): int = {
    val i = sourceFile.fileName.compareTo(other.sourceFile.fileName)
    if (i != 0) i
    else {
      val j = span.start.compareTo(other.span.start)
      if (j != 0) j
      else span.end.compareTo(other.span.end)
    }
  }

  def merge(location: TextLocation): TextLocation = {
    val newSpan = span.merge(location.span)
    new TextLocation(sourceFile, newSpan)
  }
}
