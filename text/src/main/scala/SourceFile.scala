import panther._

case class SourceFile(content: string, fileName: string) {
  val _lines: Array[TextLine] = TextLineParser.parse(content)

  val lineCount: int = _lines.length
  val length: int = content.length

  def get(position: int): char =
    if (position >= length) '\u0000'
    else content(position)

  def lineToOffset(index: int): int = _lines(index).start
  def getLine(index: int): TextLine = _lines(index)

  def getLineNumber(line: TextLine): int =
    getLineIndex(line.start) + 1

  // get the line index from a position
  def getLineIndex(position: int): int =
    _getLineIndex(position, 0, lineCount - 1)

  def isScala(): bool = fileName.endsWith(".scala")

  def _getLineIndex(position: int, lower: int, upper: int): int =
    if (lower <= upper) {
      val index = lower + (upper - lower) / 2
      val mid = _lines(index)
      val start = mid.start
      val end = mid.end

      if (position > end) {
        _getLineIndex(position, index + 1, upper)
      } else if (position < start) {
        _getLineIndex(position, lower, index - 1)
      } else {
        index
      }
    } else {
      lower - 1
    }

  def substringFromSpan(span: TextSpan): string =
    substring(span.start, span.length)

  def substring(start: int, length: int): string =
    if (start + length > content.length) {
      content.substring(start)
    } else {
      content.substring(start, start + length)
    }

  override def toString(): string = "SourceFile(" + fileName + ")"
}
