import panther._

object TextLineParser {
  def parse(content: string): Array[TextLine] = {
    val list = new TextLineList()
    var position = 0
    var lineStart = 0

    while (position < content.length) {
      val lineBreakWidth = getLineBreakWidth(content, position)
      if (lineBreakWidth == 0) {
        position = position + 1
      } else {
        addLine(list, position, lineStart, lineBreakWidth)
        position = position + lineBreakWidth
        lineStart = position
      }
    }

    if (position >= lineStart) {
      addLine(list, position, lineStart, 0)
    } else {}

    list.textLines()
  }

  def getLineBreakWidth(text: string, position: int): int = {
    val c = text(position)
    val l = if (position + 1 >= text.length) '\u0000' else text(position + 1)

    if (c == '\r' && l == '\n') 2
    else if (c == '\r' || l == '\n') 1
    else 0
  }

  def addLine(
      list: TextLineList,
      position: int,
      lineStart: int,
      lineBreakWidth: int
  ): unit = {
    val lineLength = position - lineStart
    val lineLengthIncludingLineBreak = lineStart + lineBreakWidth
    val line = new TextLine(lineStart, lineLength, lineLengthIncludingLineBreak)
    list.add(line)
  }
}
