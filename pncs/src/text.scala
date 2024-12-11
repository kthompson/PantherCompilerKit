import panther._
import system.io._

case class TextSpan(start: int, length: int) {
    val end: int = start + length
}

object TextSpanFactory {
    def fromBounds(start: int, end: int): TextSpan = new TextSpan(start, end - start)
}

case class TextLine(start: int, length: int, lengthWithLineBreaks: int) {
    val end: int = start + length + 1
    val span: TextSpan = new TextSpan(start, length)
}

case class TextLineList() {
    var _size: int = 0
    var _items: Array[TextLine] = new Array[TextLine](0)

    def ensureCapacity(count: int): unit = {
        if (_size + count >= _items.length) {
            val newItems = new Array[TextLine]((_size + count) * 2)
            for (i <- 0 to (_size-1)) {
                newItems(i) = _items(i)
            }
            _items = newItems
        } else {
            ()
        }
    }

    def add(line: TextLine): unit = {
        ensureCapacity(1)
        _items(_size) = line
        _size = _size + 1
    }

    def textLines(): Array[TextLine] = {
        var newItems = new Array[TextLine](_size)
        for (i <- 0 to (_size-1)) {
            newItems(i) = _items(i)
        }
        newItems
    }
}

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
        } else {

        }

        list.textLines()
    }

    def getLineBreakWidth(text: string, position: int): int = {
        val c = text(position)
        val l = if (position + 1 >= text.length) '\u0000' else text(position + 1)

        if (c == '\r' && l == '\n') 2
        else if (c == '\r' || l == '\n') 1
        else 0
    }

    def addLine(list: TextLineList, position: int, lineStart: int, lineBreakWidth: int): unit = {
        val lineLength = position - lineStart
        val lineLengthIncludingLineBreak = lineStart + lineBreakWidth
        val line = new TextLine(lineStart, lineLength, lineLengthIncludingLineBreak)
        list.add(line)
    }
}

case class SourceFile(content: string, fileName: string) {
    val _lines: Array[TextLine] = TextLineParser.parse(content)

    val lineCount: int = _lines.length
    val length: int = content.length

    def get(position: int): char =
        if (position >= length) '\u0000'
        else content(position)

    def lineToOffset(index: int): int = _lines(index).start
    def getLine(index: int): TextLine = _lines(index)

    // get the line index from a position
    def getLineIndex(position: int): int =
        _getLineIndex(position, 0, lineCount - 1)

    def isScala(): bool = fileName.endsWith(".scala")

    def _getLineIndex(position: int, lower: int, upper: int): int =
        if (lower <= upper ) {
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

    def toString(span: TextSpan): string = toString(span.start, span.length)
    def toString(start: int, length: int): string =
        if (start + length > content.length) {
            content.substring(start)
        } else {
            content.substring(start, start + length)
        }
}

object MakeSourceFile {
    def fromFile(filename: string): SourceFile = {
        val contents = File.readAllText(filename)
        new SourceFile(contents, filename)
    }

    def fromContent(contents: string): SourceFile =
        new SourceFile(contents, "")

    def empty(): SourceFile = fromContent("")
}

case class TextLocation(sourceFile: SourceFile, span: TextSpan) {
    val fileName: string = sourceFile.fileName
    val startLine: int = sourceFile.getLineIndex(span.start)
    val startCharacter: int = span.start - sourceFile.lineToOffset(startLine)
    val endLine: int = sourceFile.getLineIndex(span.end)
    val endCharacter: int = span.end - sourceFile.lineToOffset(endLine)

    override def toString(): string =
        fileName + "(" + string(startLine + 1) + "," + string(startCharacter + 1) + ")"
        
    def compareTo(other: TextLocation): int = {
        val i = sourceFile.fileName.compareTo(other.sourceFile.fileName)
        if (i != 0) i
        else {
            val j = span.start.compareTo(other.span.start)
            if (j != 0) j
            else span.end.compareTo(other.span.end)
        }
    }
}

object TextLocationFactory {
    def empty(): TextLocation =
        new TextLocation(MakeSourceFile.empty(), new TextSpan(0, 0))
}