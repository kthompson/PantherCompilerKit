import panther._
import system.io._

case class TextSpan(start: int, length: int) {
    val end: int = start + length
}

object TextSpanFactory {
    def fromBounds(start: int, end: int): TextSpan = new TextSpan(start, end - start)
}

case class TextLine(start: int, length: int, length_with_line_breaks: int) {
    val end: int = start + length + 1
    val span: TextSpan = new TextSpan(start, length)
}

case class TextLineList() {
    var _size: int = 0
    var _items: Array[TextLine] = new Array[TextLine](0)

    def ensure_capacity(count: int): unit = {
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
        ensure_capacity(1)
        _items(_size) = line
        _size = _size + 1
    }

    def text_lines(): Array[TextLine] = {
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
            val lineBreakWidth = get_line_break_width(content, position)
            if (lineBreakWidth == 0) {
                position = position + 1
            } else {
                add_line(list, position, lineStart, lineBreakWidth)
                position = position + lineBreakWidth
                lineStart = position
            }
        }

        if (position >= lineStart) {
            add_line(list, position, lineStart, 0)
        } else {

        }

        list.text_lines()
    }

    def get_line_break_width(text: string, position: int): int = {
        val c = text(position)
        val l = if (position + 1 >= text.length) '\u0000' else text(position + 1)

        if (c == '\r' && l == '\n') 2
        else if (c == '\r' || l == '\n') 1
        else 0
    }

    def add_line(list: TextLineList, position: int, lineStart: int, lineBreakWidth: int): unit = {
        val lineLength = position - lineStart
        val lineLengthIncludingLineBreak = lineStart + lineBreakWidth
        val line = new TextLine(lineStart, lineLength, lineLengthIncludingLineBreak)
        list.add(line)
    }
}

case class SourceFile(content: string, fileName: string) {
    val _lines: Array[TextLine] = TextLineParser.parse(content)

    val line_count: int = _lines.length
    val length: int = content.length

    def get(position: int): char =
        if (position >= length) '\u0000'
        else content(position)

    def line_to_offset(index: int): int = _lines(index).start
    def get_line(index: int): TextLine = _lines(index)

    // get the line index from a position
    def get_line_index(position: int): int =
        _get_line_index(position, 0, line_count - 1)

    def isScala(): bool = fileName.endsWith(".scala")

    def _get_line_index(position: int, lower: int, upper: int): int =
        if (lower <= upper ) {
            val index = lower + (upper - lower) / 2
            val mid = _lines(index)
            val start = mid.start
            val end = mid.end

            if (position > end) {
                _get_line_index(position, index + 1, upper)
            } else if (position < start) {
                _get_line_index(position, lower, index - 1)
            } else {
                index
            }
        } else {
            lower - 1
        }

    def to_string(span: TextSpan): string = to_string(span.start, span.length)
    def to_string(start: int, length: int): string =
        if (start + length > content.length) {
            content.substring(start)
        } else {
            content.substring(start, start + length)
        }
}

object MakeSourceFile {
    def from_file(filename: string): SourceFile = {
        val contents = File.readAllText(filename)
        new SourceFile(contents, filename)
    }

    def from_content(contents: string): SourceFile =
        new SourceFile(contents, "")

    def empty(): SourceFile = from_content("")
}

case class TextLocation(source_file: SourceFile, span: TextSpan) {
    val file_name: string = source_file.fileName
    val start_line: int = source_file.get_line_index(span.start)
    val start_character: int = span.start - source_file.line_to_offset(start_line)
    val end_line: int = source_file.get_line_index(span.end)
    val end_character: int = span.end - source_file.line_to_offset(end_line)

    def to_string(): string =
        file_name + "(" + string(start_line + 1) + "," + string(start_character + 1) + ")"
        
    def compareTo(other: TextLocation): int = {
        val i = source_file.fileName.compareTo(other.source_file.fileName)
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