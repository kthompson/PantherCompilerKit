import panther._

case class IndentedStringBuilder() {
  val indentValue: string = "  "

  var currentIndent: string = ""
  var sb: StringBuilder = StringBuilder()
  var needsIndent: bool = false

  def appendChar(value: char): unit = {
    if (needsIndent) {
      sb.append(currentIndent)
      needsIndent = false
    }

    if (value == '\n') {
      needsIndent = true
    }

    sb.appendChar(value)
  }

  def appendString(str: string, index: int, count: int): unit = {
    if (str.length > 0 && index >= 0 && index < str.length && count > 0) {
      appendChar(str(index))
      appendString(str, index + 1, count - 1)
    }
  }

  def append(value: string): unit = appendString(value, 0, value.length)

  def appendLine(value: string): unit =
    append(value + "\n")

  def indent(): unit = {
    currentIndent = currentIndent + indentValue
  }

  def unindent(): unit = {
    if (currentIndent.length >= indentValue.length) {
      currentIndent =
        currentIndent.substring(0, currentIndent.length - indentValue.length)
    }
  }

  override def toString(): string = sb.toString()
}
