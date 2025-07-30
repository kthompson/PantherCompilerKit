import panther._

case class StringBuilder() {
  var content: string = ""

  def appendChar(value: char): unit = {
    content = content + value.toString()
  }

  def append(value: string): unit = {
    content = content + value
  }

  def appendLine(value: string): unit = {
    content = content + value + "\n"
  }

  override def toString(): string = content
}
