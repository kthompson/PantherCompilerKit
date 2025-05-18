import panther._

case class StringBuilder() {
  var content = ""

  def append(value: string): unit = {
    content = content + value
  }

  def appendLine(value: string): unit = {
    content = content + value + "\n"
  }

  override def toString(): string = content
}
