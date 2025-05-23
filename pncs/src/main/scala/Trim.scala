import panther._

object Trim {

  def left(value: string): string = {
    var trimmed = value
    while (trimmed.nonEmpty && trimmed(0) == ' ') {
      trimmed = trimmed.substring(1)
    }

    trimmed
  }

  def right(value: string): string = {
    var trimmed = value
    while (trimmed.nonEmpty && trimmed(trimmed.length - 1) == ' ') {
      trimmed = trimmed.substring(0, trimmed.length - 1)
    }

    trimmed
  }

  def both(value: string): string = {
    var trimmed = left(value)
    trimmed = right(trimmed)
    trimmed
  }

}
