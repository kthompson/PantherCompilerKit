import panther._

object Pad {

  def right(value: string, len: int, padChar: char): string = {
    var padded = value
    while (padded.length < len) {
      padded = padded + string(padChar)
    }

    padded
  }

  def left(value: string, len: int, padChar: char): string = {
    var padded = value
    while (padded.length < len) {
      padded = string(padChar) + padded
    }

    padded
  }

}