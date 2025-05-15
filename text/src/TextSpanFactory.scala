import panther.int

object TextSpanFactory {
  def fromBounds(start: int, end: int): TextSpan =
    new TextSpan(start, end - start)
}
