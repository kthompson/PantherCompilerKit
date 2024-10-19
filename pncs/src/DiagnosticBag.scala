import panther._

case class Diagnostic(location: TextLocation, message: string) {
    override def toString(): string = location.to_string() + ": " + message
}

case class DiagnosticBag() {
  var _size: int = 0
  var _items: Array[Diagnostic] = new Array[Diagnostic](0)

  def report(location: TextLocation, message: string): unit = add(new Diagnostic(location, message))

  def reportTypeNotDefined(location: TextLocation, name: string): unit = report(location, "Type " + name + " not defined")
  
  def reportBadCharacter(location: TextLocation, value: char): unit = report(location, "Invalid character in input: " + string(value))

  def reportEmptyCharLiteral(location: TextLocation): unit = report(location, "Empty character literal")

  def reportUnterminatedBlockComment(location: TextLocation): unit = report(location, "Unterminated block comment")

  def reportUnterminatedChar(location: TextLocation): unit = report(location, "Unterminated character literal")

  def reportUnterminatedString(location: TextLocation): unit = report(location, "Unterminated string literal")

  def reportExpectedExpression(location: TextLocation, currentKind: int): unit =
    report(location, "Unexpected token " + SyntaxFacts.getKindName(currentKind) + ", expected expression")

  def reportUnexpectedToken(location: TextLocation, currentKind: int, expectedKind: int): unit =
    report(location, "Unexpected token " + SyntaxFacts.getKindName(currentKind) + ", expected " + SyntaxFacts.getKindName(expectedKind))

  def reportInvalidEscapeSequence(location: TextLocation, current: char): unit =
    report(location, "Invalid character in escape sequence: " + string(current))

  def reportExpectedPattern(location: TextLocation, currentKind: int): unit =
    report(location, "Unexpected token " + SyntaxFacts.getKindName(currentKind) + ", expected pattern")
  
  def ensureCapacity(count: int): unit = {
    if (_size + count >= _items.length) {
      val newItems = new Array[Diagnostic]((_size + count) * 2)
      for (i <- 0 to (_size - 1)) {
        newItems(i) = _items(i)
      }
      _items = newItems
    } else {
      ()
    }
  }

  def add(diagnostic: Diagnostic): unit = {
    ensureCapacity(1)
    _items(_size) = diagnostic
    _size = _size + 1
  }

  def diagnostics(): Array[Diagnostic] = {
    val newItems = new Array[Diagnostic](_size)
    for (i <- 0 to (_size - 1)) {
      newItems(i) = _items(i)
    }
    newItems
  }
}
