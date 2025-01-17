import panther._

case class Diagnostic(location: TextLocation, message: string) {
    override def toString(): string = location.toString() + ": " + message
    def compareTo(other: Diagnostic): int = {
      val i = location.compareTo(other.location)
      if(i != 0) i
      else message.compareTo(other.message)
    }
}

enum Diagnostics {
  case Empty
  case Node(left: Diagnostics, head: Diagnostic, tail: Diagnostics)
}

case class DiagnosticBag() {
  var count: int = 0
  var diagnostics: Diagnostics = Diagnostics.Empty

  def report(location: TextLocation, message: string): unit = add(new Diagnostic(location, message))

  def reportSymbolNotDefined(location: TextLocation, name: string): unit =
    report(location, "Symbol " + name + " not defined")

  def reportTypeNotDefined(location: TextLocation, name: string): unit =
    report(location, "Type " + name + " not defined")

  def reportTopLevelStatementsInMultipleFiles(firstLocation: TextLocation, secondLocation: TextLocation): unit =
    report(firstLocation, "Top-level statements in multiple files " + firstLocation.toString() + " and " + secondLocation.toString())
    
  def reportDuplicateDefinition(name: string, first: TextLocation, second: TextLocation): unit =
    report(first, "Duplicate definition of " + name + " at " + second.toString())
  
  def reportMultipleEntryPoints(first: TextLocation, second: TextLocation): unit = 
    report(first, "Multiple entry points in files " + first.toString() + " and " + second.toString())

  def reportInvalidNamespace(location: TextLocation): unit =
    report(location, "Invalid namespace")
  
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

  def add(diagnostic: Diagnostic): unit = {
    count = count + 1
    diagnostics = _insert(diagnostics, diagnostic)
  }

  def _insert(node: Diagnostics, diagnostic: Diagnostic): Diagnostics = {
    node match {
      case Diagnostics.Empty => Diagnostics.Node(Diagnostics.Empty, diagnostic, Diagnostics.Empty)
      case Diagnostics.Node(left, head, tail) =>
        val cmp = diagnostic.compareTo(head)
        if (cmp == 0) {
          node
        } else if (diagnostic.compareTo(head) < 0) {
          Diagnostics.Node(_insert(left, diagnostic), head, tail)
        } else {
          Diagnostics.Node(left, head, _insert(tail, diagnostic))
        }
    }
  }

  def addDiagnostics(more: Diagnostics): unit = more match {
    case Diagnostics.Empty => ()
    case Diagnostics.Node(left, head, right) =>
      add(head)
      addDiagnostics(left)
      addDiagnostics(right)
  }
}
