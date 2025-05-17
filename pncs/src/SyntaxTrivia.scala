import panther.{bool, int, string}

case class SyntaxTrivia(kind: int, start: int, text: string) {
  def isStatementTerminator(): bool = kind == SyntaxKind.EndOfLineTrivia
}
