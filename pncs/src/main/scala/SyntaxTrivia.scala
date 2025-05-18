import panther._

case class SyntaxTrivia(kind: int, start: int, text: string) {
  def isStatementTerminator(): bool = kind == SyntaxKind.EndOfLineTrivia
}
