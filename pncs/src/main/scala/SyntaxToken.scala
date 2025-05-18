import panther._

case class SyntaxToken(
    sourceFile: SourceFile,
    kind: int,
    start: int,
    text: string,
    value: SyntaxTokenValue,
    leading: Array[SyntaxTrivia],
    trailing: Array[SyntaxTrivia]
) {
  val span: TextSpan = new TextSpan(start, text.length)
  val location: TextLocation = new TextLocation(sourceFile, span)

  def isStatementTerminator(): bool =
    if (
      kind == SyntaxKind.EndOfInputToken || kind == SyntaxKind.CloseBraceToken
    ) true
    else _isStatementTerminator(0)

  def _isStatementTerminator(index: int): bool =
    if (index >= trailing.length) {
      false
    } else if (trailing(index).isStatementTerminator()) {
      true
    } else _isStatementTerminator(index + 1)
}
