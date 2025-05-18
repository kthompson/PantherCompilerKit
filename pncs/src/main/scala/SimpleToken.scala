import panther._

case class SimpleToken(
    kind: int,
    start: int,
    text: string,
    value: SyntaxTokenValue
)
