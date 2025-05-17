import panther.{int, string}

case class SimpleToken(
    kind: int,
    start: int,
    text: string,
    value: SyntaxTokenValue
)
