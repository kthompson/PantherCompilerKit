import panther._

enum SyntaxTokenValue {
  case Boolean(value: bool)
  case String(value: string)
  case Character(value: char)
  case Number(value: int)
  case None

  case Error
}
