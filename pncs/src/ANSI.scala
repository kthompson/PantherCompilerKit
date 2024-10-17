import panther._

object ANSI {
  val Red = "\u001b[0;31m"
  val Clear = "\u001b[0m"


  // keyword c678dd
  // Identifier = e5c07b
  // whitespace = abb2bf
  // = 56b6c2
  // string #98c379

  def foregroundColor(hex: string): string =
    foregroundColor(Hex.fromString(hex, 0, 2), Hex.fromString(hex, 2, 2), Hex.fromString(hex, 4, 2))

  def foregroundColor(r: int, g: int, b: int): string =
    "\u001b[38;2;" + string(r) + ";" + string(g) + ";" + string(b) + "m"

  def backgroundColor(hex: string): string =
    backgroundColor(Hex.fromString(hex, 0, 2), Hex.fromString(hex, 2, 2), Hex.fromString(hex, 4, 2))

  def backgroundColor(r: int, g: int, b: int): string =
    "\u001b[48;2;" + string(r) + ";" + string(g) + ";" + string(b) + "m"
}
