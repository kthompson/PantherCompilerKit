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
    foregroundColorRGB(
      Hex.fromWithinString(hex, 0, 2),
      Hex.fromWithinString(hex, 2, 2),
      Hex.fromWithinString(hex, 4, 2)
    )

  def foregroundColorRGB(r: int, g: int, b: int): string =
    "\u001b[38;2;" + string(r) + ";" + string(g) + ";" + string(b) + "m"

  def backgroundColor(hex: string): string =
    backgroundColorRGB(
      Hex.fromWithinString(hex, 0, 2),
      Hex.fromWithinString(hex, 2, 2),
      Hex.fromWithinString(hex, 4, 2)
    )

  def backgroundColorRGB(r: int, g: int, b: int): string =
    "\u001b[48;2;" + string(r) + ";" + string(g) + ";" + string(b) + "m"
}
