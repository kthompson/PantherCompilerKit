import panther.string

object ColorPalette {

  //  Colors taken from IntelliJ IDEA's dark theme language defaults
  val Keyword = "CF8E6D"
  val Number = "2AACB8"
  val String = "6AAB73"
  val Identifier = "BCBEC4"
  val Punctuation = "BCBEC4"
  val Method = "57AAF7"
  val Field = "C77DBB"
  val Comment = "7A7E85"
  val TypeParameter = "4E807D"

  val Error = "F75464"

  val Brackets = new Array[string](3)
  Brackets(0) = "d19a66"
  Brackets(1) = "c678dd"
  Brackets(2) = "56b6c2"

//  def hex(n: int): string = Pad.left(Hex.toString(n), 2, '0')
//
//  def rgb(r: int, g: int, b: int): string = hex(r) + hex(g) + hex(b)

}
