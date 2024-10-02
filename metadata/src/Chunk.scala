import panther._

case class Chunk() {
  // TODO: we don't currently support byte types so we will implement this with ints

  // line number for each instruction
  var lines: Array[int] = new Array[int](0)
  // the actual instructions
  var content: Array[int] = new Array[int](0)
  var capacity = 0
  var size = 0

  def ensureSpace(n: int): unit = {
    if (size + n > capacity) {
      var newCapacity = if (capacity == 0) 16 else capacity * 2
      while (size + n > newCapacity) {
        newCapacity = newCapacity * 2
      }
      val newContent = new Array[int](newCapacity)
      for (i <- 0 to (size - 1)) {
        newContent(i) = content(i)
      }

      val newLines = new Array[int](newCapacity)
      for (i <- 0 to (size - 1)) {
        newLines(i) = lines(i)
      }

      lines = newLines
      content = newContent
      capacity = newCapacity
    } else ()
  }

  def emitOpcode(opcode: int, line: int): unit = {
    emitI4(opcode, line)
  }

  def emitI4(value: int, line: int): unit = {
    ensureSpace(1)
    content(size) = (value)
    lines(size) = line
    size = size + 1
  }

  def readI4(offset: int): int = content(offset)
  def readLine(offset: int): int = lines(offset)
}