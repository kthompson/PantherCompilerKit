import panther._

object InterpretResult {
  val Ok = 0
  val CompileError = 1
  val RuntimeError = 2
}

case class VM(chunk: Chunk) {
  var sp = 0
  var ip = 0

  def readI4(): int = {
    val value = chunk.readI4(ip)
    ip += 1
    value
  }

  def run(): int /* InterpretResult */ = {
    var result = -1
    while (result == -1) {
      val instruction = readI4()
      if(instruction == Opcode.Ret) {
        result = InterpretResult.Ok
      } else if(instruction == Opcode.LdcI4) {
        val value = readI4()
        println("ldc.i4 " + value)
      } else {
        println("Unknown opcode " + instruction)
        result = InterpretResult.RuntimeError
      }
    }

    result
  }
}
