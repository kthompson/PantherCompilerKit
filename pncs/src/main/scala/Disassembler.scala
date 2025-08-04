import panther._

case class Disassembler(chunk: Chunk, metadata: Metadata) {
  var extra = ""

  def disassembleMethod(token: MethodToken): bool = {
    val method = metadata.methods.get(token)
    if (method.address == -1) {
      false
    } else {
      var offset = method.address

      println(metadata.getMethodName(token) + ":")
      while (offset < chunk.size && offset != -1) {
        offset = disassembleInstruction(offset)
      }
      println()
      true
    }
  }

  def disassembleInstruction(offset: int): int = {
    // offset
    print(Pad.left(Hex.toString(offset), 4, '0') + " ")

    // extra
    print(extra)

    // line number
    if (offset > 0 && chunk.readLine(offset) == chunk.readLine(offset - 1)) {
      print("   | ")
    } else {
      val line = chunk.readLine(offset)
      val lineStr = Pad.left(line.toString, 4, ' ')
      print(lineStr + " ")
    }

    // opcode
    val opcode = chunk.readI4(offset)
    val name = Opcode.nameOf(opcode)
    if (opcode == Opcode.Ret) {
      simpleInstruction(name, offset)
      -1
    } else if (opcode == Opcode.Nop) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Ldarg0) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Ldarg1) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Ldarg2) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Ldarg3) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Ldloc0) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Ldloc1) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Ldloc2) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Ldloc3) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Stloc0) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Stloc1) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Stloc2) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Stloc3) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.LdcI4) {
      i4Instruction(chunk, offset, name)
      //    } else if (opcode == Opcode.Ldargn) {
      //      i4Instruction(chunk, offset, name)
      //    } else if (opcode == Opcode.Ldlocn) {
      //      i4Instruction(chunk, offset, name)
      //    } else if (opcode == Opcode.Stlocn) {
      //      i4Instruction(chunk, offset, name)
    } else if (opcode == Opcode.Ldstr) {
      stringTokenInstruction(chunk, offset, name)
    } else if (opcode == Opcode.Newobj) {
      methodTokenInstruction(chunk, offset, name)
    } else if (opcode == Opcode.Call) {
      methodTokenInstruction(chunk, offset, name)
    } else if (opcode == Opcode.Ldfld) {
      fieldTokenInstruction(chunk, offset, name)
    } else if (opcode == Opcode.Ldsfld) {
      fieldTokenInstruction(chunk, offset, name)
    } else if (opcode == Opcode.Stfld) {
      fieldTokenInstruction(chunk, offset, name)
    } else if (opcode == Opcode.Stsfld) {
      fieldTokenInstruction(chunk, offset, name)
    } else if (opcode == Opcode.Dup) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Pop) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Swap) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Add) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Sub) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Mul) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Div) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Rem) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Neg) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.And) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Or) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Xor) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Not) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Shl) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Shr) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Ceq) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Cgt) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Clt) {
      simpleInstruction(name, offset)
    } else if (opcode == Opcode.Br) {
      branchInstruction(name, offset)
    } else if (opcode == Opcode.Brfalse) {
      branchInstruction(name, offset)
    } else if (opcode == Opcode.Brtrue) {
      branchInstruction(name, offset)
    } else {
      panic("Unsupported opcode " + Opcode.nameOf(opcode))
      offset + 1
    }
  }

  def i4Instruction(chunk: Chunk, offset: int, name: string): int = {
    val value = chunk.readI4(offset + 1)
    println(name + " " + string(value))
    offset + 2
  }

  def branchInstruction(name: string, offset: int): int = {
    val target = chunk.readI4(offset + 1)
    println(name + " " + Hex.toString(target))
    offset + 2
  }

  def simpleInstruction(name: string, offset: int): int = {
    println(name)
    offset + 1
  }

  def fieldTokenInstruction(chunk: Chunk, offset: int, name: string): int = {
    val token = chunk.readI4(offset + 1)
    val fieldName = metadata.getFieldName(FieldToken(token))
    println(name + " " + fieldName)
    offset + 2
  }

  def methodTokenInstruction(chunk: Chunk, offset: int, name: string): int = {
    val token = chunk.readI4(offset + 1)
    val methodName = metadata.getMethodName(MethodToken(token))
    println(name + " " + methodName)
    offset + 2
  }

  def stringTokenInstruction(chunk: Chunk, offset: int, name: string): int = {
    val token = chunk.readI4(offset + 1)
    val str = metadata.getString(StringToken(token))
    println(name + " \"" + str + "\"")
    offset + 2
  }
}
