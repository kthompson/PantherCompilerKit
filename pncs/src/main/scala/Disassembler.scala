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
    if (opcode == Opcode.Ret) {
      simpleInstruction("ret", offset)
      -1
    } else if (opcode == Opcode.Nop) {
      simpleInstruction("nop", offset)
    } else if (opcode == Opcode.Ldarg0) {
      simpleInstruction("ldarg.0", offset)
    } else if (opcode == Opcode.Ldarg1) {
      simpleInstruction("ldarg.1", offset)
    } else if (opcode == Opcode.Ldarg2) {
      simpleInstruction("ldarg.2", offset)
    } else if (opcode == Opcode.Ldarg3) {
      simpleInstruction("ldarg.3", offset)
    } else if (opcode == Opcode.Ldloc0) {
      simpleInstruction("ldloc.0", offset)
    } else if (opcode == Opcode.Ldloc1) {
      simpleInstruction("ldloc.1", offset)
    } else if (opcode == Opcode.Ldloc2) {
      simpleInstruction("ldloc.2", offset)
    } else if (opcode == Opcode.Ldloc3) {
      simpleInstruction("ldloc.3", offset)
    } else if (opcode == Opcode.Stloc0) {
      simpleInstruction("stloc.0", offset)
    } else if (opcode == Opcode.Stloc1) {
      simpleInstruction("stloc.1", offset)
    } else if (opcode == Opcode.Stloc2) {
      simpleInstruction("stloc.2", offset)
    } else if (opcode == Opcode.Stloc3) {
      simpleInstruction("stloc.3", offset)
    } else if (opcode == Opcode.LdcI4) {
      i4Instruction(chunk, offset, "ldc.i4")
      //    } else if (opcode == Opcode.Ldargn) {
      //      i4Instruction(chunk, offset, "ldarg")
      //    } else if (opcode == Opcode.Ldlocn) {
      //      i4Instruction(chunk, offset, "ldloc")
      //    } else if (opcode == Opcode.Stlocn) {
      //      i4Instruction(chunk, offset, "stloc")
    } else if (opcode == Opcode.Ldstr) {
      stringTokenInstruction(chunk, offset, "ldstr")
    } else if (opcode == Opcode.Newobj) {
      methodTokenInstruction(chunk, offset, "newobj")
    } else if (opcode == Opcode.Ldfld) {
      fieldTokenInstruction(chunk, offset, "ldfld")
    } else if (opcode == Opcode.Stfld) {
      fieldTokenInstruction(chunk, offset, "stfld")
    } else if (opcode == Opcode.Dup) {
      simpleInstruction("dup", offset)
    } else if (opcode == Opcode.Pop) {
      simpleInstruction("pop", offset)
    } else if (opcode == Opcode.Swap) {
      simpleInstruction("swap", offset)
    } else if (opcode == Opcode.Add) {
      simpleInstruction("add", offset)
    } else if (opcode == Opcode.Sub) {
      simpleInstruction("sub", offset)
    } else if (opcode == Opcode.Mul) {
      simpleInstruction("mul", offset)
    } else if (opcode == Opcode.Div) {
      simpleInstruction("div", offset)
    } else if (opcode == Opcode.Rem) {
      simpleInstruction("rem", offset)
    } else if (opcode == Opcode.Neg) {
      simpleInstruction("neg", offset)
    } else if (opcode == Opcode.And) {
      simpleInstruction("and", offset)
    } else if (opcode == Opcode.Or) {
      simpleInstruction("or", offset)
    } else if (opcode == Opcode.Xor) {
      simpleInstruction("xor", offset)
    } else if (opcode == Opcode.Not) {
      simpleInstruction("not", offset)
    } else if (opcode == Opcode.Shl) {
      simpleInstruction("shl", offset)
    } else if (opcode == Opcode.Shr) {
      simpleInstruction("shr", offset)
    } else if (opcode == Opcode.Ceq) {
      simpleInstruction("ceq", offset)
    } else if (opcode == Opcode.Cgt) {
      simpleInstruction("cgt", offset)
    } else if (opcode == Opcode.Clt) {
      simpleInstruction("clt", offset)
    } else if (opcode == Opcode.Br) {
      branchInstruction("br", offset)
    } else if (opcode == Opcode.Brfalse) {
      branchInstruction("brfalse", offset)
    } else if (opcode == Opcode.Brtrue) {
      branchInstruction("brtrue", offset)
    } else {
      panic("Unknown opcode " + string(opcode))
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
