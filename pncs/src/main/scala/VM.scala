import panther._

enum InterpretResult {
  case Continue // will continue running
  case Ok // completed running the program
  case OkValue(
      value: Value
  ) // completed running the program and returned a value
  case CompileError
  case RuntimeError
}

/*
 Stack Frame structure:

  +------------------+
  |  Local Variables |
  |  (n values)      |
  +------------------+
  |  Return Address  |
  |  (4 bytes)       |
  +------------------+
  |  Saved FP        |
  |  (4 bytes)       |
  +------------------+
  |  Parameters      |
  |  (n values)      |
  +------------------+

the return address is the address of the next instruction after the call
the saved FP is the value of the FP register when the function was called
  this is used to restore the FP register when the function returns

a call instruction pushes values onto the stack in the following order:
  1. parameters
  2. saved FP (current FP)
  3. return address (ip + 1)
  4. local variables

a ret instruction pops values off the stack in the following order:
  1. local variables
  2. saved FP
  3. return address
  4. parameters
 */

case class VM(
    chunk: Chunk,
    metadata: Metadata,
    entry: Option[MethodToken],
    stack: Array[Value],
    heap: Array[Value],
    settings: CompilerSettings
) {
  var sp = 0 // stack pointer
  var ip = 0 // instruction pointer
  var argsp = 0 // argument stack pointer
  var localp = 0 // local variable pointer
  var heapp = 0 // heap pointer

  // static field pointer (portion of the heap)
  var staticp = 0

  val disassembler = new Disassembler(chunk, metadata)

  // setup metadata (this should be the emitter's job)
  // 1. setup some space for static fields
  //    a. count the number of static fields, and assign them an index
  //    b. record number of static fields in the Chunk header

  def setupHeap(): unit = {
    // allocate space on the heap for static fields
    alloc(metadata.statics())
  }

  // allocates space on the heap for an object
  def alloc(size: int): int = {
    if (heapp + size > heap.length) {
      panic("Heap overflow")
    }
    val addr = heapp
    heapp = heapp + size
    addr
  }

  def readI4(): int = {
    val value = chunk.readI4(ip)
    ip = ip + 1
    value
  }

  def readStringToken(): StringToken = {
    val value = readI4()
    StringToken(value)
  }

  def readMethodToken(): MethodToken = {
    val value = readI4()
    MethodToken(value)
  }

  def readFieldToken(): FieldToken = {
    val value = readI4()
    FieldToken(value)
  }

  def readTypeDefToken(): TypeDefToken = {
    val value = readI4()
    TypeDefToken(value)
  }

  def push(value: Value): InterpretResult = {
    stack(sp) = value
    sp = sp + 1
    InterpretResult.Continue
  }

  def pushBool(value: bool): InterpretResult =
    push(Value.Bool(value))

  def binaryAdd(): InterpretResult = {
    val b = pop()
    val a = pop()
    a match {
      case Value.Int(a) =>
        b match {
          case Value.Int(b) => push(Value.Int(a + b))
          case _            => runtimeError("Expected int on stack")
        }
      case Value.String(a) =>
        b match {
          case Value.String(b) => push(Value.String(a + b))
          case _               => runtimeError("Expected bool on stack")
        }
      case _ => runtimeError("Expected int or bool on stack")
    }
  }

  def pop(): Value = {
    sp = sp - 1
    stack(sp)
  }

  def runtimeError(msg: string): InterpretResult = {
    if (settings.enableTracing) {
      panic("Runtime error: " + msg)
    } else {
      println("Runtime error: " + msg)
    }
    InterpretResult.RuntimeError
  }

  // Helper function to convert values to int for comparison
  def toInt(value: Value): int = {
    value match {
      case Value.Int(i)      => i
      case Value.Bool(true)  => 1
      case Value.Bool(false) => 0
      case _ => panic("Cannot convert " + value + " to int for comparison")
    }
  }

  def binaryIntBoolOp(
      op: int,
      opName: string
  ): InterpretResult = {
    val b = pop()
    val a = pop()

    // Handle string comparisons
    Tuple2(a, b) match {
      case Tuple2(Value.String(aStr), Value.String(bStr)) =>
        val result = op match {
          case Opcode.Ceq => Option.Some(aStr == bStr)
          case Opcode.Cgt =>
            Option.Some(aStr > bStr) // lexicographic comparison
          case Opcode.Clt =>
            Option.Some(aStr < bStr) // lexicographic comparison
          case _ => Option.None
        }
        pushBoolOrInvalidOp(result, opName)

      // Handle numeric and boolean comparisons (original logic)
      case _ =>
        // Convert to int for comparison (HACK: this shouldnt be converting bools to ints but its easier for now)
        val bInt = toInt(b)
        val aInt = toInt(a)

        val result = op match {
          case Opcode.Ceq => Option.Some(aInt == bInt)
          case Opcode.Cgt => Option.Some(aInt > bInt)
          case Opcode.Clt => Option.Some(aInt < bInt)
          case _          => Option.None
        }

        pushBoolOrInvalidOp(result, opName)
    }
  }

  def binaryIntOp(op: int, opName: string): InterpretResult = {
    val b = popInt()
    val a = popInt()
    val result = op match {
      case Opcode.Sub =>
        Option.Some(a - b)
      case Opcode.Mul =>
        Option.Some(a * b)
      case Opcode.Div =>
        Option.Some(a / b)
      case Opcode.Rem =>
        Option.Some(a % b)
      case Opcode.And =>
        Option.Some(a & b)
      case Opcode.Or =>
        Option.Some(a | b)
      case Opcode.Xor =>
        Option.Some(a ^ b)
      case Opcode.Shl =>
        Option.Some(a << b)
      case Opcode.Shr =>
        Option.Some(a >> b)
      case _ =>
        Option.None
    }

    pushIntOrInvalidOp(result, opName)
  }

  def unaryOp(op: int, opName: string): InterpretResult = {
    val a = popInt()

    val result = op match {
      case Opcode.Not => Option.Some(~a)
      case Opcode.Neg => Option.Some(-a)
      case _          => Option.None
    }

    pushIntOrInvalidOp(result, opName)
  }

  def pushIntOrInvalidOp(result: Option[int], opName: string) = {
    result match {
      case Option.None =>
        runtimeError("Invalid operation: " + opName)
      case Option.Some(value) =>
        push(Value.Int(value))
        InterpretResult.Continue
    }
  }

  def pushBoolOrInvalidOp(result: Option[bool], opName: string) = {
    result match {
      case Option.None =>
        runtimeError("Invalid operation: " + opName)
      case Option.Some(value) =>
        pushBool(value)
        InterpretResult.Continue
    }
  }

  def binaryBitwiseOp(op: int, opName: string): InterpretResult = {
    val b = pop()
    val a = pop()

    Tuple2(a, b) match {
      case Tuple2(Value.Int(a), Value.Int(b)) =>
        val result = op match {
          case Opcode.And => Option.Some(a & b)
          case Opcode.Or  => Option.Some(a | b)
          case _          => Option.None
        }
        pushIntOrInvalidOp(result, opName)

      case Tuple2(Value.Bool(a), Value.Bool(b)) =>
        val result = op match {
          case Opcode.And => Option.Some(a && b)
          case Opcode.Or  => Option.Some(a || b)
          case _          => Option.None
        }
        pushBoolOrInvalidOp(result, opName)

      case _ =>
        runtimeError("Expected int or bool on stack")
    }
  }

  def popBool(): bool = {
    val value = pop()
    value match {
      case Value.Bool(b) => b
      case _             => panic("Expected bool on stack, found " + value)
    }
  }

  def popInt(): int = {
    val value = pop()
    value match {
      case Value.Int(i) => i
      case _            => panic("Expected int on stack, found " + value)
    }
  }

  def stackAsInt(pos: int): int = {
    stack(pos) match {
      case Value.Int(value) => value
      case _                => panic("Expected int on stack at position " + pos)
    }
  }

  def checkIsInstance(value: Value, expectedTypeToken: TypeDefToken): bool = {
    // Get the type name from metadata to compare against runtime value
    val expectedTypeName = metadata.getTypeName(expectedTypeToken)

    value match {
      case Value.Int(_) =>
        expectedTypeName == "int"
      case Value.String(_) =>
        expectedTypeName == "string"
      case Value.Bool(_) =>
        expectedTypeName == "bool"
      case Value.Ref(actualTypeToken, _) =>
        // Compare type tokens directly for object references
        actualTypeToken.token == expectedTypeToken.token
      case Value.Uninitialized =>
        false
    }
  }

  def performCast(value: Value, targetTypeToken: TypeDefToken): Value = {
    // Get the target type name from metadata
    val targetTypeName = metadata.getTypeName(targetTypeToken)

    value match {
      case Value.Int(i) =>
        targetTypeName match {
          case "int" => value // identity cast
          case "char" =>
            Value.Int(i) // int to char (keeping as int for simplicity)
          case "string" => Value.String(i.toString())
          case "bool"   => Value.Bool(i != 0)
          case "any"    => value // cast to any preserves the value
          case _ =>
            runtimeError("Cannot cast int to " + targetTypeName)
            Value.Uninitialized
        }

      case Value.Bool(b) =>
        targetTypeName match {
          case "bool"   => value // identity cast
          case "int"    => Value.Int(if (b) 1 else 0)
          case "string" => Value.String(b.toString())
          case "any"    => value // cast to any preserves the value
          case _ =>
            runtimeError("Cannot cast bool to " + targetTypeName)
            Value.Uninitialized
        }

      case Value.String(s) =>
        targetTypeName match {
          case "string" => value // identity cast
          case "any"    => value // cast to any preserves the value
          case _ =>
            runtimeError("Cannot cast string to " + targetTypeName)
            Value.Uninitialized
        }

      case Value.Ref(actualTypeToken, addr) =>
        targetTypeName match {
          case "any" => value // cast to any preserves the value
          case _ =>
            if (actualTypeToken.token == targetTypeToken.token) {
              value // identity cast for same type
            } else {
              runtimeError(
                "Cannot cast " + metadata.getTypeName(
                  actualTypeToken
                ) + " to " + targetTypeName
              )
              Value.Uninitialized
            }
        }

      case Value.Uninitialized =>
        targetTypeName match {
          case "unit" => value
          case "any"  => value
          case _ =>
            runtimeError("Cannot cast uninitialized value to " + targetTypeName)
            Value.Uninitialized
        }
    }
  }

  def run(): InterpretResult = {
    setupHeap()

    var result = entry match {
      case Option.None => InterpretResult.Continue
      case Option.Some(value) =>
        if (settings.enableTracing) {
          val name = metadata.getMethodName(value)
          println("Running program with entry point: " + name)
        }
        methodCall(value, -1)
    }

    while (result == InterpretResult.Continue) {
      result = step()
    }
    result
  }

  def step(): InterpretResult = {
    if (settings.enableTracing) {
      disassembler.extra = Pad.left(Hex.toString(sp), 4, '0') + " "
      disassembler.disassembleInstruction(ip)
    }
    val instruction = readI4()
    instruction match {
      case Opcode.Nop =>
        InterpretResult.Continue

      // control instructions
      case Opcode.Ret =>
        methodReturn()

      case Opcode.Call =>
        val token = readMethodToken()
        // return after the current instruction
        methodCall(token, ip)

      case Opcode.Br =>
        val target = readI4()

        ip = target
        InterpretResult.Continue

      case Opcode.Brfalse =>
        val target = readI4()

        if (!popBool()) {
          ip = target
        }
        InterpretResult.Continue

      case Opcode.Brtrue =>
        val target = readI4()

        if (popBool()) {
          ip = target
        }
        InterpretResult.Continue

      // load constant
      case Opcode.LdcI4 =>
        val value = readI4()
        push(Value.Int(value))

      case Opcode.Ldstr =>
        val value = readStringToken()
        val str = metadata.getString(value)
        push(Value.String(str))

      case Opcode.Ldtrue =>
        pushBool(true)
        InterpretResult.Continue

      case Opcode.Ldfalse =>
        pushBool(false)
        InterpretResult.Continue

      // load args
      case Opcode.Ldarg0 =>
        push(stack(argsp))
      case Opcode.Ldarg1 =>
        push(stack(argsp + 1))
      case Opcode.Ldarg2 =>
        push(stack(argsp + 2))
      case Opcode.Ldarg3 =>
        push(stack(argsp + 3))

      // load locals
      case Opcode.Ldloc0 =>
        push(stack(localp))
      case Opcode.Ldloc1 =>
        push(stack(localp + 1))
      case Opcode.Ldloc2 =>
        push(stack(localp + 2))
      case Opcode.Ldloc3 =>
        push(stack(localp + 3))

      // store locals
      case Opcode.Stloc0 =>
        stack(localp) = pop()
        InterpretResult.Continue
      case Opcode.Stloc1 =>
        stack(localp + 1) = pop()
        InterpretResult.Continue
      case Opcode.Stloc2 =>
        stack(localp + 2) = pop()
        InterpretResult.Continue
      case Opcode.Stloc3 =>
        stack(localp + 3) = pop()
        InterpretResult.Continue

      // binary ops
      case Opcode.Add =>
        binaryAdd()
      case Opcode.Sub =>
        binaryIntOp(Opcode.Sub, "sub")
      case Opcode.Mul =>
        binaryIntOp(Opcode.Mul, "mul")
      case Opcode.Div =>
        binaryIntOp(Opcode.Div, "div")
      case Opcode.Rem =>
        binaryIntOp(Opcode.Rem, "rem")
      case Opcode.And =>
        binaryBitwiseOp(Opcode.And, "and")
      case Opcode.Or =>
        binaryBitwiseOp(Opcode.Or, "and")
      case Opcode.Xor =>
        binaryIntOp(Opcode.Xor, "xor")
      case Opcode.Shl =>
        binaryIntOp(Opcode.Shl, "shl")
      case Opcode.Shr =>
        binaryIntOp(Opcode.Shr, "shr")
      case Opcode.Ceq =>
        binaryIntBoolOp(Opcode.Ceq, "ceq")
      case Opcode.Cgt =>
        binaryIntBoolOp(Opcode.Cgt, "cgt")
      case Opcode.Clt =>
        binaryIntBoolOp(Opcode.Clt, "clt")

      // unary ops
      case Opcode.Not =>
        unaryOp(Opcode.Not, "not")
      case Opcode.Neg =>
        unaryOp(Opcode.Neg, "neg")

      // misc stack ops
      case Opcode.Dup =>
        val a = pop()
        push(a)
        push(a)
      case Opcode.Pop =>
        pop()
        InterpretResult.Continue
      case Opcode.Swap =>
        val a = pop()
        val b = pop()
        push(a)
        push(b)

      // conversion ops
      case Opcode.ConvStr =>
        val a = pop()
        a match {
          case Value.Bool(b) =>
            push(Value.String(b.toString()))
            InterpretResult.Continue
          case Value.Int(i) =>
            // TODO: may need native function here
            push(Value.String(i.toString()))
            InterpretResult.Continue
          case Value.String(s) =>
            push(Value.String(s))
            InterpretResult.Continue
          case Value.Uninitialized =>
            push(Value.String("unit"))
            InterpretResult.Continue
          case ref: Value.Ref =>
            val fullName = metadata.getTypeName(ref.token)
            push(Value.String(fullName))
            InterpretResult.Continue
        }

      case Opcode.ConvBool =>
        val a = pop()
        a match {
          case Value.Int(0) =>
            pushBool(false)
            InterpretResult.Continue
          case Value.Int(1) =>
            pushBool(true)
            InterpretResult.Continue
          case Value.Bool(b) =>
            pushBool(b)
            InterpretResult.Continue
          case _ =>
            runtimeError("Cannot convert value to bool")
        }

      // type checking ops
      case Opcode.IsInst =>
        val typeToken = readTypeDefToken()
        val value = pop()
        val result = checkIsInstance(value, typeToken)
        pushBool(result)

      case Opcode.Cast =>
        val typeToken = readTypeDefToken()
        val value = pop()
        val castedValue = performCast(value, typeToken)
        push(castedValue)

      case Opcode.Stsfld =>
        val token = readFieldToken()
        val field = metadata.fields.get(token)

        // pop the value to store
        val value = pop()

        // store the value in the static field
        heap(staticp + field.index) = value
        InterpretResult.Continue
      case Opcode.Ldsfld =>
        val token = readFieldToken()
        val field = metadata.fields.get(token)

        // load the static field value
        val value = heap(staticp + field.index)
        push(value)
        InterpretResult.Continue

      case Opcode.Ldfld =>
        val token = readFieldToken()
        val field = metadata.fields.get(token)

        // pop the object reference
        val objRef = pop()
        objRef match {
          case Value.Ref(typeToken, addr) =>
            // load the instance field value
            val value = heap(addr + field.index)
            push(value)
            InterpretResult.Continue
          case _ =>
            runtimeError("Expected object reference for field access")
        }

      case Opcode.Stfld =>
        val token = readFieldToken()
        val field = metadata.fields.get(token)

        // pop the value to store
        val value = pop()

        // pop the object reference
        val objRef = pop()
        objRef match {
          case Value.Ref(typeToken, addr) =>
            // store the value in the instance field
            heap(addr + field.index) = value
            InterpretResult.Continue
          case _ =>
            runtimeError("Expected object reference for field assignment")
        }

      case Opcode.Newobj =>
        val token = readMethodToken()

        // get the type definition for this constructor
        val typeDef = metadata.findTypeDefForMethod(token)

        // allocate space for the new object
        val objSize = metadata.getTypeDefSize(typeDef)
        val objAddr = alloc(objSize)

        // initialize all fields to Unit (could be improved to use default values)
        for (i <- 0 to (objSize - 1)) {
          heap(objAddr + i) = Value.Uninitialized
        }

        // push the object reference onto the stack
        push(Value.Ref(typeDef, objAddr))

        // call the constructor
        methodCall(token, ip)

      case _ =>
        val opcode = Opcode.nameOf(instruction)
        panic("Unsupported opcode " + opcode)
        InterpretResult.CompileError
    }
  }

  def methodReturn(): InterpretResult = {
    // localp points to the end of the previous call frame
    // we can index into our old stack frame using localp in the current frame
    // see methodCall for the stack frame structure
    val endFrame = localp

    val retAddr = stackAsInt(endFrame - 3)
    stack(argsp) = pop()
    sp = argsp + 1
    argsp = stackAsInt(endFrame - 1)
    localp = stackAsInt(endFrame - 2)
    ip = retAddr

    if (ip == -1) {
      // special case for tests.
      // if ip is -1, then we are at the top level and have just completed main
      // in this case argsp is also zero and the return address/value
      InterpretResult.OkValue(stack(0))
    } else {
      InterpretResult.Continue
    }
  }

  def methodCall(
      method: MethodToken,
      returnAddress: int
  ): InterpretResult = {
    val numArgs = metadata.getMethodParameterCount(method)
    val hasThis = if (metadata.getMethodHasThis(method)) 1 else 0
    val addr = metadata.getMethodAddress(method)
    val localCount = metadata.getMethodLocals(method)

    push(Value.Int(returnAddress))

    // save this frames segments
    push(Value.Int(localp))
    push(Value.Int(argsp))

    // set up the new frame
    argsp = sp - numArgs - hasThis - 3
    localp = sp

    // push the locals onto the stack
    for (i <- 0 to (localCount - 1)) {
      push(Value.Uninitialized)
    }

    // argsp  = arg0            ━━━┓
    //          arg1               ┃━━━> count
    //          arg2               ┃
    //          argN            ━━━┛
    //          return address
    //          saved localp
    //          saved argsp
    // localp = local0          ━━━┓
    //          local1             ┃━━━> localCount
    //          localN          ━━━┛
    // sp     = stack top

    ip = addr
    InterpretResult.Continue
  }
}
