import panther._

enum InterpretResult {
  case Continue // will continue running
  case Ok // completed running the program
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

case class VM(chunk: Chunk, metadata: Metadata, stack: Array[Value], heap: Array[Value]) {
  var sp = 0 // stack pointer
  var ip = 0 // instruction pointer
  var argsp = 0 // argument stack pointer
  var localp = 0 // local variable pointer

  def readI4(): int = {
    val value = chunk.readI4(ip)
    ip += 1
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

  def push(value: Value): InterpretResult = {
    stack(sp) = value
    sp += 1
    InterpretResult.Continue
  }

  def pop(): Value = {
    sp -= 1
    stack(sp)
  }

  def trace(msg: String): unit = {
    println("ip=" + ip + " sp=" + sp + " " + msg)
  }

  def runtimeError(msg: String): InterpretResult = {
    println(msg)
    InterpretResult.RuntimeError
  }

  def binaryIntBoolOp(op: (int, int) => Boolean, opName: string): InterpretResult = {
    val b = popInt()
    val a = popInt()
    trace(opName + " " + a + ", " + b)
    push(Value.Bool(op(a, b)))
    InterpretResult.Continue
  }

  def binaryBoolOrIntOp(intOp: (int, int) => int, boolOp: (bool, bool) => bool, opName: string): InterpretResult = {
    val b = pop()
    val a = pop()
    trace(opName + " " + a + ", " + b)
    a match {
      case Value.Int(a) =>
        b match {
          case Value.Int(b) => push(Value.Int(intOp(a, b)))
          case _ => runtimeError("Expected int on stack")
        }
      case Value.Bool(a) =>
        b match {
          case Value.Bool(b) => push(Value.Bool(boolOp(a, b)))
          case _ => runtimeError("Expected bool on stack")
        }
      case _ => runtimeError("Expected int or bool on stack")
    }
  }

  def binaryStrOrIntOp(intOp: (int, int) => int, strOp: (string, string) => string, opName: string): InterpretResult = {
    val b = pop()
    val a = pop()
    trace(opName + " " + a + ", " + b)
    a match {
      case Value.Int(a) =>
        b match {
          case Value.Int(b) => push(Value.Int(intOp(a, b)))
          case _ => runtimeError("Expected int on stack")
        }
      case Value.String(a) =>
        b match {
          case Value.String(b) => push(Value.String(strOp(a, b)))
          case _ => runtimeError("Expected bool on stack")
        }
      case _ => runtimeError("Expected int or bool on stack")
    }
  }

  def binaryIntOp(op: (int, int) => int, opName: string): InterpretResult = {
    val b = popInt()
    val a = popInt()
    trace(opName + " " + a + ", " + b)
    push(Value.Int(op(a, b)))
    InterpretResult.Continue
  }

  def unaryOp(op: int => int, opName: string): InterpretResult = {
    val a = popInt()
    trace(opName + " " + a)
    push(Value.Int(op(a)))
    InterpretResult.Continue
  }

  def popInt(): int = {
    val value = pop()
    value match {
      case Value.Int(i) => i
      case _ => panic("Expected int on stack")
    }
  }

  def stackAsInt(pos: int): int = {
    stack(pos) match {
      case Value.Int(value) => value
      case _ => panic("Expected int on stack at position " + pos)
    }
  }

  def run(): InterpretResult = {
    var result = InterpretResult.Continue
    while (result == InterpretResult.Continue) {
      result = step()
    }
    result
  }

  def step(): InterpretResult = {
    val instruction = readI4()
    instruction match {
      // function ops
      case Opcode.Ret =>
        trace("ret")
        val endFrame = localp
        if (localp == 0) {
          // special case for tests.
          // if localp is 0, then we are at the top level and have never entered a function
          // in this case argsp is also zero and the return address
          stack(argsp) = pop()
          return InterpretResult.Ok
        }
        val retAddr = stackAsInt(endFrame - 3)
        stack(argsp) = pop()
        sp = argsp + 1
        argsp = stackAsInt(endFrame - 1)
        localp = stackAsInt(endFrame - 2)
        ip = retAddr
        InterpretResult.Continue
      case Opcode.Call =>
        val methodTok = readMethodToken()
        val numArgs = metadata.getMethodParameterCount(methodTok)
        val addr = metadata.getMethodAddress(methodTok)
        val localCount = metadata.getMethodLocals(methodTok)
        val returnAddress = ip + 1
        push(Value.Int(returnAddress))

        // save this frames segments
        push(Value.Int(localp))
        push(Value.Int(argsp))

        // set up the new frame
        argsp = sp - numArgs - 3
        localp = sp

        // push the locals onto the stack
        for (i <- 0 to (localCount - 1)) {
          push(Value.Unit)
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

      // load constant
      case Opcode.LdcI4 =>
        val value = readI4()
        trace("ldc.i4 " + value)
        push(Value.Int(value))

      case Opcode.Ldstr =>
        val value = readStringToken()
        val str = metadata.getString(value)
        push(Value.String(str))

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
        binaryStrOrIntOp((a, b) => a + b, (a, b) => a + b, "add")
      case Opcode.Sub =>
        binaryIntOp((a, b) => a - b, "sub")
      case Opcode.Mul =>
        binaryIntOp((a, b) => a * b, "mul")
      case Opcode.Div =>
        binaryIntOp((a, b) => a / b, "div")
      case Opcode.Rem =>
        binaryIntOp((a, b) => a % b, "rem")
      case Opcode.And =>
        binaryBoolOrIntOp((a, b) => a & b, (a, b) => a & b, "and")
      case Opcode.Or =>
        binaryBoolOrIntOp((a, b) => a | b, (a, b) => a | b, "or")
      case Opcode.Xor =>
        binaryIntOp((a, b) => a ^ b, "xor")
      case Opcode.Shl =>
        binaryIntOp((a, b) => a << b, "shl")
      case Opcode.Shr =>
        binaryIntOp((a, b) => a >> b, "shr")
      case Opcode.Ceq =>
        binaryIntBoolOp((a, b) => a == b, "ceq")
      case Opcode.Cne =>
        binaryIntBoolOp((a, b) => a != b, "cne")
      case Opcode.Cge =>
        binaryIntBoolOp((a, b) => a >= b, "cge")
      case Opcode.Cgt =>
        binaryIntBoolOp((a, b) => a > b, "cgt")
      case Opcode.Cle =>
        binaryIntBoolOp((a, b) => a <= b, "cle")
      case Opcode.Clt =>
        binaryIntBoolOp((a, b) => a < b, "clt")

      // unary ops
      case Opcode.Not =>
        unaryOp(a => ~a, "not")
      case Opcode.Neg =>
        unaryOp(a => -a, "neg")

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

      case Opcode.Nop =>
        InterpretResult.Continue

      // conversion ops
      case Opcode.ConvI4 =>
        val a = pop()
        a match {
          case Value.Bool(b) =>
            trace("conv.i4 " + b)
            push(Value.Int(if (b) 1 else 0))
            InterpretResult.Continue
          case Value.Int(i) =>
            trace("conv.i4 " + i)
            push(Value.Int(i))
            InterpretResult.Continue
          case _ => runtimeError("conv.i4 requires a bool or int")
        }

      case Opcode.ConvStr =>
        val a = pop()
        a match {
          case Value.Int(i) =>
            trace("conv.str " + i)
            // TODO: may need native function here
            push(Value.String(i.toString))
            InterpretResult.Continue
          case Value.Bool(b) =>
            trace("conv.str " + b)
            push(Value.String(if (b) "true" else "false"))
            InterpretResult.Continue
          case Value.String(s) =>
            trace("conv.str " + s)
            push(Value.String(s))
            InterpretResult.Continue
          case Value.Unit =>
            trace("conv.str unit")
            push(Value.String("unit"))
            InterpretResult.Continue
        }
      case _ =>
        trace("Unknown opcode " + instruction)
        InterpretResult.CompileError
    }
  }
}