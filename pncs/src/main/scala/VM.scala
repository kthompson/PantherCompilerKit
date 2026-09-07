import panther._
import system.io.File
import system.io.Path

enum InterpretResult {
  case Continue // will continue running
  case Ok // completed running the program
  case OkValue(
      value: Value
  ) // completed running the program and returned a value
  /** The program called `exit(code)`.
    *
    * Distinct from `OkValue`, which carries the value the program evaluated
    * to: `exit(3)` and a program whose last expression is `3` mean different
    * things to whatever launched it, and only the first one chose its code.
    */
  case Exit(code: int)
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

  def getDefaultValueForType(typeToken: TypeDefToken): Value = {
    // Get the type definition and return appropriate default value
    val typeDef = metadata.typeDefs.typeDefs(typeToken.token)
    val typeName = metadata.getString(typeDef.name)
    typeName match {
      case "int"    => Value.Int(0)
      case "bool"   => Value.Bool(false)
      case "string" => Value.String("")
      case "char"   => Value.Int(0) // char represented as int
      case _ => Value.Uninitialized // For reference types and unknown types
    }
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

  /** Slides the top `numArgs` values up by one and drops `value` underneath
    * them.
    */
  def insertBelowArguments(value: Value, numArgs: int): unit = {
    var index = sp
    while (index > sp - numArgs) {
      stack(index) = stack(index - 1)
      index = index - 1
    }
    stack(sp - numArgs) = value
    sp = sp + 1
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
      case _ =>
        panic("Cannot convert " + string(value) + " to int for comparison")
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
      // Reference identity, which is what
      // [ADR 0003](../../../docs/architecture/adr/0003-equality-on-reference-types.md)
      // says `==` between two reference types means. Only equality: there is
      // no order on addresses to expose.
      case Tuple2(Value.Ref(aType, aAddr), Value.Ref(bType, bAddr)) =>
        val result =
          if (op == Opcode.Ceq)
            Option.Some(aType == bType && aAddr == bAddr)
          else Option.None

        pushBoolOrInvalidOp(result, opName)

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

  /** How a value reads as text.
    *
    * `ConvStr` and `println` have to agree — `println(x)` and
    * `println(string(x))` print the same thing — so they share this rather than
    * matching on the value twice.
    */
  def valueToString(value: Value): string = {
    value match {
      case Value.Bool(b)       => string(b)
      case Value.Int(i)        => string(i)
      case Value.String(s)     => s
      case Value.Uninitialized => "unit"
      // A reference prints as its type, there being no `Show` to reach from
      // here — the evidence a derived given carries is a front-end notion and
      // the VM only has the token.
      case ref: Value.Ref => metadata.getTypeName(ref.token)
    }
  }

  /** Runs the builtin `Callx` named, which `Builtin` gave an id.
    *
    * The receiver and arguments are on the stack exactly as they would be for
    * an ordinary call, and each of these pops what it declared and pushes its
    * result, so a builtin leaves the stack the way `methodReturn` does — one
    * value. No frame is built: there is no body to return from.
    */
  def nativeCall(id: int): InterpretResult = {
    if (id == Builtin.Println) {
      println(valueToString(pop()))
      push(Value.Uninitialized)
    } else if (id == Builtin.Print) {
      print(valueToString(pop()))
      push(Value.Uninitialized)
    } else if (id == Builtin.Panic) {
      // `panic` and `exit` are declared to return `never`, so neither pushes:
      // nothing reads the result of a call that does not come back.
      runtimeError(valueToString(pop()))
    } else if (id == Builtin.Exit) {
      // Ends the run rather than a frame, so there is no `Ret` and no stack to
      // unwind: `run` stops as soon as this is not `Continue`.
      InterpretResult.Exit(popInt())
    } else if (id == Builtin.Assert) {
      assertOp()
    } else if (id == Builtin.Mod) {
      modOp()
    } else if (id == Builtin.StringApply) {
      stringApplyOp()
    } else if (id == Builtin.CharToString) {
      charToStringOp()
    } else if (id == Builtin.IntApply) {
      intApplyOp()
    } else if (id == Builtin.BoolApply) {
      boolApplyOp()
    } else if (id == Builtin.CharApply) {
      charApplyOp()
    } else if (id == Builtin.Substring) {
      substringOp()
    } else if (id == Builtin.EndsWith) {
      endsWithOp()
    } else if (id == Builtin.StringCompareTo || id == Builtin.IntCompareTo) {
      // Two ids, one implementation: `compareOp` already tells the operand
      // types apart by what it pops.
      compareOp()
    } else if (id == Builtin.ReadAllText) {
      readAllTextOp()
    } else if (id == Builtin.WriteAllText) {
      writeAllTextOp()
    } else if (id == Builtin.PathCombine) {
      pathCombineOp()
    } else if (id == Builtin.PathName) {
      pathNameOp()
    } else {
      runtimeError("No implementation for builtin " + string(id))
    }
  }

  /** `assert(condition, message)`. */
  def assertOp(): InterpretResult = {
    val message = pop()
    val condition = pop()
    condition match {
      case Value.Bool(true)  => push(Value.Uninitialized)
      case Value.Bool(false) => runtimeError(valueToString(message))
      case _                 => runtimeError("Expected bool condition for assert")
    }
  }

  /** `string(value)`. */
  def stringApplyOp(): InterpretResult = {
      push(Value.String(valueToString(pop())))
  }

  /** `string(c)` where `c` is a char — the character itself, not its code.
    *
    * `stringApplyOp` would answer "97" for `'a'`, both being `Value.Int` by the
    * time they reach here. The emitter picks between the two by the static
    * type, which is the only place the difference still exists.
    */
  def charToStringOp(): InterpretResult = {
    val value = pop()
    value match {
      case Value.Int(code) => push(Value.String(string(char(code))))
      case Value.String(s) => push(Value.String(s))
      case _               => runtimeError("Cannot convert value to string")
    }
  }

  /** `bool(value)`. */
  def boolApplyOp(): InterpretResult = {
      val a = pop()
      a match {
        case Value.Int(0) =>
          pushBool(false)
          InterpretResult.Continue
        case Value.Int(_) =>
          pushBool(true)
          InterpretResult.Continue
        case Value.Bool(b) =>
          pushBool(b)
          InterpretResult.Continue
        case _ =>
          runtimeError("Cannot convert value to bool: " + string(a))
      }
  }

  /** `int(value)`. */
  def intApplyOp(): InterpretResult = {
      val a = pop()
      a match {
        case Value.Int(i) =>
          push(Value.Int(i))
          InterpretResult.Continue
        case Value.Bool(true) =>
          push(Value.Int(1))
          InterpretResult.Continue
        case Value.Bool(false) =>
          push(Value.Int(0))
          InterpretResult.Continue
        case Value.String(s) =>
          if (s.length == 0) {
            runtimeError("Cannot convert empty string to int")
          } else if (s(0) == '-') {
            atoi(s, 1, 0) match {
              case Option.None => runtimeError("Cannot convert string to int")
              case Option.Some(value) => push(Value.Int(-value))
            }
          } else {
            atoi(s, 0, 0) match {
              case Option.None => runtimeError("Cannot convert string to int")
              case Option.Some(value) => push(Value.Int(value))
            }
          }

        case _ =>
          runtimeError("Cannot convert value to int")
      }
  }

  /** `char(value)`. */
  def charApplyOp(): InterpretResult = {
      val a = pop()
      a match {
        case Value.Int(i) =>
          push(Value.Int(i))
          InterpretResult.Continue
        case Value.String(s) =>
          if (s.length == 1) {
            push(Value.Int(s(0)))
            InterpretResult.Continue
          } else {
            runtimeError(
              "Cannot convert string of length " + string(s.length) + " to char"
            )
          }
        case _ =>
          runtimeError("Cannot convert value to char")
      }
  }

  /** `compareTo` for every type that has an ordering.
    *
    * Answers the sign rather than the difference, so that the two orderings can
    * agree on an answer: strings have no difference to subtract, and an int one
    * would overflow.
    */
  def compareOp(): InterpretResult = {
    val b = pop()
    val a = pop()

    Tuple2(a, b) match {
      case Tuple2(Value.String(aStr), Value.String(bStr)) =>
        push(Value.Int(if (aStr < bStr) -1 else if (aStr > bStr) 1 else 0))
        InterpretResult.Continue
      case Tuple2(Value.Int(aInt), Value.Int(bInt)) =>
        push(Value.Int(if (aInt < bInt) -1 else if (aInt > bInt) 1 else 0))
        InterpretResult.Continue
      case _ =>
        runtimeError("Cannot compare " + string(a) + " and " + string(b))
    }
  }

  /** `str.substring(start, end)`, with `end` exclusive. */
  def substringOp(): InterpretResult = {
    val end = popInt()
    val start = popInt()
    val value = pop()

    value match {
      case Value.String(str) =>
        if (start < 0 || end > str.length || start > end) {
          runtimeError(
            "Substring " + string(start) + ".." + string(end) +
              " out of range for string of length " + string(str.length)
          )
        } else {
          push(Value.String(str.substring(start, end)))
          InterpretResult.Continue
        }
      case _ =>
        runtimeError("Expected string for substring")
    }
  }

  /** `mod(a, b)`.
    *
    * The same answer as the `%` operator, which is `Rem`: the sign follows the
    * dividend. Separate from `Rem` only because it is reached as a call.
    */
  def modOp(): InterpretResult = {
    val b = popInt()
    val a = popInt()
    if (b == 0) {
      runtimeError("Division by zero in mod")
    } else {
      push(Value.Int(a % b))
    }
  }

  /** `File.readAllText(path)`.
    *
    * Reads through the host, which is the same shim the Scala compiler calls,
    * so a self-hosted `pnc` reading a file and `pncs` reading it agree by
    * construction. This is the bootstrap floor, as `str.substring` calling the
    * host's substring is.
    */
  def readAllTextOp(): InterpretResult = {
    val path = pop()
    path match {
      case Value.String(file) =>
        push(Value.String(File.readAllText(file)))
      case _ =>
        runtimeError("Expected string path for readAllText")
    }
  }

  /** `File.writeAllText(path, text)`. */
  def writeAllTextOp(): InterpretResult = {
    val text = pop()
    val path = pop()
    Tuple2(path, text) match {
      case Tuple2(Value.String(file), Value.String(contents)) =>
        File.writeAllText(file, contents)
        push(Value.Uninitialized)
      case _ =>
        runtimeError("Expected string path and text for writeAllText")
    }
  }

  /** `Path.combine(path1, path2)`. */
  def pathCombineOp(): InterpretResult = {
    val second = pop()
    val first = pop()
    Tuple2(first, second) match {
      case Tuple2(Value.String(path1), Value.String(path2)) =>
        push(Value.String(Path.combine(path1, path2)))
      case _ =>
        runtimeError("Expected two string paths for combine")
    }
  }

  /** `Path.nameWithoutExtension(path)`. */
  def pathNameOp(): InterpretResult = {
    val value = pop()
    value match {
      case Value.String(path) =>
        push(Value.String(Path.nameWithoutExtension(path)))
      case _ =>
        runtimeError("Expected string path for nameWithoutExtension")
    }
  }

  /** `str.endsWith(suffix)`. */
  def endsWithOp(): InterpretResult = {
    val suffix = pop()
    val value = pop()

    Tuple2(value, suffix) match {
      case Tuple2(Value.String(str), Value.String(suffixStr)) =>
        pushBool(str.endsWith(suffixStr))
        InterpretResult.Continue
      case _ =>
        runtimeError("Expected strings for endsWith")
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
      case _ => panic("Expected bool on stack, found " + string(value))
    }
  }

  def popInt(): int = {
    val value = pop()
    value match {
      case Value.Int(i) => i
      case _ => panic("Expected int on stack, found " + string(value))
    }
  }

  def stackAsInt(pos: int): int = {
    stack(pos) match {
      case Value.Int(value) => value
      case _ => panic("Expected int on stack at position " + string(pos))
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
          case "string" => Value.String(string(i))
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
          case "string" => Value.String(string(b))
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

      // The token is on top of the stack, above the arguments. Popping it
      // first leaves the frame looking exactly as `Call` leaves it, so
      // `methodCall` computes argsp the same way.
      case Opcode.Calli =>
        val token = MethodToken(popInt())
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

      // The emitter has always emitted these for the fifth local onward; a
      // method with that many is just newly reachable, from the temporaries
      // lowering makes for a derived member's arguments.
      case Opcode.Ldlocn =>
        push(stack(localp + readI4()))

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
      case Opcode.Stlocn =>
        val index = readI4()
        stack(localp + index) = pop()
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

        // The receiver goes *beneath* the constructor's arguments, so `this`
        // sits at argument slot 0 exactly as it does for every other instance
        // call and `getMethodParameterMap` can number declared parameters from
        // 1 either way. Pushing it on top would put it after the arguments,
        // and the constructor's own return — which writes to argsp — would
        // then overwrite the first argument instead of yielding the object.
        insertBelowArguments(
          Value.Ref(typeDef, objAddr),
          metadata.getMethodParameterCount(token)
        )

        // call the constructor
        methodCall(token, ip)

      // Array operations
      // An array's first slot holds its length and its elements follow, so
      // `Ldelem` and `Stelem` index from `addr + 1`. Without the header there
      // is nowhere for `Ldlen` to read from: an array reference is an address
      // and nothing else, and `length` was being read as a field, which landed
      // on element 0.
      case Opcode.Newarr =>
        // Read the element type token
        val elementTypeToken = readTypeDefToken()

        // Pop the array size from the stack
        val sizeValue = pop()
        sizeValue match {
          case Value.Int(size) =>
            // One slot for the length, then one per element
            val arrayAddr = alloc(size + 1)
            heap(arrayAddr) = Value.Int(size)

            // Initialize all elements with appropriate default values based on type
            val defaultValue = getDefaultValueForType(elementTypeToken)
            for (i <- 0 to (size - 1)) {
              heap(arrayAddr + 1 + i) = defaultValue
            }

            // Use the element type token for the array reference
            // Note: In a complete implementation, we might need a separate array type token
            // but for now we'll use the element type token
            push(Value.Ref(elementTypeToken, arrayAddr))
            InterpretResult.Continue
          case _ =>
            runtimeError("Expected integer size for array creation")
        }

      case Opcode.Ldlen =>
        val arrayRef = pop()
        arrayRef match {
          case Value.Ref(_, addr) =>
            push(heap(addr))
            InterpretResult.Continue
          // A string has a length too, and answers it directly rather than out
          // of a header slot, for the same reason `Ldelem` indexes one.
          case Value.String(str) =>
            push(Value.Int(str.length))
            InterpretResult.Continue
          case _ =>
            runtimeError("Expected array reference for length")
        }

      case Opcode.Callx =>
        nativeCall(readI4())

      case Opcode.Ldelem =>
        // Pop the index and array reference from the stack
        val index = pop()
        val arrayRef = pop()

        index match {
          case Value.Int(indexValue) =>
            arrayRef match {
              case Value.Ref(typeToken, addr) =>
                // Load the element from the array
                val elementValue = heap(addr + 1 + indexValue)
                push(elementValue)
                InterpretResult.Continue
              // A string indexes too, and yields a char, which is an int here
              // for the same reason `ConvChar` produces one.
              case Value.String(str) =>
                if (indexValue < 0 || indexValue >= str.length) {
                  runtimeError(
                    "Index " + string(indexValue) +
                      " out of range for string of length " +
                      string(str.length)
                  )
                } else {
                  push(Value.Int(int(str(indexValue))))
                  InterpretResult.Continue
                }
              case _ =>
                runtimeError("Expected array reference for element access")
            }
          case _ =>
            runtimeError("Expected integer index for array access")
        }

      case Opcode.Stelem =>
        // Pop the value, index, and array reference from the stack
        val value = pop()
        val index = pop()
        val arrayRef = pop()

        index match {
          case Value.Int(indexValue) =>
            arrayRef match {
              case Value.Ref(typeToken, addr) =>
                // Store the value in the array
                heap(addr + 1 + indexValue) = value
                InterpretResult.Continue
              case _ =>
                runtimeError("Expected array reference for element assignment")
            }
          case _ =>
            runtimeError("Expected integer index for array assignment")
        }

      case _ =>
        val opcode = Opcode.nameOf(instruction)
        panic("Unsupported opcode " + opcode)
        InterpretResult.CompileError
    }
  }

  def atoi(s: string, index: int, value: int): Option[int] = {
    if (index >= s.length) {
      Option.Some(value)
    } else {
      val c = s(index)
      if (c < '0' || c > '9') {
        Option.None
      } else {
        val digit = c - '0'
        val newValue = value * 10 + digit
        atoi(s, index + 1, newValue)
      }
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
