import utest._

object VmTests extends TestSuite {
  val tests = Tests {

    def binary(opcode: Int, a: Value, b: Value, result: Value): Unit = {
      val stack = new Array[Value](10)
      val heap = new Array[Value](0)
      val chunk = Chunk()
      val vm = VM(chunk, Metadata(), stack, heap)

      // setup stack
      vm.push(a)
      vm.push(b)

      chunk.emitOpcode(opcode, 0)
      chunk.emitOpcode(Opcode.Ret, 0)

      assert(vm.run() == InterpretResult.Ok)
      assert(stack(0) == result)
    }

    def binaryIntInt(opcode: Int, a: Int, b: Int, c: Int): Unit =
      binary(opcode, Value.Int(a), Value.Int(b), Value.Int(c))

    def binaryBoolBool(opcode: Int, a: Boolean, b: Boolean, c: Boolean): Unit =
      binary(
        opcode,
        Value.Int(if (a) 1 else 0),
        Value.Int(if (b) 1 else 0),
        Value.Int(if (c) 1 else 0)
      )

    def binaryIntBool(opcode: Int, a: Int, b: Int, c: Boolean): Unit =
      binary(opcode, Value.Int(a), Value.Int(b), Value.Int(if (c) 1 else 0))

    def unary(opcode: Int, a: Int, b: Int): Unit = {
      val stack = new Array[Value](10)
      val heap = new Array[Value](0)
      val chunk = Chunk()
      val vm = VM(chunk, Metadata(), stack, heap)

      // setup stack
      vm.push(Value.Int(a))

      chunk.emitOpcode(opcode, 0)
      chunk.emitOpcode(Opcode.Ret, 0)

      assert(vm.run() == InterpretResult.Ok)
      assert(stack(0) == Value.Int(b))
    }

    def ldci4(a: Int): Unit = {
      val stack = new Array[Value](10)
      val heap = new Array[Value](0)
      val chunk = Chunk()
      val vm = VM(chunk, Metadata(), stack, heap)

      chunk.emitOpcode(Opcode.LdcI4, 0)
      chunk.emitI4(a, 0)
      chunk.emitOpcode(Opcode.Ret, 0)

      assert(vm.run() == InterpretResult.Ok)
      assert(stack(0) == Value.Int(a))
    }

    def ldstr(a: String): Unit = {
      val stack = new Array[Value](10)
      val heap = new Array[Value](0)
      val chunk = Chunk()
      val metadata = Metadata()
      val stringToken = metadata.addString(a)
      val vm = VM(chunk, metadata, stack, heap)

      chunk.emitOpcode(Opcode.Ldstr, 0)
      chunk.emitI4(stringToken.token, 0)
      chunk.emitOpcode(Opcode.Ret, 0)

      assert(vm.run() == InterpretResult.Ok)
      assert(stack(0) == Value.String(a))
    }

    def concatStrings(a: String, b: String): Unit = {
      val stack = new Array[Value](10)
      val heap = new Array[Value](0)
      val chunk = Chunk()
      val metadata = Metadata()
      val stringToken = metadata.addString(a)
      val stringToken2 = metadata.addString(b)
      val vm = VM(chunk, metadata, stack, heap)

      chunk.emitOpcode(Opcode.Ldstr, 0)
      chunk.emitI4(stringToken.token, 0)
      chunk.emitOpcode(Opcode.Ldstr, 0)
      chunk.emitI4(stringToken2.token, 0)
      chunk.emitOpcode(Opcode.Add, 0)
      chunk.emitOpcode(Opcode.Ret, 0)

      assert(vm.run() == InterpretResult.Ok)
      assert(stack(0) == Value.String(a + b))
    }

    test("\"hello\" + \" world\"") - concatStrings("hello", " world")
    test("\"\" + \"\"") - concatStrings("", "")
    test("\"hello\" + \"\"") - concatStrings("hello", "")
    test("\"\" + \"world\"") - concatStrings("", "world")

    test("ldstr \"hello\"") - ldstr("hello")
    test("ldstr \"world\"") - ldstr("world")
    test("ldstr \"\"") - ldstr("")

    // TODO: dup, swap, pop
    // TODO: local variables
    // TODO: arguments
    // TODO: call, ret
    // TODO: branch
    // TODO: conv
  }
}
