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
      binary(opcode, Value.Bool(a), Value.Bool(b), Value.Bool(c))

    def binaryIntBool(opcode: Int, a: Int, b: Int, c: Boolean): Unit =
      binary(opcode, Value.Int(a), Value.Int(b), Value.Bool(c))

    def unary(opcode: Int, a: Int, b: Int): Unit ={
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


    def ldci4(a: Int): Unit ={
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

    test("1 + 2 = 3") - binaryIntInt(Opcode.Add, 1, 2, 3)
    test("22 + 24 = 46") - binaryIntInt(Opcode.Add, 22, 24, 46)
    test("3 - 2 = 1") - binaryIntInt(Opcode.Sub, 3, 2, 1)
    test("2 - 3 = -1") - binaryIntInt(Opcode.Sub, 2, 3, -1)
    test("2 * 3 = 6") - binaryIntInt(Opcode.Mul, 2, 3, 6)
    test("3 / 2 = 1") - binaryIntInt(Opcode.Div, 3, 2, 1)
    test("3 % 2 = 1") - binaryIntInt(Opcode.Rem, 3, 2, 1)

    test("1 < 2") - binaryIntBool(Opcode.Clt, 1, 2, true)
    test("2 < 1") - binaryIntBool(Opcode.Clt, 2, 1, false)
    test("1 <= 2") - binaryIntBool(Opcode.Cle, 1, 2, true)
    test("2 <= 1") - binaryIntBool(Opcode.Cle, 2, 1, false)
    test("1 > 2") - binaryIntBool(Opcode.Cgt, 1, 2, false)
    test("2 > 1") - binaryIntBool(Opcode.Cgt, 2, 1, true)
    test("1 >= 2") - binaryIntBool(Opcode.Cge, 1, 2, false)
    test("2 >= 1") - binaryIntBool(Opcode.Cge, 2, 1, true)

    test("1 == 1") - binaryIntBool(Opcode.Ceq, 1, 1, true)
    test("1 == 2") - binaryIntBool(Opcode.Ceq, 1, 2, false)
    test("1 != 2") - binaryIntBool(Opcode.Cne, 1, 2, true)
    test("1 != 1") - binaryIntBool(Opcode.Cne, 1, 1, false)

    test("-(-1)") - unary(Opcode.Neg, -1, 1)
    test("-1") - unary(Opcode.Neg, 1, -1)
    test("~1") - unary(Opcode.Not, 1, -2)
    test("~0") - unary(Opcode.Not, 0, -1)

    test("1 & 2") - binaryIntInt(Opcode.And, 1, 2, 0)
    test("1 & 1") - binaryIntInt(Opcode.And, 1, 1, 1)
    test("1 & 0") - binaryIntInt(Opcode.And, 1, 0, 0)
    test("23 & 42") - binaryIntInt(Opcode.And, 23, 42, 2)
    test("true & true") - binaryBoolBool(Opcode.And, true, true, true)
    test("true & false") - binaryBoolBool(Opcode.And, true, false, false)
    test("false & true") - binaryBoolBool(Opcode.And, false, true, false)

    test("1 | 0") - binaryIntInt(Opcode.Or, 1, 0, 1)
    test("1 | 1") - binaryIntInt(Opcode.Or, 1, 1, 1)
    test("1 | 2") - binaryIntInt(Opcode.Or, 1, 2, 3)
    test("true | true") - binaryBoolBool(Opcode.Or, true, true, true)
    test("true | false") - binaryBoolBool(Opcode.Or, true, false, true)
    test("false | true") - binaryBoolBool(Opcode.Or, false, true, true)
    test("false | false") - binaryBoolBool(Opcode.Or, false, false, false)

    test("1 ^ 2") - binaryIntInt(Opcode.Xor, 1, 2, 3)
    test("1 ^ 1") - binaryIntInt(Opcode.Xor, 1, 1, 0)
    test("1 ^ 0") - binaryIntInt(Opcode.Xor, 1, 0, 1)
    test("1 << 2") - binaryIntInt(Opcode.Shl, 1, 2, 4)

    test("4 >> 2") - binaryIntInt(Opcode.Shr, 4, 2, 1)
    test("-4 >> 2") - binaryIntInt(Opcode.Shr, -4, 2, -1)

    test("ldc 42") - ldci4(42)
    test("ldc -42") - ldci4(-42)
    test("ldc 0") - ldci4(0)
    test("ldc 1") - ldci4(1)
    test("ldc -1") - ldci4(-1)
    test("ldc 100") - ldci4(100)

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