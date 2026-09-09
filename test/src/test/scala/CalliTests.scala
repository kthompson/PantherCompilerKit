import panther.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

/** `Calli` is exercised directly against a hand-built chunk. The emitter does
  * not produce one yet, and this is the instruction every call through evidence
  * will go through, so it is worth pinning independently of the front end.
  */
class CalliTests extends AnyFunSpec with Matchers {

  def runChunk(build: (Chunk, Metadata) => MethodToken): Value = {
    val metadata = new Metadata()
    val chunk = new Chunk()
    val entry = build(chunk, metadata)
    val settings = CompilerSettingsFactory.default
    val vm = VM(
      chunk,
      metadata,
      Option.Some(entry),
      new Array[Value](settings.stackSize),
      new Array[Value](settings.heapSize),
      settings
    )
    vm.run() match {
      case InterpretResult.OkValue(value) => value
      case result => throw new AssertionError("expected a value, got " + result)
    }
  }

  describe("Calli") {
    it("should call a method whose token came off the stack") {
      val value = runChunk { (chunk, metadata) =>
        // token 0: double(x: int): int
        val doubler =
          metadata.addMethod("double", MetadataFlags.Static, false, 0, 0, 0)
        metadata.addParam("x", MetadataFlags.None, 0)

        // token 1: main(): int
        val main =
          metadata.addMethod("main", MetadataFlags.Static, false, 0, 0, 0)

        // double: ldarg.0; ldarg.0; add; ret
        metadata.methods.methods(doubler.token).address = chunk.size
        chunk.emitOpcode(Opcode.Ldarg0, 1)
        chunk.emitOpcode(Opcode.Ldarg0, 1)
        chunk.emitOpcode(Opcode.Add, 1)
        chunk.emitOpcode(Opcode.Ret, 1)

        // main: ldc.i4 21; ldc.i4 <token of double>; calli; ret
        metadata.methods.methods(main.token).address = chunk.size
        chunk.emitOpcode(Opcode.LdcI4, 2)
        chunk.emitI4(21, 2)
        chunk.emitOpcode(Opcode.LdcI4, 2)
        chunk.emitI4(doubler.token, 2)
        chunk.emitOpcode(Opcode.Calli, 2)
        chunk.emitOpcode(Opcode.Ret, 2)

        main
      }

      value match {
        case Value.Int(i) => i shouldBe 42
        case other => throw new AssertionError("expected int, got " + other)
      }
    }

    it("should disassemble with no operand") {
      Opcode.nameOf(Opcode.Calli) shouldBe "calli"
    }
  }
}
