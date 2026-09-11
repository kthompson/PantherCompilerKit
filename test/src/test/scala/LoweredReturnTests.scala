import TestHelpers.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import panther.*

class LoweredReturnTests extends AnyFunSpec with Matchers {

  describe("lowered returns") {
    it("should print the returned expression") {
      val comp = mkCompilation("")
      val sb = IndentedStringBuilder(false)
      val printer = new LoweredAssemblyPrinter(comp.binder, sb, false)
      val location = TextLocationFactory.empty()

      printer.printStatement(
        LoweredStatement.Return(
          location,
          LoweredExpression.Integer(location, 7)
        )
      )

      sb.toString() shouldBe "return 7\n"
    }

    it("should emit the returned expression followed by Ret") {
      val comp = mkCompilation("")
      val emitter = new Emitter(List.Nil, comp.root, comp.binder, comp.assembly)
      val context = EmitContext(
        emitter.chunk,
        MethodMetadata(
          StringToken(0),
          MetadataFlags.None,
          0,
          0,
          0,
          0,
          false
        ),
        DictionaryModule.empty[Symbol, int](),
        DictionaryModule.empty[Symbol, int]()
      )
      val location = TextLocationFactory.empty()

      emitter.emitReturnStatement(
        LoweredStatement.Return(
          location,
          LoweredExpression.Integer(location, 7)
        ),
        context
      )

      emitter.chunk.size shouldBe 3
      emitter.chunk.readI4(0) shouldBe Opcode.LdcI4
      emitter.chunk.readI4(1) shouldBe 7
      emitter.chunk.readI4(2) shouldBe Opcode.Ret
    }
  }
}
