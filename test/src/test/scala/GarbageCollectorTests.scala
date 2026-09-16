import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import panther.*

/** Direct VM-level tests keep collection pressure independent of source-program
  * size and make the root contract explicit.
  */
class GarbageCollectorTests extends AnyFunSpec with Matchers {

  def execWithHeap(program: string, heapSize: int): InterpretResult = {
    val base = CompilerSettingsFactory.default
    val settings = base.copy(stackSize = 2048, heapSize = heapSize)
    val tree = MakeSyntaxTree.parseContent(program, settings)
    val compilation = MakeCompilation.create(ListModule.one(tree), settings)
    compilation.diagnostics.count() shouldBe 0
    compilation.exec()
  }

  def vmWithHeap(size: int): VM = {
    val base = CompilerSettingsFactory.default
    val settings = CompilerSettings(
      base.kindRecoveryAttempts,
      base.diagnosticsToPrint,
      32,
      size,
      base.debug,
      base.enableTracing,
      base.printSymbols,
      base.printBoundAssembly,
      base.printLoweredAssembly,
      base.transpile,
      base.run
    )
    val vm = VM(
      new Chunk(),
      new Metadata(),
      Option.None,
      new Array[Value](settings.stackSize),
      new Array[Value](settings.heapSize),
      settings
    )
    vm.setupHeap()
    vm
  }

  describe("garbage collector") {
    it("works independently of bytecode execution") {
      val heap = new ManagedHeap(new Array[Value](2), 4)
      heap.setupStaticFields(0)
      heap.allocateObject(1, new Array[Value](0), 0) shouldBe Option.Some(0)
      heap.allocateObject(1, new Array[Value](0), 0) shouldBe Option.Some(1)
      heap.collect(new Array[Value](0), 0)
      heap.allocateObject(1, new Array[Value](0), 0) shouldBe Option.Some(0)
      heap.allocateObject(1, new Array[Value](0), 0) shouldBe Option.Some(1)
    }

    it("collects once at an allocation slow path before reporting exhaustion") {
      val vm = vmWithHeap(1)
      vm.allocateObject(1) shouldBe Option.Some(0)
      vm.allocateObject(1) shouldBe Option.Some(0)
      vm.heap.collectionCount shouldBe 1
    }

    it("keeps stack roots and reuses unreachable blocks at stable addresses") {
      val vm = vmWithHeap(3)
      val first = vm.allocateObject(1)
      val second = vm.allocateObject(1)
      first shouldBe Option.Some(0)
      second shouldBe Option.Some(1)

      vm.push(Value.Ref(TypeDefToken(0), 0))
      vm.collect()
      vm.heap.isAllocatedStart(0) shouldBe true
      vm.heap.isAllocatedStart(1) shouldBe false

      vm.pop()
      vm.collect()
      vm.allocateObject(1) shouldBe Option.Some(0)
      vm.heap.collectionCount should be >= 2
    }

    it("traces reference fields iteratively and reclaims disconnected cycles") {
      val vm = vmWithHeap(4)
      vm.allocateObject(1) shouldBe Option.Some(0)
      vm.allocateObject(1) shouldBe Option.Some(1)
      vm.heap(0) = Value.Ref(TypeDefToken(0), 1)
      vm.heap(1) = Value.Ref(TypeDefToken(0), 0)
      vm.push(Value.Ref(TypeDefToken(0), 0))
      vm.collect()
      vm.heap.isAllocatedStart(0) shouldBe true
      vm.heap.isAllocatedStart(1) shouldBe true

      vm.pop()
      vm.collect()
      vm.heap.isAllocatedStart(0) shouldBe false
      vm.heap.isAllocatedStart(1) shouldBe false
    }

    it("gives zero-field objects distinct addresses and tracks empty arrays") {
      val vm = vmWithHeap(3)
      vm.allocateObject(0) shouldBe Option.Some(0)
      vm.allocateObject(0) shouldBe Option.Some(1)
      vm.allocateArray(0) shouldBe Option.Some(2)
      vm.collect()
      vm.allocateObject(0) shouldBe Option.Some(0)
    }

    it("preserves a large free block while splitting and reusing a small one") {
      val vm = vmWithHeap(10)
      vm.allocateObject(1) shouldBe Option.Some(0)
      vm.allocateObject(4) shouldBe Option.Some(1) // large dead hole
      vm.allocateObject(1) shouldBe Option.Some(5)
      vm.allocateObject(2) shouldBe Option.Some(6) // small dead hole
      vm.allocateObject(2) shouldBe Option.Some(8)

      vm.push(Value.Ref(TypeDefToken(0), 0))
      vm.push(Value.Ref(TypeDefToken(0), 5))
      vm.push(Value.Ref(TypeDefToken(0), 8))
      vm.collect()

      vm.allocateObject(1) shouldBe Option.Some(6)
      vm.allocateObject(1) shouldBe Option.Some(7) // split remainder
      vm.allocateObject(4) shouldBe Option.Some(1) // large hole survived
    }

    it(
      "rebuilds free lists and coalesces adjacent dead blocks on later collections"
    ) {
      val vm = vmWithHeap(6)
      vm.allocateObject(1) shouldBe Option.Some(0)
      vm.allocateObject(1) shouldBe Option.Some(1)
      vm.allocateObject(1) shouldBe Option.Some(2)
      vm.allocateObject(1) shouldBe Option.Some(3)
      vm.allocateObject(1) shouldBe Option.Some(4)
      vm.allocateObject(1) shouldBe Option.Some(5)

      vm.push(Value.Ref(TypeDefToken(0), 0))
      vm.push(Value.Ref(TypeDefToken(0), 5))
      vm.collect()
      vm.allocateObject(2) shouldBe Option.Some(1)

      vm.push(Value.Ref(TypeDefToken(0), 1))
      vm.collect()
      vm.pop()
      vm.collect()
      vm.allocateObject(4) shouldBe Option.Some(1)
    }

    it("keeps references stored in object fields across repeated collections") {
      val program =
        "class Box(value: int)\n" +
          "class Holder() {\n" +
          "  val kept = new Box(42)\n" +
          "  def churn(): int = {\n" +
          "    var i = 0\n" +
          "    var temporary = new Box(0)\n" +
          "    while (i < 5000) {\n" +
          "      temporary = new Box(i)\n" +
          "      i = i + 1\n" +
          "    }\n" +
          "    kept.value\n" +
          "  }\n" +
          "}\n" +
          "new Holder().churn()"

      execWithHeap(program, 256) shouldBe
        InterpretResult.OkValue(Value.Int(42))
    }
  }
}
