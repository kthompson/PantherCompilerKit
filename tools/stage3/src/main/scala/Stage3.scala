import panther.*

import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

/** Proves the self-hosting fixed point: the compiler emitted by `pncs` must
  * emit itself byte-for-byte when run by the VM.
  */
object Stage3 {
  def main(args: Array[String]): Unit = {
    val trace = args.contains("--trace")
    val rootArg = args.find(!_.startsWith("--"))
    val start = rootArg.fold(Paths.get(""))((value: String) => Paths.get(value))
    val root = findRepoRoot(start.toAbsolutePath.normalize())

    val sourceDir = root.resolve("pnc/src")
    val stage2 = root.resolve("pnc/target/pnc.pnb")
    val stage3 = root.resolve("pnc/target/pnc-stage3.pnb")

    require(
      Files.isDirectory(sourceDir),
      s"Panther sources not found: $sourceDir"
    )
    require(
      Files.isRegularFile(stage2),
      s"Stage 2 image not found: $stage2; run sbt pnc/compile first"
    )

    val stream = Files.walk(sourceDir)
    val sources =
      try
        stream
          .iterator()
          .asScala
          .filter(path =>
            Files.isRegularFile(path) && path.toString.endsWith(".pn")
          )
          .toSeq
          .sortBy(_.toString)
      finally stream.close()

    val compilerArgs =
      (stage3.toString +: sources.map(_.toAbsolutePath.toString)).toArray

    // The compiler builds all syntax, symbols, metadata, and bytecode inside
    // one VM run. These are host capacities for that run, not language-level
    // CompilerSettings passed to the compiler being executed.
    val defaults = CompilerSettingsFactory.default
    val settings = defaults.copy(
      stackSize = 2097152,
      // The old 256M-slot capacity predated the collector and consumed several
      // gigabytes across the heap's layout arrays before it could reach an
      // allocation safepoint. Image output now streams from its existing int
      // buffer, so it no longer needs a large contiguous managed byte array.
      heapSize = 33554432,
      enableTracing = trace
    )

    println(
      s"Stage 3: running ${stage2.getFileName} over ${sources.length} sources"
    )
    val image = PnbFile.read(stage2.toString)
    val vm = VM(
      image.chunk,
      image.metadata,
      image.entry,
      new Array[Value](settings.stackSize),
      new Array[Value](settings.heapSize),
      settings
    )
    val result = vm.runWithArgs(compilerArgs)
    println(
      s"Stage 3 heap: ${vm.heap.collectionCount} collections, " +
        s"${vm.heap.allocatedSlots} allocated slots, " +
        s"${vm.heap.reclaimedSlots} reclaimed slots, " +
        s"${vm.heap.peakLiveSlots} peak live slots, " +
        s"${vm.heap.heapp} committed slots"
    )
    val failedReservation =
      if (vm.heap.lastFailedBlockSize > 0)
        s"; last failed ${blockKindName(vm.heap, vm.heap.lastFailedBlockKind)} " +
          s"reservation=${vm.heap.lastFailedBlockSize} slots; " +
          s"site=${allocationSite(vm)}"
      else ""
    println(s"Stage 3 heap layout: ${heapLayout(vm.heap)}$failedReservation")
    result match {
      case InterpretResult.Ok | InterpretResult.OkValue(_) => ()
      case InterpretResult.Exit(0)                         => ()
      case result => sys.error(s"Stage 3 compiler failed: $result")
    }

    val mismatch = Files.mismatch(stage2, stage3)
    if (mismatch != -1) {
      sys.error(
        s"Self-hosting fixed point failed: images first differ at byte $mismatch\n" +
          s"  Stage 2: ${digest(stage2)}  $stage2\n" +
          s"  Stage 3: ${digest(stage3)}  $stage3"
      )
    }

    println(
      s"Stage 3 fixed point verified: ${Files.size(stage3)} bytes, ${digest(stage3)}"
    )
  }

  private def digest(path: Path): String = {
    val hash =
      MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(path))
    hash.map(byte => f"${byte & 0xff}%02x").mkString
  }

  /** Diagnostic-only summary of the non-moving heap after Stage 3 exits. */
  private def heapLayout(heap: ManagedHeap): String = {
    var address = heap.staticCount
    var reusableFreeSlots = 0
    var largestReusableFreeSpan = 0
    var allocatedBlocks = 0
    var largestAllocatedBlock = 0

    while (address < heap.heapp) {
      val size = heap.blockSize(address)
      if (heap.blockKind(address) == heap.FreeBlock) {
        reusableFreeSlots += size
        if (size > largestReusableFreeSpan) largestReusableFreeSpan = size
      } else {
        allocatedBlocks += 1
        if (size > largestAllocatedBlock) largestAllocatedBlock = size
      }
      address += size
    }

    val tailFreeSlots = heap.values.length - heap.heapp
    s"$reusableFreeSlots reusable free slots; " +
      s"$largestReusableFreeSpan largest reusable span; " +
      s"$tailFreeSlots tail slots; $allocatedBlocks allocated blocks; " +
      s"$largestAllocatedBlock largest allocated block"
  }

  private def blockKindName(heap: ManagedHeap, kind: int): String =
    if (kind == heap.ArrayBlock) "array"
    else if (kind == heap.ObjectBlock) "object"
    else "unknown"

  /** The failing `Newarr` has consumed its opcode and element token. */
  private def allocationSite(vm: VM): String = {
    val offset = vm.ip - 2
    var method = -1
    var methodAddress = -1
    for (index <- 0 until vm.metadata.methods.size) {
      val address = vm.metadata.methods.methods(index).address
      if (address <= offset && address > methodAddress) {
        method = index
        methodAddress = address
      }
    }
    if (method < 0 || offset < 0 || offset >= vm.chunk.size)
      s"unknown bytecode offset $offset"
    else {
      val name = vm.metadata.getMethodName(MethodToken(method))
      s"$name at source line ${vm.chunk.readLine(offset)} (bytecode offset $offset)"
    }
  }

  private def findRepoRoot(start: Path): Path = {
    var current = start
    while (current != null && !Files.isDirectory(current.resolve("pnc/src"))) {
      current = current.getParent
    }
    require(current != null, s"Panther sources not found above: $start")
    current
  }
}
