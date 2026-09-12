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
      heapSize = 268435456,
      enableTracing = trace
    )

    println(
      s"Stage 3: running ${stage2.getFileName} over ${sources.length} sources"
    )
    ImageRunner.execWithArgs(stage2.toString, settings, compilerArgs) match {
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

  private def findRepoRoot(start: Path): Path = {
    var current = start
    while (current != null && !Files.isDirectory(current.resolve("pnc/src"))) {
      current = current.getParent
    }
    require(current != null, s"Panther sources not found above: $start")
    current
  }
}
