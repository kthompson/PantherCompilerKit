import panther.{assert => panthAssert, *}
import TestHelpers.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}

/** Each `.scala` fixture transpiled to Panther, held to a snapshot.
  *
  * `TranspilerTests` asserts on one-liners inline, which is right for the
  * handful of places the transpiler rewrites rather than copies. These cover
  * the other half: that a whole file comes through with its structure,
  * indentation and comments intact, which is not something an inline
  * expectation can show.
  *
  * The `pnc/src` tree is a transpiler snapshot too, and a much larger one, but
  * it changes whenever the compiler's own sources do. These do not, so a diff
  * here is always about the transpiler.
  *
  * {{{
  *   UPDATE_SNAPSHOTS=1 sbt "test/testOnly TranspileSnapshotTests"
  * }}}
  */
class TranspileSnapshotTests extends AnyFunSpec with Matchers {

  private val fixturesDir = Paths.get("test/fixtures/transpile")
  private val snapshotsDir = fixturesDir.resolve("snapshots")
  private val updating = sys.env.get("UPDATE_SNAPSHOTS").contains("1")

  private def fixtures: Seq[Path] = {
    val stream = Files.list(fixturesDir)
    try {
      stream
        .filter(p => p.getFileName.toString.endsWith(".scala"))
        .sorted()
        .toArray
        .toSeq
        .map(_.asInstanceOf[Path])
    } finally stream.close()
  }

  describe("transpiler snapshots") {
    it("should have at least one fixture") {
      fixtures should not be empty
    }

    fixtures.foreach { file =>
      val fileName = file.getFileName.toString
      val name = fileName.substring(0, fileName.length - 6)

      it("should transpile " + name) {
        val actual = mkTranspiled(Files.readString(file))
        val snapshot = snapshotsDir.resolve(name + ".pn")

        if (updating) {
          Files.createDirectories(snapshotsDir)
          Files.writeString(snapshot, actual)
        } else if (!Files.exists(snapshot)) {
          fail(
            "no snapshot at " + snapshot + "\n" +
              "run: UPDATE_SNAPSHOTS=1 sbt \"test/testOnly TranspileSnapshotTests\""
          )
        } else {
          actual shouldBe Files.readString(snapshot)
        }
      }
    }
  }
}
