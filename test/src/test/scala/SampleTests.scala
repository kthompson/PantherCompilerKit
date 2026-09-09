import panther.{assert => panthAssert, *}
import TestHelpers.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}

/** Every sample in `samples/`, compiled and run, with its output and three
  * views of its compilation held to a snapshot.
  *
  * The samples are the only end-to-end coverage in the repo: everything else
  * tests one stage against a hand-written expectation. These take a real
  * program the whole way and pin what came out, so a change that alters
  * behaviour has to say so in a diff rather than passing quietly.
  *
  * Snapshots live beside the sample in `samples/snapshots/`. To update them
  * after an intended change:
  *
  * {{{
  *   UPDATE_SNAPSHOTS=1 sbt "test/testOnly SampleTests"
  * }}}
  *
  * and read the diff before committing it — a snapshot that changed for a
  * reason you cannot state is a regression you just accepted.
  */
class SampleTests extends AnyFunSpec with Matchers {

  private val samplesDir = Paths.get("samples")
  private val snapshotsDir = samplesDir.resolve("snapshots")
  private val updating = sys.env.get("UPDATE_SNAPSHOTS").contains("1")

  private def sampleFiles: Seq[Path] = {
    val stream = Files.list(samplesDir)
    try {
      stream
        .filter(p => p.getFileName.toString.endsWith(".pn"))
        .sorted()
        .toArray
        .toSeq
        .map(_.asInstanceOf[Path])
    } finally stream.close()
  }

  private def sampleName(file: Path): String = {
    val name = file.getFileName.toString
    name.substring(0, name.length - 3)
  }

  /** Compares against the stored snapshot, or writes it when updating.
    *
    * A missing snapshot fails rather than passing silently: a new sample with
    * no recorded output is untested, and the fix is to run with
    * `UPDATE_SNAPSHOTS=1` and read what it wrote.
    */
  private def checkSnapshot(
      name: String,
      kind: String,
      actual: String
  ): Unit = {
    val file = snapshotsDir.resolve(name + "." + kind + ".txt")
    if (updating) {
      Files.createDirectories(snapshotsDir)
      Files.writeString(file, actual)
    } else if (!Files.exists(file)) {
      fail(
        "no snapshot at " + file + "\n" +
          "run: UPDATE_SNAPSHOTS=1 sbt \"test/testOnly SampleTests\""
      )
    } else {
      withClue(name + "." + kind) {
        actual shouldBe Files.readString(file)
      }
    }
  }

  /** What the program printed, and how it ended. */
  private def runSample(source: String): String = {
    val buffer = new java.io.ByteArrayOutputStream()
    val result = Console.withOut(buffer) {
      mkCompilation(source).exec()
    }

    val ending = result match {
      case InterpretResult.Exit(code)     => "exit " + code
      case InterpretResult.RuntimeError   => "runtime error"
      case InterpretResult.CompileError   => "compile error"
      case InterpretResult.OkValue(value) => "ok"
      case _                              => "ok"
    }

    buffer.toString("utf-8") + "--- " + ending + "\n"
  }

  describe("samples") {
    it("should have at least one sample") {
      sampleFiles should not be empty
    }

    sampleFiles.foreach { file =>
      val name = sampleName(file)

      describe(name) {
        lazy val source = Files.readString(file)

        it("should compile without diagnostics") {
          val comp = mkFailingCompilation(source)
          withClue(diagnosticMessages(comp).mkString("\n")) {
            comp.diagnostics.count() shouldBe 0
          }
        }

        it("should produce the expected output") {
          checkSnapshot(name, "out", runSample(source))
        }

        /** The binder's answer: every symbol and the type it was given. This is
          * where a type that went missing shows up.
          */
        it("should produce the expected symbols") {
          checkSnapshot(
            name,
            "symbols",
            mkCompilation(source).symbolsText(false)
          )
        }

        /** The desugaring: `while` to labels and gotos, `match` to tests,
          * arguments hoisted to temporaries.
          */
        it("should produce the expected lowered assembly") {
          checkSnapshot(
            name,
            "lowered",
            mkCompilation(source).loweredAssemblyText(false)
          )
        }

        /** The instructions themselves. */
        it("should produce the expected disassembly") {
          checkSnapshot(name, "disasm", mkCompilation(source).disassemblyText())
        }
      }
    }
  }
}
