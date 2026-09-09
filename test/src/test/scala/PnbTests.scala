import panther.{assert => panthAssert, *}
import TestHelpers.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/** A compiled image has to run the same way the compilation it came from does.
  *
  * Every assertion here compares a program executed from a `.pnb` against the
  * same program executed in memory, because "it round-trips" is only
  * interesting if what comes back behaves identically — a table that
  * deserializes into the wrong shape still deserializes.
  */
class PnbTests extends AnyFunSpec with Matchers {

  /** Compiles, writes an image, reads it back, and runs that. */
  private def execViaImage(source: string): InterpretResult = {
    val file = Files.createTempFile("panther-image", ".pnb")
    try {
      val settings = CompilerSettingsFactory.default
      mkCompilation(source).emit(file.toString)
      ImageRunner.exec(file.toString, settings)
    } finally {
      Files.deleteIfExists(file)
      ()
    }
  }

  /** The property the whole format exists for. */
  private def assertSameThroughImage(source: string): Unit = {
    val direct = mkCompilation(source).exec()
    val viaImage = execViaImage(source)
    withClue(source) { viaImage shouldBe direct }
  }

  /** The int-to-bytes encoding, on its own.
    *
    * A whole program only happens to contain the values that would expose a bad
    * one — a middle byte of 0xFF in an otherwise small number corrupts
    * everything above it if the sign is not masked off, and no test program
    * reliably contains one.
    */
  describe("PnbFile encoding") {
    def roundTrip(values: Seq[Int]): Seq[Int] = {
      val list = new IntList()
      values.foreach(list.add)
      val back = PnbFile.fromBytes(PnbFile.toBytes(list))
      (0 until back.size).map(back.read)
    }

    it("should round-trip small values") {
      roundTrip(Seq(0, 1, 2, 127, 128, 255, 256)) shouldBe
        Seq(0, 1, 2, 127, 128, 255, 256)
    }

    it("should round-trip a high byte in every position") {
      val values = Seq(0x000000ff, 0x0000ff00, 0x00ff0000, 0x7f000000)
      roundTrip(values) shouldBe values
    }

    it("should round-trip negative values") {
      // -1 is every byte set, and a method with no body has address -1, so
      // this is in every image
      roundTrip(Seq(-1, -2, -256, -65536, Int.MinValue)) shouldBe
        Seq(-1, -2, -256, -65536, Int.MinValue)
    }

    it("should use four bytes per int") {
      val list = new IntList()
      list.add(1)
      list.add(2)
      PnbFile.toBytes(list).length shouldBe 8
    }

    /** Through an actual file, which is the only way the sign matters:
      * `toBytes` hands back 0..255, but a byte read back off disk arrives
      * signed, so 0xFF is -1 and floods every bit above it unless it is masked.
      */
    it("should round-trip high bytes through a file") {
      val file = Files.createTempFile("panther-bytes", ".bin")
      try {
        val values =
          Seq(0x00ff0000, 0x0000ff00, 0x00ffff00, -1, -2, 0x7fffffff, 305419896)
        val list = new IntList()
        values.foreach(list.add)

        system.io.File.writeAllBytes(file.toString, PnbFile.toBytes(list))
        val back = PnbFile.fromBytes(
          system.io.File.readAllBytes(file.toString)
        )

        (0 until back.size).map(back.read) shouldBe values
      } finally {
        Files.deleteIfExists(file)
        ()
      }
    }
  }

  describe("PnbFile") {
    it("should write a file that starts with the magic and version") {
      val file = Files.createTempFile("panther-image", ".pnb")
      try {
        mkCompilation("val x = 1").emit(file.toString)
        val bytes = Files.readAllBytes(file)
        bytes.length should be > 12
        // "PNB\0", most significant byte first
        bytes(0) shouldBe 'P'.toByte
        bytes(1) shouldBe 'N'.toByte
        bytes(2) shouldBe 'B'.toByte
        bytes(3) shouldBe 0.toByte
        // version 1 in the next four
        bytes(4) shouldBe 0.toByte
        bytes(7) shouldBe 1.toByte
      } finally {
        Files.deleteIfExists(file)
        ()
      }
    }

    it("should refuse a file that is not an image") {
      val file = Files.createTempFile("panther-image", ".pnb")
      try {
        Files.write(file, Array[Byte](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
        val thrown = intercept[Exception] {
          ImageRunner.exec(file.toString, CompilerSettingsFactory.default)
        }
        thrown.getMessage should include("not a .pnb file")
      } finally {
        Files.deleteIfExists(file)
        ()
      }
    }

    it("should round-trip a value") {
      assertSameThroughImage("42")
      assertSameThroughImage("1 + 2")
      assertSameThroughImage("true")
    }

    /** Strings are the table that had no serializer at all, so an image used to
      * come back with names for nothing and no literals to load.
      */
    it("should round-trip string literals") {
      assertSameThroughImage("\"hello\"")
      assertSameThroughImage("\"a\" + \"b\"")
      assertSameThroughImage("val s = \"hello\"\ns.substring(1, 3)")
      // an empty string is the length-zero record
      assertSameThroughImage("\"\"")
    }

    it("should round-trip a program with control flow") {
      assertSameThroughImage(
        "var i = 0\nvar total = 0\nwhile (i < 5) {\n" +
          "  total = total + i\n  i = i + 1\n}\ntotal"
      )
      assertSameThroughImage("if (1 < 2) 10 else 20")
    }

    /** Fields are the table whose reader strode three ints through four-int
      * records.
      */
    it("should round-trip a program with classes and fields") {
      assertSameThroughImage(
        "class Point(x: int, y: int)\nval p = new Point(3, 4)\np.x + p.y"
      )
    }

    it("should round-trip a program with methods") {
      assertSameThroughImage(
        "def twice(n: int): int = n * 2\ntwice(21)"
      )
    }

    it("should round-trip a program that prints") {
      execOutputViaImage("println(\"from an image\")") shouldBe
        "from an image\n"
    }

    it("should round-trip a program that exits") {
      execViaImage("exit(3)") shouldBe InterpretResult.Exit(3)
    }

    /** The entry point is one int in the header, and getting it wrong runs the
      * wrong method rather than failing.
      */
    it("should keep the entry point") {
      assertSameThroughImage("def unused(): int = 99\n7")
    }
  }

  private def execOutputViaImage(source: string): String = {
    val buffer = new java.io.ByteArrayOutputStream()
    Console.withOut(buffer) {
      execViaImage(source)
    }
    buffer.toString("utf-8")
  }
}
