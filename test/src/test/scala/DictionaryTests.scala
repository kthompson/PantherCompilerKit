import TestHelpers._
import java.nio.file.{Files, Paths}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import panther.*

class DictionaryTests extends AnyFunSpec with Matchers {
  private def execValueWithHeap(program: string, heapSize: int): Value = {
    val settings = CompilerSettingsFactory.default.copy(
      stackSize = 65536,
      heapSize = heapSize
    )
    val tree = MakeSyntaxTree.parseContent(program, settings)
    val compilation = MakeCompilation.create(ListModule.one(tree), settings)
    compilation.diagnostics.count() shouldBe 0
    compilation.exec() match {
      case InterpretResult.OkValue(value) => value
      case result =>
        throw new AssertionError("Expected exec result, got: " + result)
    }
  }

  describe("Dictionary") {
    it("inserts new keys and replaces existing keys") {
      val empty = DictionaryModule.empty[String, Int]()
      val one = empty.put("a", 1)
      val two = one.put("b", 2)
      val replaced = two.put("a", 40)

      empty.length shouldBe 0
      one.length shouldBe 1
      two.length shouldBe 2
      replaced.length shouldBe 2
      replaced.getUnsafe("a") shouldBe 40
      replaced.getUnsafe("b") shouldBe 2
    }
  }

  describe("IntDictionary") {
    it("keeps colliding entries reachable after replacement and removal") {
      val dictionary = IntDictionaryModule.empty[String]()

      // All three initially land in bucket 1 of the four-slot table.
      dictionary.put(1, "one")
      dictionary.put(5, "five")
      dictionary.put(9, "nine")
      dictionary.put(5, "FIVE")
      dictionary.remove(1)

      dictionary.length() shouldBe 2
      dictionary.get(1) shouldBe Option.None
      dictionary.getUnsafe(5) shouldBe "FIVE"
      dictionary.getUnsafe(9) shouldBe "nine"
    }

    it("resizes without losing entries or negative keys") {
      val dictionary = IntDictionaryModule.empty[Int]()
      var key = -40

      while (key <= 80) {
        dictionary.put(key, key * 3)
        key += 1
      }

      dictionary.length() shouldBe 121
      dictionary.getUnsafe(-40) shouldBe -120
      dictionary.getUnsafe(0) shouldBe 0
      dictionary.getUnsafe(80) shouldBe 240
    }

    it("pages a logical capacity beyond 32K without losing entries") {
      val dictionary = IntDictionaryModule.empty[Int]()
      var key = -13000

      while (key <= 13000) {
        dictionary.put(key, key * 3)
        key += 1
      }

      dictionary.length() shouldBe 26001
      dictionary.getUnsafe(-13000) shouldBe -39000
      dictionary.getUnsafe(0) shouldBe 0
      dictionary.getUnsafe(13000) shouldBe 39000

      key = -13000
      while (key <= 13000) {
        dictionary.getUnsafe(key) shouldBe key * 3
        key += 1
      }
    }

    it("handles a partial final page across colliding and wrapping probes") {
      val dictionary = new IntDictionary[Int](257)
      dictionary.put(255, 10)
      dictionary.put(512, 20)
      dictionary.put(256, 30)
      dictionary.put(-1, 40)
      dictionary.put(Int.MinValue, 50)

      dictionary.getUnsafe(255) shouldBe 10
      dictionary.getUnsafe(512) shouldBe 20
      dictionary.getUnsafe(256) shouldBe 30
      dictionary.getUnsafe(-1) shouldBe 40
      dictionary.getUnsafe(Int.MinValue) shouldBe 50
    }

    it("rebuilds tombstones during churn and guards a nonpositive capacity") {
      val dictionary = new IntDictionary[Int](0)
      var key = 0

      while (key < 32) {
        dictionary.put(key, key)
        dictionary.remove(key)
        key += 1
      }

      dictionary.put(100, 200)
      dictionary.length() shouldBe 1
      dictionary.getUnsafe(100) shouldBe 200
    }

    it("executes the generated Panther implementation in the VM") {
      val tuple2 = Files.readString(Paths.get("pnc/src/Tuple2.pn"))
      val option = Files
        .readString(Paths.get("pnc/src/Option.pn"))
        .replace("using panther\n", "")
      val implementation =
        Files
          .readString(Paths.get("pnc/src/IntDictionary.pn"))
          .replace("using panther\n", "")
      val program =
        "val dictionary = IntDictionaryModule.empty[int]()\n" +
          "dictionary.put(1, 10)\n" +
          "dictionary.put(5, 50)\n" +
          "dictionary.put(5, 55)\n" +
          "dictionary.put(-3, 30)\n" +
          "dictionary.put(9, 90)\n" +
          "dictionary.remove(1)\n" +
          "var key = 20\n" +
          "while (key < 52) {\n" +
          "  dictionary.put(key, key)\n" +
          "  dictionary.remove(key)\n" +
          "  key = key + 1\n" +
          "}\n" +
          "dictionary.put(100, 100)\n" +
          "dictionary.getUnsafe(5) + dictionary.getUnsafe(-3) + dictionary.getUnsafe(9) + dictionary.getUnsafe(100)"

      assertValueInt(
        execValue(
          tuple2 + "\n" + option + "\n" + implementation + "\n" + program
        ),
        275
      )
    }

    it("executes a generated paged table beyond 32K logical capacity") {
      val tuple2 = Files.readString(Paths.get("pnc/src/Tuple2.pn"))
      val option = Files
        .readString(Paths.get("pnc/src/Option.pn"))
        .replace("using panther\n", "")
      val implementation = Files
        .readString(Paths.get("pnc/src/IntDictionary.pn"))
        .replace("using panther\n", "")
      val program =
        "val dictionary = IntDictionaryModule.empty[int]()\n" +
          "var key = -13000\n" +
          "while (key <= 13000) {\n" +
          "  dictionary.put(key, key * 3)\n" +
          "  key = key + 1\n" +
          "}\n" +
          "dictionary.length() + dictionary.getUnsafe(-13000) + dictionary.getUnsafe(13000) * 2"

      assertValueInt(
        execValueWithHeap(
          tuple2 + "\n" + option + "\n" + implementation + "\n" + program,
          524288
        ),
        65001
      )
    }

  }
}
