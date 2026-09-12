import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DictionaryTests extends AnyFunSpec with Matchers {
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
}
