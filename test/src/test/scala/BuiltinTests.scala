import panther.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

/** The builtin table is the operand encoding for `Opcode.Callx`, so the emitter
  * and the VM have to read it the same way. `idOf` and `nameOf` being inverses
  * is what makes that true.
  */
class BuiltinTests extends AnyFunSpec with Matchers {

  /** Every builtin, by the qualified name the emitter looks it up under. */
  private val names = Seq(
    "println",
    "print",
    "panic",
    "exit",
    "assert",
    "mod",
    "string.apply",
    "int.apply",
    "bool.apply",
    "char.apply",
    "string.substring",
    "string.endsWith",
    "string.compareTo",
    "int.compareTo",
    "File.readAllText",
    "File.writeAllText",
    "Path.combine",
    "Path.nameWithoutExtension"
  )

  describe("Builtin") {
    it("should map every name to an id") {
      for (name <- names) {
        withClue(name) { Builtin.idOf(name) should not be Builtin.None }
      }
    }

    it("should round-trip every name through its id") {
      for (name <- names) {
        withClue(name) { Builtin.nameOf(Builtin.idOf(name)) shouldBe name }
      }
    }

    /** A shifted or duplicated entry would still round-trip name-to-name for
      * some subset, but two builtins sharing an id means one of them runs the
      * other's implementation.
      */
    it("should give every builtin a distinct id") {
      val ids = names.map(Builtin.idOf)
      ids.distinct.length shouldBe names.length
    }

    /** `apply` is declared on four types and `compareTo` on two. The emitter
      * used to dispatch on the bare name, which cannot tell these apart — it
      * special-cased `apply` by parent name and would have mis-dispatched any
      * further collision.
      */
    it("should tell same-named builtins on different types apart") {
      val applies = Seq(
        "string.apply",
        "int.apply",
        "bool.apply",
        "char.apply"
      ).map(Builtin.idOf)
      applies.distinct.length shouldBe 4

      Builtin.idOf("string.compareTo") should not be
        Builtin.idOf("int.compareTo")
    }

    /** What the emitter's guard tests: an extern method that names no builtin
      * fails the compile rather than emitting something meaningless.
      */
    it("should answer None for a name that is not a builtin") {
      Builtin.idOf("notABuiltin") shouldBe Builtin.None
      Builtin.idOf("") shouldBe Builtin.None
      // the bare names the old chain dispatched on are not keys any more
      Builtin.idOf("apply") shouldBe Builtin.None
      Builtin.idOf("substring") shouldBe Builtin.None
      Builtin.idOf("compareTo") shouldBe Builtin.None
    }
  }
}
