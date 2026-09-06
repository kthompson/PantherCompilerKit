import panther.*
import TestHelpers.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

/** The transpiler is mostly a token walk, so what is worth testing is the
  * handful of places it rewrites rather than copies. Derivation is the largest
  * of them: Scala generates structural equality and printing for `case class`
  * and `enum` alike, Panther has one kind of class, and `[derive(Eq, Show)]` is
  * what carries the difference across
  * ([ADR 0004](../../../../docs/architecture/adr/0004-traits-given-evidence-and-contextual-extensions.md)).
  */
class TranspilerTests extends AnyFunSpec with Matchers {

  describe("Transpiler") {
    it("should derive Eq and Show for a case class") {
      mkTranspiled("case class Point(x: int, y: int)") shouldBe
        "[derive(Eq, Show)] class Point(x: int, y: int)"
    }

    it("should leave a plain class without an attribute") {
      mkTranspiled("class Heap(size: int)") shouldBe "class Heap(size: int)"
    }

    /** The attribute is written where `case` was, so whatever indented the
      * declaration still indents it and the members below are untouched.
      */
    it("should keep a case class's surroundings") {
      mkTranspiled(
        "object Shapes {\n  case class Circle(r: int)\n}"
      ) shouldBe "object Shapes {\n  [derive(Eq, Show)] class Circle(r: int)\n}"
    }

    /** A `[` opening a line is an attribute, not the type arguments of the name
      * that ended the line before it. Every transpiled file starts with usings,
      * so this shape is the common one rather than a corner.
      */
    it("should not read an attribute as the preceding using's type arguments") {
      val transpiled = mkTranspiled(
        "import panther._\n\ncase class TextSpan(start: int)"
      )
      transpiled should include("[derive(Eq, Show)] class TextSpan")
      treeDiagnosticMessages(mkSyntaxTree(transpiled)) shouldBe empty
    }

    /** Scala's `enum` generates structural equality the same way `case class`
      * does. There is no `case` keyword to write the attribute over, so it goes
      * between the `enum` keyword's leading trivia and the keyword.
      */
    it("should derive Eq and Show for an enum") {
      mkTranspiled(
        "enum Color {\n  case Red\n  case Green\n}"
      ) shouldBe "[derive(Eq, Show)] enum Color {\n  case Red\n  case Green\n}"
    }

    it("should derive for a generic enum") {
      mkTranspiled(
        "enum Opt[T] {\n  case Has(value: T)\n}"
      ) shouldBe "[derive(Eq, Show)] enum Opt[T] {\n  case Has(value: T)\n}"
    }

    /** The indentation and any doc comment are the keyword's leading trivia, so
      * they have to come out before the attribute rather than after it.
      */
    it("should keep an enum's indentation and doc comment") {
      mkTranspiled(
        "object Shapes {\n  /** a colour */\n  enum Color {\n    case Red\n  }\n}"
      ) shouldBe
        "object Shapes {\n  /** a colour */\n  [derive(Eq, Show)] enum Color {\n    case Red\n  }\n}"
    }

    /** What the transpiled sources are made of: the attribute has to survive a
      * round trip through the Panther parser it is written for.
      */
    it("should emit an attribute the parser reads back") {
      val decl = mkClassMember(mkTranspiled("case class Point(x: int)"))
      decl.derives match {
        case Option.Some(attribute) =>
          derivedNames(attribute) shouldBe Seq("Eq", "Show")
        case Option.None => fail("no derive attribute")
      }
    }
  }
}
