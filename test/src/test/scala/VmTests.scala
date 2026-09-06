import TestHelpers._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class VmTests extends AnyFunSpec with Matchers {

  describe("VM") {
    it("should execute constants") {
      assertExecValueInt("12", 12)
      assertExecValueInt("0", 0)
      assertExecValueInt("-5", -5)
      assertExecValueBool("true", true)
      assertExecValueBool("false", false)
      assertExecValueString("\"hello\"", "hello")
    }

    it("should execute unary operations") {
      assertExecValueInt("-12", -12)
      assertExecValueInt("+12", 12)
      assertExecValueBool("!true", false)
      assertExecValueBool("!false", true)
    }

    it("should execute binary operations") {
      assertExecValueInt("1 + 2", 3)
      assertExecValueInt("5 - 3", 2)
      assertExecValueInt("4 * 3", 12)
      assertExecValueInt("15 / 3", 5)
      assertExecValueInt("17 % 5", 2)
      assertExecValueInt("17 & 5", 1)
      assertExecValueInt("17 | 5", 21)

      assertExecValueBool("5 == 5", true)
      assertExecValueBool("5 == 3", false)
      assertExecValueBool("5 != 3", true)
      assertExecValueBool("5 != 5", false)
      assertExecValueBool("5 < 10", true)
      assertExecValueBool("10 < 5", false)
      assertExecValueBool("5 <= 5", true)
      assertExecValueBool("5 <= 3", false)
      assertExecValueBool("10 > 5", true)
      assertExecValueBool("5 > 10", false)
      assertExecValueBool("5 >= 5", true)
      assertExecValueBool("3 >= 5", false)

      assertExecValueBool("true && true", true)
      assertExecValueBool("true && false", false)
      assertExecValueBool("false && true", false)
      assertExecValueBool("false && false", false)
      assertExecValueBool("true || true", true)
      assertExecValueBool("true || false", true)
      assertExecValueBool("false || true", true)
      assertExecValueBool("false || false", false)
    }

    /** Evidence passing end to end: `$runtimeInit` builds a record of method
      * tokens for the given, the call site pushes it after the declared
      * arguments, and `a.equals(b)` inside the constrained function reads the
      * member's slot out of it and dispatches with `Calli`.
      */
    it("should call a trait member through evidence") {
      val setup =
        "trait Eqv[T] { def equals(a: T, b: T): bool }\n" +
          "given Eqv[int] { def equals(a: int, b: int): bool = a == b }\n" +
          "def same[T: Eqv](a: T, b: T): bool = a.equals(b)"

      assertExecValueBoolWithSetup(setup, "same(3, 3)", true)
      assertExecValueBoolWithSetup(setup, "same(3, 4)", false)
    }

    /** Two givens for the same trait: the call site has to select the record
      * that matches the type argument, not whichever was declared first.
      */
    it("should select evidence by type argument") {
      val setup =
        "trait Eqv[T] { def equals(a: T, b: T): bool }\n" +
          "given Eqv[int] { def equals(a: int, b: int): bool = a == b }\n" +
          "given Eqv[string] { def equals(a: string, b: string): bool = a == b }\n" +
          "def same[T: Eqv](a: T, b: T): bool = a.equals(b)"

      assertExecValueBoolWithSetup(setup, "same(1, 1)", true)
      assertExecValueBoolWithSetup(setup, "same(1, 2)", false)
      assertExecValueBoolWithSetup(setup, "same(\"ab\", \"ab\")", true)
      assertExecValueBoolWithSetup(setup, "same(\"ab\", \"cd\")", false)
    }

    /** A record has one slot per trait member, so calling the second member
      * has to read slot 1. Getting the layout wrong would silently call the
      * other method, which is why both are exercised.
      */
    it("should index the right member of a multi-member trait") {
      val setup =
        "trait Ranked[T] {\n" +
          "  def lt(a: T, b: T): bool\n" +
          "  def gt(a: T, b: T): bool\n" +
          "}\n" +
          "given Ranked[int] {\n" +
          "  def lt(a: int, b: int): bool = a < b\n" +
          "  def gt(a: int, b: int): bool = a > b\n" +
          "}\n" +
          "def smaller[T: Ranked](a: T, b: T): bool = a.lt(b)\n" +
          "def bigger[T: Ranked](a: T, b: T): bool = a.gt(b)"

      assertExecValueBoolWithSetup(setup, "smaller(2, 9)", true)
      assertExecValueBoolWithSetup(setup, "smaller(9, 2)", false)
      assertExecValueBoolWithSetup(setup, "bigger(9, 2)", true)
      assertExecValueBoolWithSetup(setup, "bigger(2, 9)", false)
    }

    /** ADR 0006: a conditional given using its premise. `Eqv[Box[int]]` is
      * proved by the conditional given, whose body needs `Eqv[int]` — which is
      * not a symbol it can name, but slot 1 of the record it was reached
      * through: slot 0 is `equals`, and the dependency half starts after it.
      */
    it("should let a conditional given use its premise") {
      val setup =
        "trait Eqv[T] { def equals(a: T, b: T): bool }\n" +
          "class Box[T](value: T)\n" +
          "given Eqv[int] { def equals(a: int, b: int): bool = a == b }\n" +
          "given [T: Eqv] => Eqv[Box[T]] {\n" +
          "  def equals(a: Box[T], b: Box[T]): bool = a.value.equals(b.value)\n" +
          "}\n" +
          "def same[T: Eqv](a: T, b: T): bool = a.equals(b)"

      assertExecValueBoolWithSetup(
        setup,
        "same(new Box[int](4), new Box[int](4))",
        true
      )
      assertExecValueBoolWithSetup(
        setup,
        "same(new Box[int](4), new Box[int](5))",
        false
      )
    }

    /** The premise is per instantiation, so the same conditional given has to
      * reach a different record depending on which one it was called through.
      * One record per goal is what makes that work.
      */
    it("should use the right premise at two instantiations") {
      val setup =
        "trait Eqv[T] { def equals(a: T, b: T): bool }\n" +
          "class Box[T](value: T)\n" +
          "given Eqv[int] { def equals(a: int, b: int): bool = a == b }\n" +
          "given Eqv[string] {\n" +
          "  def equals(a: string, b: string): bool = false\n" +
          "}\n" +
          "given [T: Eqv] => Eqv[Box[T]] {\n" +
          "  def equals(a: Box[T], b: Box[T]): bool = a.value.equals(b.value)\n" +
          "}\n" +
          "def same[T: Eqv](a: T, b: T): bool = a.equals(b)"

      assertExecValueBoolWithSetup(
        setup,
        "same(new Box[int](4), new Box[int](4))",
        true
      )
      // `Eqv[string]` answers false for everything, so the string instantiation
      // reaching the int record would show up here
      assertExecValueBoolWithSetup(
        setup,
        "same(new Box[string](\"a\"), new Box[string](\"a\"))",
        false
      )
    }

    /** The class half of evidence passing: the constructor stores its evidence
      * in a field, and an instance method reads it back out to dispatch.
      */
    it("should call a trait member through evidence held by a class") {
      val setup =
        "trait Eqv[T] { def equals(a: T, b: T): bool }\n" +
          "given Eqv[int] { def equals(a: int, b: int): bool = a == b }\n" +
          "class Box[T: Eqv](value: T) {\n" +
          "  def matches(other: T): bool = value.equals(other)\n" +
          "}"

      assertExecValueBoolWithSetup(setup, "new Box[int](7).matches(7)", true)
      assertExecValueBoolWithSetup(setup, "new Box[int](7).matches(8)", false)
    }

    /** ADR 0004's operator declarations, over the prelude's own `Eq` and its
      * `given Eq[int]`. `a == b` inside a constrained generic has no row in the
      * builtin table — the operands are a type parameter — so it resolves to
      * the member that claims `==` and dispatches through the record.
      *
      * Nothing is declared by the snippet, so this is also the end-to-end test
      * that the prelude's synthesized bodies actually run.
      */
    it("should run == through prelude evidence for a type parameter") {
      val setup = "def same[T: Eq](a: T, b: T): bool = a == b"

      assertExecValueBoolWithSetup(setup, "same(3, 3)", true)
      assertExecValueBoolWithSetup(setup, "same(3, 4)", false)
      assertExecValueBoolWithSetup(setup, "same(\"ab\", \"ab\")", true)
      assertExecValueBoolWithSetup(setup, "same(\"ab\", \"cd\")", false)
      assertExecValueBoolWithSetup(setup, "same(true, true)", true)
      assertExecValueBoolWithSetup(setup, "same(true, false)", false)
    }

    it("should run != through prelude evidence") {
      val setup = "def differs[T: Eq](a: T, b: T): bool = a != b"

      assertExecValueBoolWithSetup(setup, "differs(3, 4)", true)
      assertExecValueBoolWithSetup(setup, "differs(3, 3)", false)
    }

    /** One trait, four tokens. Each has to reach its own member, which is the
      * record-layout question again — with `<` and `>` a swap is visible
      * rather than silent.
      */
    it("should run each prelude comparison operator") {
      val setup =
        "def lt[T: Ord](a: T, b: T): bool = a < b\n" +
          "def le[T: Ord](a: T, b: T): bool = a <= b\n" +
          "def gt[T: Ord](a: T, b: T): bool = a > b\n" +
          "def ge[T: Ord](a: T, b: T): bool = a >= b"

      assertExecValueBoolWithSetup(setup, "lt(2, 9)", true)
      assertExecValueBoolWithSetup(setup, "lt(9, 2)", false)
      assertExecValueBoolWithSetup(setup, "le(2, 2)", true)
      assertExecValueBoolWithSetup(setup, "le(9, 2)", false)
      assertExecValueBoolWithSetup(setup, "gt(9, 2)", true)
      assertExecValueBoolWithSetup(setup, "gt(2, 9)", false)
      assertExecValueBoolWithSetup(setup, "ge(2, 2)", true)
      assertExecValueBoolWithSetup(setup, "ge(2, 9)", false)
    }

    /** `Show` claims no token, so `show` is reached as a contextual extension
      * — and its body is the conversion the language already has.
      */
    it("should run show through prelude evidence") {
      val setup = "def render[T: Show](value: T): string = value.show()"

      assertExecValueStringWithSetup(setup, "render(42)", "42")
      assertExecValueStringWithSetup(setup, "render(true)", "true")
      assertExecValueStringWithSetup(setup, "render(\"hi\")", "hi")
    }

    /** On a ground type the given is known while binding, and its members are
      * static, so this compiles to an ordinary call rather than a record read.
      * Both paths have to produce the same answer.
      *
      * This is also what settles evidence beating ADR 0003's identity rule:
      * two separately constructed `Box(4)` are different objects, so identity
      * would call them unequal.
      */
    it("should run == through a given for a ground type") {
      val setup =
        "class Box(value: int)\n" +
          "given Eq[Box] {\n" +
          "  operator ==(a: Box, b: Box): bool = a.value == b.value\n" +
          "  operator !=(a: Box, b: Box): bool = a.value != b.value\n" +
          "}"

      assertExecValueBoolWithSetup(setup, "new Box(4) == new Box(4)", true)
      assertExecValueBoolWithSetup(setup, "new Box(4) == new Box(5)", false)
      assertExecValueBoolWithSetup(setup, "new Box(4) != new Box(5)", true)
    }

    /** ADR 0004's derivation, end to end. `Eq` opens with reference identity
      * and then compares parameter by parameter, so two separately
      * constructed `Point(1, 2)` are equal where identity alone would say no.
      */
    it("should run a derived Eq") {
      val setup = "[derive(Eq)]\nclass Point(x: int, y: int)"

      assertExecValueBoolWithSetup(setup, "new Point(1, 2) == new Point(1, 2)", true)
      assertExecValueBoolWithSetup(setup, "new Point(1, 2) == new Point(1, 3)", false)
      assertExecValueBoolWithSetup(setup, "new Point(1, 2) == new Point(9, 2)", false)
      assertExecValueBoolWithSetup(setup, "new Point(1, 2) != new Point(1, 3)", true)
      assertExecValueBoolWithSetup(setup, "new Point(1, 2) != new Point(1, 2)", false)
    }

    /** Lexicographic over the parameters, in order: the first that differs
      * decides, so `y` is only consulted when the two `x` agree.
      */
    it("should run a derived Ord") {
      val setup = "[derive(Ord)]\nclass Point(x: int, y: int)"

      assertExecValueBoolWithSetup(setup, "new Point(1, 2) < new Point(1, 3)", true)
      assertExecValueBoolWithSetup(setup, "new Point(1, 3) < new Point(1, 2)", false)
      assertExecValueBoolWithSetup(setup, "new Point(1, 9) < new Point(2, 0)", true)
      assertExecValueBoolWithSetup(setup, "new Point(2, 2) < new Point(1, 3)", false)
      assertExecValueBoolWithSetup(setup, "new Point(1, 2) <= new Point(1, 2)", true)
      assertExecValueBoolWithSetup(setup, "new Point(1, 3) > new Point(1, 2)", true)
      assertExecValueBoolWithSetup(setup, "new Point(1, 2) >= new Point(1, 2)", true)
      assertExecValueBoolWithSetup(setup, "new Point(1, 2) >= new Point(1, 3)", false)
    }

    /** `"Name(" + show(p1) + ", " + … + ")"`, reached through a constrained
      * generic — `Show` claims no token, and a contextual extension on a
      * ground type is a gap ADR 0004 records.
      */
    it("should run a derived Show") {
      val render = "def render[T: Show](v: T): string = v.show()\n"

      assertExecValueStringWithSetup(
        render + "[derive(Show)]\nclass Point(x: int, y: int)",
        "render(new Point(1, 2))",
        "Point(1, 2)"
      )
      assertExecValueStringWithSetup(
        render + "[derive(Show)]\nclass Wrapper(name: string, on: bool)",
        "render(new Wrapper(\"hi\", true))",
        "Wrapper(hi, true)"
      )
      assertExecValueStringWithSetup(
        render + "[derive(Show)]\nclass Unit1(x: int)",
        "render(new Unit1(7))",
        "Unit1(7)"
      )
    }

    /** A contextual extension on a ground type. The given is known while
      * binding and its members are static, so `.show()` is an ordinary call —
      * reached only because `Point` has no `show` of its own.
      */
    it("should run show on a ground type") {
      assertExecValueStringWithSetup(
        "[derive(Show)]\nclass Point(x: int, y: int)",
        "new Point(1, 2).show()",
        "Point(1, 2)"
      )
      assertExecValueStringWithSetup(
        "class Point(x: int, y: int)\n" +
          "given Show[Point] {\n" +
          "  def show(value: Point): string = \"p\" + string(value.x)\n" +
          "}",
        "new Point(1, 2).show()",
        "p1"
      )
    }

    /** A member the type declares itself wins: the extension is only reached
      * after an ordinary lookup has failed.
      */
    it("should prefer an intrinsic member over an extension") {
      assertExecValueStringWithSetup(
        "[derive(Show)]\nclass Point(x: int, y: int) {\n" +
          "  def show(): string = \"mine\"\n" +
          "}",
        "new Point(1, 2).show()",
        "mine"
      )
    }

    /** A derived type whose parameter is itself derived. The inner given is
      * registered before any body is built, so the outer one finds it.
      */
    it("should run a derived Eq over a derived parameter") {
      val setup =
        "[derive(Eq)]\nclass Inner(v: int)\n" +
          "[derive(Eq)]\nclass Outer(inner: Inner, tag: string)"

      assertExecValueBoolWithSetup(
        setup,
        "new Outer(new Inner(1), \"a\") == new Outer(new Inner(1), \"a\")",
        true
      )
      assertExecValueBoolWithSetup(
        setup,
        "new Outer(new Inner(1), \"a\") == new Outer(new Inner(2), \"a\")",
        false
      )
      assertExecValueBoolWithSetup(
        setup,
        "new Outer(new Inner(1), \"a\") == new Outer(new Inner(1), \"b\")",
        false
      )
    }

    /** An enum's derived members match the cases first, then their parameters
      * (ADR 0004). Two different cases are never equal, and the `is` test for
      * `b` is nested rather than `&&`-ed because `&&` evaluates both sides and
      * the field reads are only safe once the case is known.
      */
    val shape = "[derive(Eq, Ord, Show)]\n" +
      "enum Shape {\n  case Circle(r: int)\n  case Rect(w: int, h: int)\n}\n"

    it("should run a derived Eq over an enum") {
      assertExecValueBoolWithSetup(
        shape,
        "Shape.Circle(1) == Shape.Circle(1)",
        true
      )
      assertExecValueBoolWithSetup(
        shape,
        "Shape.Circle(1) == Shape.Circle(2)",
        false
      )
      assertExecValueBoolWithSetup(
        shape,
        "Shape.Rect(1, 2) == Shape.Rect(1, 2)",
        true
      )
      assertExecValueBoolWithSetup(
        shape,
        "Shape.Rect(1, 2) == Shape.Rect(1, 3)",
        false
      )
      // different cases, whatever the parameters
      assertExecValueBoolWithSetup(
        shape,
        "Shape.Circle(1) == Shape.Rect(1, 2)",
        false
      )
      assertExecValueBoolWithSetup(
        shape,
        "Shape.Circle(1) != Shape.Rect(1, 2)",
        true
      )
    }

    /** Case order decides before any parameter does, so every `Circle` sorts
      * before every `Rect` regardless of what they hold.
      */
    it("should run a derived Ord over an enum") {
      assertExecValueBoolWithSetup(
        shape,
        "Shape.Circle(9) < Shape.Rect(1, 1)",
        true
      )
      assertExecValueBoolWithSetup(
        shape,
        "Shape.Rect(1, 1) < Shape.Circle(9)",
        false
      )
      // within a case, lexicographic over the parameters
      assertExecValueBoolWithSetup(
        shape,
        "Shape.Circle(1) < Shape.Circle(2)",
        true
      )
      assertExecValueBoolWithSetup(
        shape,
        "Shape.Rect(1, 1) < Shape.Rect(1, 2)",
        true
      )
      assertExecValueBoolWithSetup(
        shape,
        "Shape.Rect(2, 1) < Shape.Rect(1, 9)",
        false
      )
      assertExecValueBoolWithSetup(
        shape,
        "Shape.Circle(1) >= Shape.Circle(1)",
        true
      )
    }

    /** An enum prints as its case rather than as itself: `Circle(7)`, not
      * `Shape(7)`.
      */
    it("should run a derived Show over an enum") {
      assertExecValueStringWithSetup(shape, "Shape.Circle(7).show()", "Circle(7)")
      assertExecValueStringWithSetup(shape, "Shape.Rect(1, 2).show()", "Rect(1, 2)")
    }

    /** A case with no parameters is one value, not a constructor: every
      * mention of `Color.Red` is the same object, built once in
      * `$runtimeInit`. Without that, reference identity — which is what `==`
      * falls back to with no evidence — would call two of them different.
      */
    val color = "enum Color {\n  case Red\n  case Green\n  case Blue\n}\n"

    it("should make a parameterless enum case a single value") {
      val setup = color +
        "val a: Color = Color.Red\n" +
        "val b: Color = Color.Red\n" +
        "val c: Color = Color.Green\n"

      assertExecValueBoolWithSetup(setup, "a == b", true)
      assertExecValueBoolWithSetup(setup, "a == c", false)
    }

    it("should derive over an enum whose cases take no parameters") {
      val setup = "[derive(Eq, Ord, Show)]\n" + color

      assertExecValueBoolWithSetup(setup, "Color.Red == Color.Red", true)
      assertExecValueBoolWithSetup(setup, "Color.Red == Color.Green", false)
      assertExecValueBoolWithSetup(setup, "Color.Red < Color.Green", true)
      assertExecValueBoolWithSetup(setup, "Color.Blue < Color.Red", false)
      assertExecValueBoolWithSetup(setup, "Color.Green >= Color.Green", true)
      assertExecValueStringWithSetup(setup, "Color.Green.show()", "Green()")
    }

    /** Two classes with instance fields. A field's index is its offset within
      * the object, so the second class's fields have to start at 0 again —
      * numbered across types, they land outside the object and the next
      * allocation zeroes them.
      */
    it("should keep two classes' fields apart") {
      val setup = "class First(a: int, b: int)\nclass Second(c: int, d: int)"

      assertExecValueIntWithSetup(
        setup,
        "new First(1, 2).a + new Second(3, 4).d",
        5
      )
    }

    it("should compare strings lexicographically") {
      assertExecValueBool("\"apple\" < \"banana\"", true)
      assertExecValueBool("\"banana\" < \"apple\"", false)
      assertExecValueBool("\"apple\" <= \"apple\"", true)
      assertExecValueBool("\"banana\" > \"apple\"", true)
      assertExecValueBool("\"apple\" > \"banana\"", false)
      assertExecValueBool("\"apple\" >= \"apple\"", true)
      assertExecValueBool("\"apple\" >= \"banana\"", false)
    }

    it("should handle binary precedence correctly") {
      assertExecValueInt("1 + 2 * 3", 7) // 1 + (2 * 3)
      assertExecValueInt("2 * 3 + 1", 7) // (2 * 3) + 1
      assertExecValueInt("10 - 4 / 2", 8) // 10 - (4 / 2)
    }

    it("should handle unary precedence correctly") {
      assertExecValueInt("-2 + 3", 1) // (-2) + 3
      assertExecValueInt("-(2 + 3)", -5) // -(2 + 3)
    }

    it("should execute local variable declarations and usage") {
      assertExecValueIntWithSetup("val x = 12", "x", 12)
      assertExecValueIntWithSetup("val x = 12", "x + 7", 19)
      assertExecValueIntWithSetup("val x = 12", "7 + x", 19)
      assertExecValueIntWithSetup("val x = 12\nval y = x + 1", "x", 12)
      assertExecValueIntWithSetup("val x = 12\nval y = x + 1", "y", 13)
    }

    it("should execute if expressions") {
      assertExecValueInt("if (true) 1 else 2", 1)
      assertExecValueInt("if (false) 1 else 2", 2)
      assertExecValueInt("if (5 > 3) 10 else 20", 10)
      assertExecValueInt("if (3 > 5) 10 else 20", 20)
    }

    it("should execute function calls") {
      assertExecValueIntWithSetup(
        "def noargs(): int = 7",
        "noargs()",
        7
      )
      assertExecValueIntWithSetup(
        "def onearg(x: int): int = x + 1",
        "onearg(12)",
        13
      )
      assertExecValueIntWithSetup(
        "def twoargs(x: int, y: int): int = x + y",
        "twoargs(12, 13)",
        25
      )
      assertExecValueIntWithSetup(
        "def threeargs(x: int, y: int, z: int): int = x + y + z",
        "threeargs(12, 13, 14)",
        39
      )
    }

    it("should execute object methods") {
      val setup = "object TestObject {\n" +
        "  def testMethod(x: int): int = x + 1\n" +
        "  def testMethod2(x: int, y: int): int = x + y\n" +
        "  def noArgs(): int = 42\n" +
        "  def threeArgs(a: int, b: int, c: int): int = a * b + c\n" +
        "  def multiply(x: int, y: int): int = x * y\n" +
        "  def subtract(x: int, y: int): int = x - y\n" +
        "  def negate(x: int): int = -x\n" +
        "}\n"

      assertExecValueIntWithSetup(setup, "TestObject.testMethod(0)", 1)
      assertExecValueIntWithSetup(setup, "TestObject.testMethod(11)", 12)
      assertExecValueIntWithSetup(setup, "TestObject.testMethod(12)", 13)
      assertExecValueIntWithSetup(setup, "TestObject.testMethod2(11, 12)", 23)
      assertExecValueIntWithSetup(setup, "TestObject.testMethod2(12, 13)", 25)

      // No arguments
      assertExecValueIntWithSetup(setup, "TestObject.noArgs()", 42)

      // Three arguments
      assertExecValueIntWithSetup(
        setup,
        "TestObject.threeArgs(2, 3, 4)",
        10
      ) // 2 * 3 + 4
      assertExecValueIntWithSetup(
        setup,
        "TestObject.threeArgs(5, 6, 7)",
        37
      ) // 5 * 6 + 7
      assertExecValueIntWithSetup(
        setup,
        "TestObject.threeArgs(0, 10, 5)",
        5
      ) // 0 * 10 + 5

      // More arithmetic operations
      assertExecValueIntWithSetup(setup, "TestObject.multiply(7, 8)", 56)
      assertExecValueIntWithSetup(setup, "TestObject.multiply(-3, 4)", -12)
      assertExecValueIntWithSetup(setup, "TestObject.subtract(10, 3)", 7)
      assertExecValueIntWithSetup(setup, "TestObject.subtract(5, 8)", -3)
      assertExecValueIntWithSetup(setup, "TestObject.negate(42)", -42)
      assertExecValueIntWithSetup(setup, "TestObject.negate(-15)", 15)

      // Nested calls
      assertExecValueIntWithSetup(
        setup,
        "TestObject.testMethod(TestObject.noArgs())",
        43
      ) // testMethod(42) = 43
      assertExecValueIntWithSetup(
        setup,
        "TestObject.multiply(TestObject.testMethod(2), 5)",
        15
      ) // multiply(3, 5) = 15
    }

    it("should access object fields") {
      val setup = "object Taco {\n" +
        "  val field1 = 12\n" +
        "  val field2 = 13\n" +
        "}\n" +
        "object Calculator {\n" +
        "  val zero = 0\n" +
        "  val one = 1\n" +
        "  val negativeValue = -42\n" +
        "  val largeValue = 9999\n" +
        "}\n" +
        "object MathConstants {\n" +
        "  val pi = 3\n" + // Simplified for int
        "  val e = 2\n" + // Simplified for int
        "}\n"

      assertExecValueIntWithSetup(setup, "Taco.field1", 12)
      assertExecValueIntWithSetup(setup, "Taco.field2", 13)

      // Calculator object tests
      assertExecValueIntWithSetup(setup, "Calculator.zero", 0)
      assertExecValueIntWithSetup(setup, "Calculator.one", 1)
      assertExecValueIntWithSetup(setup, "Calculator.negativeValue", -42)
      assertExecValueIntWithSetup(setup, "Calculator.largeValue", 9999)

      // MathConstants tests
      assertExecValueIntWithSetup(setup, "MathConstants.pi", 3)
      assertExecValueIntWithSetup(setup, "MathConstants.e", 2)

      // Field access in expressions
      assertExecValueIntWithSetup(
        setup,
        "Taco.field1 + Taco.field2",
        25
      ) // 12 + 13
      assertExecValueIntWithSetup(
        setup,
        "Calculator.zero + Calculator.one",
        1
      ) // 0 + 1
      assertExecValueIntWithSetup(
        setup,
        "MathConstants.pi * MathConstants.e",
        6
      ) // 3 * 2
      assertExecValueIntWithSetup(
        setup,
        "Taco.field1 * Calculator.one",
        12
      ) // 12 * 1
      assertExecValueIntWithSetup(
        setup,
        "Calculator.largeValue - Taco.field1",
        9987
      ) // 9999 - 12

      // More complex expressions
      assertExecValueIntWithSetup(
        setup,
        "Taco.field1 + Taco.field2 * Calculator.one",
        25
      ) // 12 + 13 * 1
      assertExecValueIntWithSetup(
        setup,
        "(Taco.field1 + Taco.field2) * MathConstants.e",
        50
      ) // (12 + 13) * 2
      assertExecValueIntWithSetup(
        setup,
        "Calculator.largeValue + Calculator.negativeValue",
        9957
      ) // 9999 + (-42)

      // Test with unary operations
      assertExecValueIntWithSetup(setup, "-Taco.field1", -12)
      assertExecValueIntWithSetup(setup, "-Calculator.negativeValue", 42)
    }

    it("should execute classes without args") {
      assertExecValueIntWithSetup(
        "class Foo() {\n" +
          " def bar() = 12\n" +
          "}",
        "new Foo().bar()",
        12
      )
    }

    /** Constructor parameters become fields, and the constructor stores them.
      * Reading one back is the smallest check that it does.
      */
    it("should store constructor parameters into fields") {
      assertExecValueIntWithSetup("class Foo(x: int)", "new Foo(7).x", 7)
      assertExecValueIntWithSetup(
        "class Foo(x: int, y: int)",
        "new Foo(1, 2).y",
        2
      )
    }

    /** A bare `x` inside an instance method is `this.x`, which needs the
      * receiver pushed before `Ldfld` — the name itself carries none.
      */
    it("should read a constructor parameter field with no explicit receiver") {
      assertExecValueIntWithSetup(
        "class Foo(x: int, y: int) {\n" +
          "  def add(): int = x + y\n" +
          "}",
        "new Foo(12, 13).add()",
        25
      )
    }

    it("should read a constructor parameter field through this") {
      assertExecValueIntWithSetup(
        "class Foo(x: int, y: int) {\n" +
          "  def add(): int = this.x + this.y\n" +
          "}",
        "new Foo(12, 13).add()",
        25
      )
    }

    it("should construct a generic class") {
      assertExecValueIntWithSetup(
        "class Box[T](value: T) {\n" +
          "  def get(): T = value\n" +
          "}",
        "new Box[int](7).get()",
        7
      )
    }

    it("should pass arguments to an instance method") {
      // the receiver takes argument slot 0, so declared parameters start at 1
      assertExecValueIntWithSetup(
        "class Foo() {\n" +
          "  def bar(x: int): int = x + 1\n" +
          "}",
        "new Foo().bar(41)",
        42
      )
    }

    it("should pass several arguments to an instance method") {
      assertExecValueIntWithSetup(
        "class Foo() {\n" +
          "  def bar(a: int, b: int, c: int): int = a * b + c\n" +
          "}",
        "new Foo().bar(6, 7, 9)",
        51
      )
    }

    it("should read a field through this") {
      assertExecValueIntWithSetup(
        "class Foo() {\n" +
          "  var x = 42\n" +
          "  def get(): int = this.x\n" +
          "}",
        "new Foo().get()",
        42
      )
    }

    it("should use this alongside a declared parameter") {
      assertExecValueIntWithSetup(
        "class Foo() {\n" +
          "  var x = 40\n" +
          "  def add(y: int): int = this.x + y\n" +
          "}",
        "new Foo().add(2)",
        42
      )
    }

    it("should access class fields via field declaration") {
      assertExecValueIntWithSetup(
        "class Foo() {\n" +
          "  var x = 12\n" +
          "  var y = 13\n" +
          "}",
        "new Foo().x + new Foo().y",
        25
      )
    }

    it("should execute pattern matching with integer cases") {
      assertExecValueIntWithSetup(
        "val x = 1",
        "x match {\n  case 1 => 10\n  case 2 => 20\n  case _ => 0\n}",
        10
      )
      assertExecValueIntWithSetup(
        "val x = 2",
        "x match {\n  case 1 => 10\n  case 2 => 20\n  case _ => 0\n}",
        20
      )
      assertExecValueIntWithSetup(
        "val x = 5",
        "x match {\n  case 1 => 10\n  case 2 => 20\n  case _ => 0\n}",
        0
      )
    }

    it("should execute pattern matching with wildcard only") {
      assertExecValueIntWithSetup(
        "val x = 42",
        "x match {\n  case _ => 99\n}",
        99
      )
    }

    /** A bare name binds the scrutinee and always matches, so it is a
      * catch-all that can also be read.
      */
    it("should bind a variable pattern to the scrutinee") {
      assertExecValueIntWithSetup(
        "def f(x: int): int = x match {\n  case 1 => 100\n  case n => n + 1\n}",
        "f(7)",
        8
      )
      assertExecValueIntWithSetup(
        "def f(x: int): int = x match {\n  case 1 => 100\n  case n => n + 1\n}",
        "f(1)",
        100
      )
    }

    /** `case Color.Red` names a case without destructuring it. It binds
      * nothing, but it is still a test — every case used to take the first
      * branch.
      */
    val colorMatch =
      "enum Color {\n  case Red\n  case Green\n  case Blue\n}\n" +
        "def name(c: Color): string = c match {\n" +
        "  case Color.Red => \"r\"\n" +
        "  case Color.Green => \"g\"\n" +
        "  case Color.Blue => \"b\"\n}\n"

    it("should match a parameterless enum case") {
      assertExecValueStringWithSetup(colorMatch, "name(Color.Red)", "r")
      assertExecValueStringWithSetup(colorMatch, "name(Color.Green)", "g")
      assertExecValueStringWithSetup(colorMatch, "name(Color.Blue)", "b")
    }

    val shapeEnum =
      "enum Shape {\n  case Circle(r: int)\n  case Rect(w: int, h: int)\n}\n"

    it("should extract an enum case's parameters") {
      val setup = shapeEnum +
        "def area(s: Shape): int = s match {\n" +
        "  case Shape.Circle(r) => r * r * 3\n" +
        "  case Shape.Rect(w, h) => w * h\n}\n"

      assertExecValueIntWithSetup(setup, "area(Shape.Circle(2))", 12)
      assertExecValueIntWithSetup(setup, "area(Shape.Rect(3, 4))", 12)
      assertExecValueIntWithSetup(setup, "area(Shape.Rect(5, 6))", 30)
    }

    it("should test a literal inside an extract pattern") {
      val setup = shapeEnum +
        "def f(s: Shape): string = s match {\n" +
        "  case Shape.Rect(1, h) => \"thin\"\n" +
        "  case Shape.Rect(w, h) => \"wide\"\n" +
        "  case Shape.Circle(r) => \"round\"\n}\n"

      assertExecValueStringWithSetup(setup, "f(Shape.Rect(1, 4))", "thin")
      assertExecValueStringWithSetup(setup, "f(Shape.Rect(2, 4))", "wide")
      assertExecValueStringWithSetup(setup, "f(Shape.Circle(2))", "round")
    }

    it("should ignore a discarded parameter in an extract pattern") {
      val setup = shapeEnum +
        "def width(s: Shape): int = s match {\n" +
        "  case Shape.Rect(w, _) => w\n" +
        "  case _ => 0\n}\n"

      assertExecValueIntWithSetup(setup, "width(Shape.Rect(3, 4))", 3)
      assertExecValueIntWithSetup(setup, "width(Shape.Circle(9))", 0)
    }

    it("should destructure a class") {
      assertExecValueIntWithSetup(
        "class Point(x: int, y: int)\n" +
          "def sum(p: Point): int = p match {\n  case Point(x, y) => x + y\n}\n",
        "sum(new Point(3, 4))",
        7
      )
    }

    /** A nested pattern's test only runs once the outer one has matched, and
      * its binding reads a field of a field.
      */
    it("should match a nested extract pattern") {
      val setup = "enum Option[out T] {\n  case Some(value: T)\n  case None\n}\n" +
        "def f(o: Option[Option[int]]): int = o match {\n" +
        "  case Option.Some(Option.Some(v)) => v\n" +
        "  case Option.Some(Option.None) => -1\n" +
        "  case Option.None => -2\n}\n"

      assertExecValueIntWithSetup(setup, "f(Option.Some(Option.Some(5)))", 5)
      assertExecValueIntWithSetup(setup, "f(Option.Some(Option.None))", -1)
      assertExecValueIntWithSetup(setup, "f(Option.None)", -2)
    }

    /** The annotation on `case x: int` is a test as well as a type. Without
      * it the first annotated case caught everything.
      */
    it("should test a type assertion pattern") {
      val setup = "def f(v: any): string = v match {\n" +
        "  case s: string => \"str\"\n" +
        "  case i: int => \"int\"\n" +
        "  case _ => \"other\"\n}\n"

      assertExecValueStringWithSetup(setup, "f(1)", "int")
      assertExecValueStringWithSetup(setup, "f(\"x\")", "str")
      assertExecValueStringWithSetup(setup, "f(true)", "other")
    }

    it("should bind a type assertion pattern") {
      val setup =
        "def f(v: any): int = v match {\n  case i: int => i + 1\n  case _ => 0\n}\n"

      assertExecValueIntWithSetup(setup, "f(41)", 42)
      assertExecValueIntWithSetup(setup, "f(\"x\")", 0)
    }

    /** `a.b` used as a place is the value of `b`. Emitting only `a` dropped a
      * level from every chain longer than one, which nested patterns are the
      * first thing to build.
      */
    it("should read a field of a field") {
      assertExecValueIntWithSetup(
        "class Inner(v: int)\nclass Outer(inner: Inner)\n" +
          "val o = new Outer(new Inner(7))\n",
        "o.inner.v",
        7
      )
    }

    it("should execute is expression basic functionality") {
      // Test that is expressions execute properly and return correct boolean values
      assertExecValueBool("12 is int", true)
      assertExecValueBool("12 is bool", false)
      assertExecValueBool("true is bool", true)
      assertExecValueBool("true is int", false)
      assertExecValueBool("\"hello\" is string", true)
      assertExecValueBool("\"hello\" is int", false)

      // Test with variables
      assertExecValueIntWithSetup("val x = 12", "if (x is int) 1 else 0", 1)
      assertExecValueIntWithSetup("val x = 12", "if (x is bool) 1 else 0", 0)
      assertExecValueIntWithSetup("val y = true", "if (y is bool) 1 else 0", 1)
      assertExecValueIntWithSetup(
        "val y = true",
        "if (y is string) 1 else 0",
        0
      )
    }

    it("should execute cast expressions - identity casts") {
      // Test identity casts (casting to the same type)
      assertExecValueInt("12 as int", 12)
      assertExecValueBool("true as bool", true)
      assertExecValueBool("false as bool", false)
      assertExecValueString("\"hello\" as string", "hello")

      // Test with variables
      assertExecValueIntWithSetup("val x = 42", "x as int", 42)
      assertExecValueBoolWithSetup("val flag = true", "flag as bool", true)
      assertExecValueStringWithSetup(
        "val text = \"world\"",
        "text as string",
        "world"
      )
    }

    it("should execute cast expressions - valid conversions") {
      // Test casts that should succeed at runtime
      // Skip char literals for now since emitCharacterLiteral is not implemented
      assertExecValueString("42 as string", "42") // int to string
      assertExecValueString("true as string", "true") // bool to string
      assertExecValueString("false as string", "false") // bool to string

      // Test with expressions
      assertExecValueInt("(1 + 2) as int", 3)
      assertExecValueString("(5 * 6) as string", "30")
      assertExecValueBool("(10 > 5) as bool", true)
    }

    //    it should "execute cast expressions - cast to any type" in {
    //      // Test casting various types to 'any' type
    //      // Note: The actual value should remain the same, just the type changes
    //      assertExecValueInt("42 as any as int", 42)
    //      assertExecValueBool("true as any as bool", true)
    //      assertExecValueString("\"test\" as any as string", "test")
    //
    //      // Test with variables
    //      assertExecValueIntWithSetup("val x = 100", "x as any as int", 100)
    //      assertExecValueBoolWithSetup("val flag = false", "flag as any as bool", false)
    //    }
    //
    //    it should "execute cast expressions - cast from any type" in {
    //      // Test casting from 'any' type to specific types
    //      val anySetup = "val obj: any = 42"
    //      assertExecValueIntWithSetup(anySetup, "obj as int", 42)
    //
    //      val anyBoolSetup = "val obj: any = true"
    //      assertExecValueBoolWithSetup(anyBoolSetup, "obj as bool", true)
    //
    //      val anyStringSetup = "val obj: any = \"hello\""
    //      assertExecValueStringWithSetup(anyStringSetup, "obj as string", "hello")
    //    }
    //
    //    it should "execute cast expressions - chained casts" in {
    //      // Test multiple casts in sequence
    //      assertExecValueInt("42 as any as int", 42)
    //      assertExecValueString("42 as string as any as string", "42")
    //      assertExecValueBool("true as any as bool as any as bool", true)
    //
    //      // Test with expressions in between
    //      assertExecValueInt("(20 + 22) as any as int", 42)
    //      assertExecValueString("(\"hel\" + \"lo\") as any as string", "hello")
    //    }

    it("should execute cast expressions - cast with operators") {
      // Test that cast has correct precedence with other operators
      assertExecValueBool("42 as int == 42", true)
      assertExecValueBool("42 as string == \"42\"", true)
      assertExecValueBool("true as bool && false", false)
      assertExecValueBool("false as bool || true", true)

      // Test cast in arithmetic expressions
      assertExecValueInt("(40 + 2) as int", 42)
      assertExecValueInt("40 + 2 as int", 42) // Should be: 40 + (2 as int)
    }

    it("should execute cast expressions - cast in control flow") {
      // Test cast expressions in if conditions
      assertExecValueInt("if (42 as int == 42) 1 else 0", 1)
      assertExecValueInt("if (true as bool) 100 else 200", 100)
      assertExecValueInt("if (false as bool) 100 else 200", 200)

      // Test cast in if branches
      assertExecValueInt("if (true) 42 as int else 0", 42)
      assertExecValueString("if (false) \"no\" else \"yes\" as string", "yes")

      // Test with variables
      assertExecValueIntWithSetup(
        "val x = 50",
        "if (x as int > 40) 1 else 0",
        1
      )
    }

    it("should execute array indexing - basic access") {
      // Test basic array indexing with literal indices
      val setup = "val array = new Array[int](3)"
      // Note: These tests assume array elements are initialized to 0
      // If that's not the case, we may need to modify the setup
      assertExecValueIntWithSetup(setup, "array(0)", 0)
      assertExecValueIntWithSetup(setup, "array(1)", 0)
      assertExecValueIntWithSetup(setup, "array(2)", 0)
    }

    it("should execute array indexing - with assignment") {
      // Test the specific "NewExpression in bindLHS" fix
      val setup = "var array = new Array[int](5)"
      // First test assignment returns unit
      assertExecValueIntWithSetup(
        setup + "\narray(0) = 42",
        "0",
        0
      ) // dummy assertion to ensure compilation

      // Test assignment followed by access
      val assignAndRead = setup + "\narray(0) = 42\narray(1) = 13"
      assertExecValueIntWithSetup(assignAndRead, "array(0)", 42)
      assertExecValueIntWithSetup(assignAndRead, "array(1)", 13)
    }

    it("should execute array indexing - computed indices") {
      val setup = "var array = new Array[int](10)\nval i = 5\narray(i) = 99"
      assertExecValueIntWithSetup(setup, "array(i)", 99)
      assertExecValueIntWithSetup(setup, "array(5)", 99)

      // Test with arithmetic expressions as indices
      val complexSetup = setup + "\narray(2 * 3) = 77\narray(1 + 4) = 55"
      assertExecValueIntWithSetup(complexSetup, "array(6)", 77)
      assertExecValueIntWithSetup(
        complexSetup,
        "array(5)",
        55
      ) // This overwrites the previous value at index 5
    }

    it("should execute array indexing - in expressions") {
      val setup =
        "var array = new Array[int](5)\narray(0) = 10\narray(1) = 20\narray(2) = 30"

      // Test array indexing in arithmetic expressions
      assertExecValueIntWithSetup(setup, "array(0) + array(1)", 30)
      assertExecValueIntWithSetup(setup, "array(2) - array(0)", 20)
      assertExecValueIntWithSetup(setup, "array(1) * 2", 40)

      // Test array indexing in boolean expressions
      assertExecValueBoolWithSetup(setup, "array(0) == 10", true)
      assertExecValueBoolWithSetup(setup, "array(1) > array(0)", true)
      assertExecValueBoolWithSetup(setup, "array(2) < array(1)", false)
    }

    it("should execute array indexing - different types") {
      // Test array indexing with different element types
      val boolSetup =
        "var boolArray = new Array[bool](3)\nboolArray(0) = true\nboolArray(1) = false"
      assertExecValueBoolWithSetup(boolSetup, "boolArray(0)", true)
      assertExecValueBoolWithSetup(boolSetup, "boolArray(1)", false)

      val stringSetup =
        "var stringArray = new Array[string](2)\nstringArray(0) = \"hello\"\nstringArray(1) = \"world\""
      assertExecValueStringWithSetup(stringSetup, "stringArray(0)", "hello")
      assertExecValueStringWithSetup(stringSetup, "stringArray(1)", "world")
    }

    it("should execute string conversions - basic literals") {
      // Test string conversion of basic literals
      assertExecValueString("string(42)", "42")
      assertExecValueString("string(0)", "0")
      assertExecValueString("string(-123)", "-123")
      assertExecValueString("string(true)", "true")
      assertExecValueString("string(false)", "false")

      // String conversion of string (identity)
      assertExecValueString("string(\"hello\")", "hello")
      assertExecValueString("string(\"\")", "")
    }

    it("should execute string conversions - with variables") {
      // Test string conversion of variables
      assertExecValueStringWithSetup("val num = 123", "string(num)", "123")
      assertExecValueStringWithSetup("val flag = true", "string(flag)", "true")
      assertExecValueStringWithSetup(
        "val flag2 = false",
        "string(flag2)",
        "false"
      )
      assertExecValueStringWithSetup(
        "val text = \"world\"",
        "string(text)",
        "world"
      )

      // Negative numbers
      assertExecValueStringWithSetup("val neg = -42", "string(neg)", "-42")
    }

    it("should execute string conversions - with expressions") {
      // Test string conversion of arithmetic expressions
      assertExecValueString("string(1 + 2)", "3")
      assertExecValueString("string(10 - 5)", "5")
      assertExecValueString("string(3 * 4)", "12")
      assertExecValueString("string(15 / 3)", "5")
      assertExecValueString("string(17 % 5)", "2")

      // Test string conversion of boolean expressions
      assertExecValueString("string(true && false)", "false")
      assertExecValueString("string(true || false)", "true")
      assertExecValueString("string(!true)", "false")
      assertExecValueString("string(!false)", "true")
      assertExecValueString("string(5 > 3)", "true")
      assertExecValueString("string(5 == 5)", "true")
      assertExecValueString("string(5 != 3)", "true")
      assertExecValueString("string(5 < 3)", "false")

      // Test string conversion of unary expressions
      assertExecValueString("string(-42)", "-42")
      assertExecValueString("string(+42)", "42")
    }

    it("should execute string conversions - complex expressions") {
      val setup = "val x = 42\nval y = true"

      // String conversions in comparisons
      assertExecValueBoolWithSetup(setup, "string(x) == \"42\"", true)
      assertExecValueBoolWithSetup(setup, "string(y) == \"true\"", true)
      assertExecValueBoolWithSetup(setup, "string(x) != \"0\"", true)
      assertExecValueBoolWithSetup(setup, "string(y) != \"false\"", true)

      // String conversions of computed values
      assertExecValueStringWithSetup(setup, "string(x + 10)", "52")
      assertExecValueStringWithSetup(setup, "string(x * 2)", "84")
      assertExecValueStringWithSetup(setup, "string(x - 10)", "32")
    }

    it("should execute string conversions - with control flow") {
      // String conversions with if expressions
      assertExecValueString("string(if (true) 1 else 2)", "1")
      assertExecValueString("string(if (false) 1 else 2)", "2")
      assertExecValueString("string(if (5 > 3) 100 else 200)", "100")
      assertExecValueString("string(if (3 > 5) true else false)", "false")

      // String conversions with block expressions
      assertExecValueString("string({ 42 })", "42")
      assertExecValueString("string({ true })", "true")
      assertExecValueString(
        "string({ val temp = 10\n temp * 2 })",
        "20"
      )
    }

    it("should execute string conversions - nested and chained") {
      val setup = "val x = 42"

      // Nested string conversions
      assertExecValueStringWithSetup(setup, "string(string(x))", "42")
      assertExecValueString("string(string(\"hello\"))", "hello")

      // String conversions in nested expressions
      assertExecValueStringWithSetup(
        setup,
        "string(string(x) == \"42\")",
        "true"
      )
      assertExecValueStringWithSetup(
        setup,
        "string(string(x + 8) == \"50\")",
        "true"
      )
    }

    it("should execute string conversions - edge cases") {
      // Large numbers
      assertExecValueString("string(999999)", "999999")
      assertExecValueString("string(-999999)", "-999999")

      // Complex arithmetic
      assertExecValueString("string((10 + 5) * (20 / 4))", "75")
      assertExecValueString("string(100 - 50 + 25)", "75")

      // Complex boolean logic
      assertExecValueString("string((5 > 3) && (10 < 20))", "true")
      assertExecValueString("string((5 < 3) || (10 > 20))", "false")
    }

    it("should execute int conversions - basic functionality") {
      // Test int conversion of basic literals
      assertExecValueInt("int(42)", 42)
      assertExecValueInt("int(0)", 0)
      assertExecValueInt("int(-123)", -123)
      assertExecValueInt("int(true)", 1)
      assertExecValueInt("int(false)", 0)

      // Int conversion with variables
      assertExecValueIntWithSetup("val flag = true", "int(flag)", 1)
      assertExecValueIntWithSetup("val flag2 = false", "int(flag2)", 0)
      assertExecValueIntWithSetup("val num = 123", "int(num)", 123)
    }

    it("should execute int conversions - string cases") {
      assertExecValueInt("int(\"42\")", 42)
      assertExecValueInt("int(\"0\")", 0)
      assertExecValueInt("int(\"-123\")", -123)
      assertExecValueIntWithSetup("val str = \"56\"", "int(str)", 56)
      assertExecValueIntWithSetup("val str = \"-78\"", "int(str)", -78)
      // Edge cases
      assertExecValueInt("int(\"00123\")", 123)
      assertExecValueInt("int(\"-00123\")", -123)
      assertExecValueInt("int(\"2147483647\")", 2147483647) // Max int
      assertExecValueInt("int(\"-2147483648\")", -2147483648) // Min int
    }

    it("should execute bool conversions - basic functionality") {
      // Test bool conversion of basic literals
      assertExecValueBool("bool(true)", true)
      assertExecValueBool("bool(false)", false)
      assertExecValueBool("bool(42)", true)
      assertExecValueBool("bool(0)", false)
      assertExecValueBool("bool(-5)", true)

      // Bool conversion with variables
      assertExecValueBoolWithSetup("val num = 123", "bool(num)", true)
      assertExecValueBoolWithSetup("val zero = 0", "bool(zero)", false)
      assertExecValueBoolWithSetup("val flag = true", "bool(flag)", true)
    }

    it("should execute conversion edge cases") {
      // Test conversion of expressions
      assertExecValueString("string(1 + 2 * 3)", "7")
      assertExecValueInt("int(5 > 3)", 1)
      assertExecValueInt("int(3 > 5)", 0)
      assertExecValueBool("bool(10 - 10)", false)
      assertExecValueBool("bool(10 - 9)", true)

      // Test nested conversions
      assertExecValueString("string(int(true))", "1")
      assertExecValueString("string(int(false))", "0")
      assertExecValueInt("int(bool(42))", 1)
      assertExecValueInt("int(bool(0))", 0)
    }

    it("should execute conversions with complex expressions") {
      val setup = "val x = 42\nval y = true"

      // Complex conversions
      assertExecValueStringWithSetup(setup, "string(int(y) + x)", "43")
      assertExecValueIntWithSetup(setup, "int(string(x) == \"42\")", 1)
      assertExecValueBoolWithSetup(setup, "bool(int(y) * x)", true)

      // Conversions in control flow
      assertExecValueStringWithSetup(
        setup,
        "string(if (bool(x)) int(y) else 0)",
        "1"
      )
    }
  }
}
