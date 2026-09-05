import TestHelpers.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class BinderTests extends AnyFunSpec with Matchers {

  describe("Binder") {
    it("should create builtin symbols") {
      val comp = mkCompilation("")
      val symbols = enumSymbols(comp)
      assertSymbol(symbols, SymbolKind.Class, "any")
      assertSymbol(symbols, SymbolKind.Class, "int")
      assertConversionMethod(symbols)
      assertSymbol(symbols, SymbolKind.Class, "string")
      assertSymbol(symbols, SymbolKind.Field, "length")
      assertConversionMethod(symbols)
      assertSymbol(symbols, SymbolKind.Class, "bool")
      assertConversionMethod(symbols)
      assertSymbol(symbols, SymbolKind.Class, "char")
      assertConversionMethod(symbols)
      assertSymbol(symbols, SymbolKind.Class, "unit")
      assertSymbol(symbols, SymbolKind.Class, "Array")
      assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Invariant), "T")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertSymbol(symbols, SymbolKind.Parameter, "size")
      assertSymbol(symbols, SymbolKind.Field, "length")
      assertSymbol(symbols, SymbolKind.Method, "apply")
      assertSymbol(symbols, SymbolKind.Parameter, "index")
      //    assertSymbol(symbols, SymbolKind.Object, "predef")
      assertSymbol(symbols, SymbolKind.Method, "println")
      assertSymbol(symbols, SymbolKind.Parameter, "message")
      assertSymbol(symbols, SymbolKind.Method, "print")
      assertSymbol(symbols, SymbolKind.Parameter, "message")
      assertSymbol(symbols, SymbolKind.Method, "panic")
      assertSymbol(symbols, SymbolKind.Parameter, "message")
      assertSymbol(symbols, SymbolKind.Method, "exit")
      assertSymbol(symbols, SymbolKind.Parameter, "code")
      assertSymbol(symbols, SymbolKind.Method, "assert")
      assertSymbol(symbols, SymbolKind.Parameter, "condition")
      assertSymbol(symbols, SymbolKind.Parameter, "message")
      assertSymbol(symbols, SymbolKind.Method, "mod")
      assertSymbol(symbols, SymbolKind.Parameter, "a")
      assertSymbol(symbols, SymbolKind.Parameter, "b")
      assertSymbol(symbols, SymbolKind.Object, "$Program")
      assertSymbol(symbols, SymbolKind.Method, "$runtimeInit")
      assertSymbol(symbols, SymbolKind.Method, "main")
      assertNoSymbols(symbols)
    }

    it("should bind top level fields") {
      val comp = mkCompilation("val x = 12")
      val symbols = enumNonBuiltinSymbols(comp)
      assertProgramSymbol(symbols)
      assertSymbol(symbols, SymbolKind.Field, "x")
      assertSymbol(symbols, SymbolKind.Method, "main")
      assertNoSymbols(symbols)
    }

    it("should bind methods") {
      val comp = mkCompilation("def foo() = 12")
      val symbols = enumNonBuiltinSymbols(comp)

      assertProgramSymbol(symbols)
      assertSymbol(symbols, SymbolKind.Method, "foo")
      assertMainSymbol(symbols)
      assertNoSymbols(symbols)
    }

    it("should bind classes without args") {
      val comp = mkCompilation("class Foo()")
      val symbols = enumNonBuiltinSymbols(comp)
      assertSymbol(symbols, SymbolKind.Class, "Foo")
      assertSymbol(symbols, SymbolKind.This, "this")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertProgramSymbol(symbols)
      assertMainSymbol(symbols)
      assertNoSymbols(symbols)
    }

    it("should bind classes with args") {
      val comp = mkCompilation("class Foo(x: int, y: int)")
      val symbols = enumNonBuiltinSymbols(comp)
      assertSymbol(symbols, SymbolKind.Class, "Foo")
      assertSymbol(symbols, SymbolKind.This, "this")
      assertSymbol(symbols, SymbolKind.Field, "y")
      assertSymbol(symbols, SymbolKind.Field, "x")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertSymbol(symbols, SymbolKind.Parameter, "x")
      assertSymbol(symbols, SymbolKind.Parameter, "y")

      assertProgramSymbol(symbols)
      assertMainSymbol(symbols)

      assertNoSymbols(symbols)
    }

    it("should bind class fields") {
      val comp = mkCompilation(
        "class Foo() {\n" +
          "  var z = 0\n" +
          "}"
      )

      val symbols = enumNonBuiltinSymbols(comp)
      assertSymbol(symbols, SymbolKind.Class, "Foo")
      assertSymbol(symbols, SymbolKind.This, "this")
      assertSymbol(symbols, SymbolKind.Field, "z")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
    }

    it("should bind enums without args") {
      val comp = mkCompilation(
        "enum Foo {\n" +
          "  case Bar\n" +
          "  case Baz\n" +
          "}"
      )
      val symbols = enumNonBuiltinSymbols(comp)
      assertSymbol(symbols, SymbolKind.Alias, "Foo")
      assertSymbol(symbols, SymbolKind.Class, "Bar")
      assertSymbol(symbols, SymbolKind.Class, "Baz")
      assertSymbol(symbols, SymbolKind.This, "this")

      assertProgramSymbol(symbols)
      assertMainSymbol(symbols)

      assertNoSymbols(symbols)
    }

    it("should bind enums with args") {
      val comp = mkCompilation(
        "enum Foo {\n" +
          "  case Bar(x: int)\n" +
          "  case Baz(y: int)\n" +
          "}"
      )
      val symbols = enumNonBuiltinSymbols(comp)
      assertSymbol(symbols, SymbolKind.Alias, "Foo")
      assertSymbol(symbols, SymbolKind.Class, "Bar")
      assertSymbol(symbols, SymbolKind.Field, "x")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertSymbol(symbols, SymbolKind.Parameter, "x")

      assertSymbol(symbols, SymbolKind.Class, "Baz")
      assertSymbol(symbols, SymbolKind.Field, "y")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertSymbol(symbols, SymbolKind.Parameter, "y")
      assertSymbol(symbols, SymbolKind.This, "this")

      assertProgramSymbol(symbols)
      assertMainSymbol(symbols)

      assertNoSymbols(symbols)
    }

    it("should create locals for extract patterns with variables") {
      val comp = mkCompilation(
        "enum Option[T] {\n" +
          "  case Some(value: T)\n" +
          "  case None\n" +
          "}\n" +
          "val opt = Option.Some(42)\n" +
          "val result = opt match {\n" +
          "  case Option.Some(value) => value\n" +
          "  case Option.None => 0\n" +
          "}"
      )
      val symbols = enumNonBuiltinSymbols(comp)
      assertSymbol(symbols, SymbolKind.Alias, "Option")
      assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Invariant), "T")
      assertSymbol(symbols, SymbolKind.Class, "Some")
      assertSymbol(symbols, SymbolKind.Field, "value")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertSymbol(symbols, SymbolKind.Parameter, "value")

      assertSymbol(symbols, SymbolKind.Class, "None")
      assertSymbol(symbols, SymbolKind.This, "this")

      assertProgramSymbol(symbols)
      assertSymbol(symbols, SymbolKind.Field, "result")
      assertSymbol(symbols, SymbolKind.Field, "opt")
      assertMainSymbol(symbols)

      // Look for pattern variables among remaining symbols
      var foundPatternVariable = false
      while (symbols.moveNext()) {
        val symbol = symbols.current()
        if (symbol.kind == SymbolKind.Local && symbol.name == "value") {
          foundPatternVariable = true
        }
      }
      foundPatternVariable shouldBe true
    }

    it("should reject reassigning a val") {
      val comp = mkFailingCompilation("val x = 10\nx = 20")
      diagnosticMessages(comp) shouldEqual Seq("reassignment to val x")
    }

    it("should allow reassigning a var") {
      mkCompilation("var y = 10\ny = 20")
    }

    it("should not treat a val initializer as a reassignment") {
      // Field initializers are rewritten into assignments for $runtimeInit,
      // so the declaration must not report against itself.
      mkCompilation("val x = 10\nval y = x")
    }

    it("should reject reassigning a local val") {
      val comp =
        mkFailingCompilation("def f() = {\n  val a = 1\n  a = 2\n  a\n}")
      diagnosticMessages(comp) shouldEqual Seq("reassignment to val a")
    }

    it("should allow reassigning a local var") {
      mkCompilation("def f() = {\n  var a = 1\n  a = 2\n  a\n}")
    }

    it("should reject assigning to a val field through member access") {
      val comp = mkFailingCompilation(
        "class Box(v: int) {\n  val fixed = 1\n}\nval b = new Box(3)\nb.fixed = 5"
      )
      diagnosticMessages(comp) shouldEqual Seq("reassignment to val fixed")
    }

    // The positive case - assigning to a `var` field through member access -
    // cannot be covered end to end yet: the binder accepts it and the lowerer
    // then hits the unimplemented panic in Lowered.scala lowerAssignment. The val case below
    // works because reporting a diagnostic stops the pipeline before lowering.

    it("should reject assigning to a builtin read-only field") {
      val comp =
        mkFailingCompilation("val arr = new Array[int](3)\narr.length = 9")
      diagnosticMessages(comp) shouldEqual Seq("reassignment to val length")
    }

    // A failure has to come back as a diagnostic, never as an exception.
    // Each of these used to take the compiler down instead of reporting.
    it("should report a diagnostic for an unsupported operator") {
      val comp = mkFailingCompilation("val n = 5\nval s = \"text \" + n")
      diagnosticMessages(comp) should contain(
        "No operator '+' for operands string and int"
      )
    }

    it("should report a diagnostic for break") {
      val comp = mkFailingCompilation("while (true) {\n  break\n}")
      diagnosticMessages(comp) should contain("break is not supported")
    }

    it("should report a diagnostic for continue") {
      val comp = mkFailingCompilation("while (true) {\n  continue\n}")
      diagnosticMessages(comp) should contain("continue is not supported")
    }

    // Equality on reference types. The operator table only covers the value
    // types, so everything a program declares has to come from the conversion
    // between the two operands.
    it("should compare an enum against one of its own cases") {
      mkCompilation(
        "enum Color {\n" +
          "  case Red\n" +
          "  case Green\n" +
          "}\n" +
          "def isRed(c: Color): bool = c == Color.Red"
      )
    }

    it("should compare a generic enum against a case with no arguments") {
      mkCompilation(
        "enum Option[T] {\n" +
          "  case Some(value: T)\n" +
          "  case None\n" +
          "}\n" +
          "def isEmpty(o: Option[int]): bool = o == Option.None"
      )
    }

    it("should compare two values of the same class type") {
      mkCompilation(
        "class Box(v: int)\n" +
          "def same(a: Box, b: Box): bool = a != b"
      )
    }

    it("should reject comparing unrelated class types") {
      val comp = mkFailingCompilation(
        "class Box(v: int)\n" +
          "class Bag(v: int)\n" +
          "def same(a: Box, b: Bag): bool = a == b"
      )
      diagnosticMessages(comp) should contain(
        "No operator '==' for operands Box and Bag"
      )
    }

    // The value types keep the table's answer: what it rejects there is a real
    // mismatch, not a missing entry.
    it("should reject comparing a string against a char") {
      val comp =
        mkFailingCompilation("val s = \"a\"\nval c = 'a'\nval eq = s == c")
      diagnosticMessages(comp) should contain(
        "No operator '==' for operands string and char"
      )
    }

    it("should bind out as covariant and in as contravariant") {
      val comp = mkCompilation(
        "class Producer[out T]()\n" +
          "class Consumer[in T]()\n" +
          "class Fixed[T]()"
      )
      val symbols = enumNonBuiltinSymbols(comp)
      assertSymbol(symbols, SymbolKind.Class, "Producer")
      assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Covariant), "T")
      assertSymbol(symbols, SymbolKind.This, "this")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertSymbol(symbols, SymbolKind.Class, "Consumer")
      assertSymbol(
        symbols,
        SymbolKind.TypeParameter(Variance.Contravariant),
        "T"
      )
      assertSymbol(symbols, SymbolKind.This, "this")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertSymbol(symbols, SymbolKind.Class, "Fixed")
      assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Invariant), "T")
      assertSymbol(symbols, SymbolKind.This, "this")
    }

    it("should not create named locals for discard patterns") {
      val comp = mkCompilation(
        "enum Option[T] {\n" +
          "  case Some(value: T)\n" +
          "  case None\n" +
          "}\n" +
          "val opt = Option.Some(42)\n" +
          "val result = opt match {\n" +
          "  case Option.Some(_) => 1\n" +
          "  case Option.None => 0\n" +
          "}"
      )
      val symbols = enumNonBuiltinSymbols(comp)
      assertSymbol(symbols, SymbolKind.Alias, "Option")
      assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Invariant), "T")
      assertSymbol(symbols, SymbolKind.Class, "Some")
      assertSymbol(symbols, SymbolKind.Field, "value")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertSymbol(symbols, SymbolKind.Parameter, "value")

      assertSymbol(symbols, SymbolKind.Class, "None")
      assertSymbol(symbols, SymbolKind.This, "this")

      assertProgramSymbol(symbols)
      assertSymbol(symbols, SymbolKind.Field, "result")
      assertSymbol(symbols, SymbolKind.Field, "opt")
      assertMainSymbol(symbols)

      // Ensure no local symbol named "_" was created (generated temporaries like $1, $2 are ok)
      while (symbols.moveNext()) {
        val symbol = symbols.current()
        if (symbol.kind == SymbolKind.Local && symbol.name == "_") {
          symbol.name should not be "_"
        }
      }
    }

    it("should handle mixed extract and discard patterns") {
      val comp = mkCompilation(
        "enum Option[T] {\n" +
          "  case Some(value: T)\n" +
          "  case None\n" +
          "}\n" +
          "val opt = Option.Some(42)\n" +
          "val result = opt match {\n" +
          "  case Option.Some(x) => x\n" +
          "  case Option.None => 0\n" +
          "  case _ => -1\n" +
          "}"
      )
      val symbols = enumNonBuiltinSymbols(comp)
      assertSymbol(symbols, SymbolKind.Alias, "Option")
      assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Invariant), "T")
      assertSymbol(symbols, SymbolKind.Class, "Some")
      assertSymbol(symbols, SymbolKind.Field, "value")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertSymbol(symbols, SymbolKind.Parameter, "value")

      assertSymbol(symbols, SymbolKind.Class, "None")
      assertSymbol(symbols, SymbolKind.This, "this")

      assertProgramSymbol(symbols)
      assertSymbol(symbols, SymbolKind.Field, "result")
      assertSymbol(symbols, SymbolKind.Field, "opt")
      assertMainSymbol(symbols)

      // Look for the pattern variable 'x' but not for any '_' locals
      var foundPatternVariable = false
      while (symbols.moveNext()) {
        val symbol = symbols.current()
        if (symbol.kind == SymbolKind.Local && symbol.name == "x") {
          foundPatternVariable = true
        }
        if (symbol.kind == SymbolKind.Local && symbol.name == "_") {
          symbol.name should not be "_"
        }
      }
      foundPatternVariable shouldBe true
    }

    /** A trait gets neither `this` nor `.ctor`, unlike the class tests above.
      * That absence is the point: there is nothing to construct and no
      * receiver, because evidence for a trait comes from a `given`.
      */
    it("should bind traits") {
      val comp = mkCompilation("trait Show { def show(): string }")
      val symbols = enumNonBuiltinSymbols(comp)

      assertSymbol(symbols, SymbolKind.Trait, "Show")
      assertSymbol(symbols, SymbolKind.Method, "show")

      assertProgramSymbol(symbols)
      assertMainSymbol(symbols)
      assertNoSymbols(symbols)
    }

    it("should bind generic traits") {
      val comp = mkCompilation("trait Eq[T] { def equals(a: T, b: T): bool }")
      val symbols = enumNonBuiltinSymbols(comp)

      val eq = assertSymbol(symbols, SymbolKind.Trait, "Eq")
      assertSymbolType(comp, eq, "Eq<T>")

      assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Invariant), "T")
      assertSymbol(symbols, SymbolKind.Method, "equals")
      assertSymbol(symbols, SymbolKind.Parameter, "a")
      assertSymbol(symbols, SymbolKind.Parameter, "b")

      assertProgramSymbol(symbols)
      assertMainSymbol(symbols)
      assertNoSymbols(symbols)
    }

    it("should reject instantiating a trait") {
      val comp = mkFailingCompilation(
        "trait Show { def show(): string }\nval x = new Show()"
      )
      diagnosticMessages(comp) should contain(
        "Trait Show cannot be instantiated"
      )
    }

    it("should reject instantiating a generic trait") {
      val comp = mkFailingCompilation(
        "trait Eq[T] { def equals(a: T, b: T): bool }\nval x = new Eq[int]()"
      )
      diagnosticMessages(comp) should contain(
        "Trait Eq cannot be instantiated"
      )
    }

    /** A context bound is stored applied to the type variable it constrains, so
      * `[K: Eq]` becomes `Eq<$0>`. That is what makes `Types.substitute` turn
      * it into the resolution goal once `K` is known.
      */
    it("should bind a context bound on a function") {
      val comp = mkCompilation(
        "trait Eq[T] { def equals(a: T, b: T): bool }\n" +
          "def same[K: Eq](a: K, b: K): bool = true"
      )
      val program = assertSome(comp.root.lookup("$Program"))
      val same = assertSome(program.lookup("same"))

      assertSymbolType(comp, same, "<K>(a: $0, b: $0) -> bool where Eq<$0>")
    }

    /** A constrained class carries its constraint on `.ctor`, not on the class
      * type: the constructor is elaborated like any other method and the `new`
      * site is where the type arguments are concrete (ADR 0005, decision B).
      */
    it("should bind a context bound on a class constructor") {
      val comp = mkCompilation(
        "trait Eq[T] { def equals(a: T, b: T): bool }\n" +
          "class Box[K: Eq](value: K)"
      )
      val box = assertSome(comp.root.lookup("Box"))
      assertSymbolType(comp, box, "Box<K>")

      val ctor = assertSome(box.lookup(".ctor"))
      assertSymbolType(comp, ctor, "<K>(value: $0) -> unit where Eq<$0>")
    }

    it("should leave unconstrained parameters alone") {
      val comp = mkCompilation(
        "trait Eq[T] { def equals(a: T, b: T): bool }\n" +
          "def f[K: Eq, V](a: K, b: V): bool = true"
      )
      val program = assertSome(comp.root.lookup("$Program"))
      val f = assertSome(program.lookup("f"))

      // one constraint, on $0, with nothing recorded for V
      assertSymbolType(comp, f, "<K, V>(a: $0, b: $1) -> bool where Eq<$0>")
    }

    it("should reject a context bound that is not a trait") {
      val comp = mkFailingCompilation(
        "class Foo()\ndef f[K: Foo](a: K): bool = true"
      )
      diagnosticMessages(comp) should contain(
        "Foo is not a trait and cannot be a context bound"
      )
    }

    it("should reject a context bound whose trait takes no type parameter") {
      val comp = mkFailingCompilation(
        "trait Show { def show(): string }\ndef f[K: Show](a: K): bool = true"
      )
      diagnosticMessages(comp) should contain(
        "Trait Show takes 0 type parameters; a context bound requires exactly 1"
      )
    }

    it("should reject a context bound naming an unknown type") {
      val comp = mkFailingCompilation("def f[K: Nope](a: K): bool = true")
      diagnosticMessages(comp) should contain("Type Nope not defined")
    }

    it("should register a given globally") {
      val comp = mkCompilation(
        "trait Eq[T] { def equals(a: T, b: T): bool }\n" +
          "given Eq[int] { def equals(a: int, b: int): bool = a == b }\n" +
          "given Eq[string] { def equals(a: string, b: string): bool = a == b }"
      )
      givenHeads(comp) shouldBe Seq("Eq<int>", "Eq<string>")
    }

    /** A conditional given records its head applied to its own type variable,
      * so `given [T: Ord] => Ord[Box[T]]` registers as `Ord<Box<$0>>`.
      */
    it("should register a conditional given") {
      val comp = mkCompilation(
        "trait Ord[T] { def compare(a: T, b: T): int }\n" +
          "class Box[T](value: T)\n" +
          "given [T: Ord] => Ord[Box[T]] {\n" +
          "  def compare(a: Box[T], b: Box[T]): int = 0\n" +
          "}"
      )
      givenHeads(comp) shouldBe Seq("Ord<Box<$0>>")
    }

    it("should reject a second given for the same pair") {
      val comp = mkFailingCompilation(
        "trait Eq[T] { def equals(a: T, b: T): bool }\n" +
          "given Eq[int] { def equals(a: int, b: int): bool = a == b }\n" +
          "given Eq[int] { def equals(a: int, b: int): bool = false }"
      )
      diagnosticMessages(comp) should contain(
        "Given for Eq<int> overlaps the one at :2:8"
      )
      // only the first survives
      givenHeads(comp) shouldBe Seq("Eq<int>")
    }

    /** Overlap is unification, not equality: `Ord[Box[T]]` and `Ord[Box[int]]`
      * are two givens for one pair as soon as `T` can be `int`. Comparing type
      * arguments structurally would let this pair through.
      */
    it("should reject a concrete given overlapping a conditional one") {
      val comp = mkFailingCompilation(
        "trait Ord[T] { def compare(a: T, b: T): int }\n" +
          "class Box[T](value: T)\n" +
          "given [T: Ord] => Ord[Box[T]] {\n" +
          "  def compare(a: Box[T], b: Box[T]): int = 0\n" +
          "}\n" +
          "given Ord[Box[int]] {\n" +
          "  def compare(a: Box[int], b: Box[int]): int = 0\n" +
          "}"
      )
      diagnosticMessages(comp) should contain(
        "Given for Ord<Box<int>> overlaps the one at :3:20"
      )
    }

    it("should allow givens whose heads cannot unify") {
      val comp = mkCompilation(
        "trait Ord[T] { def compare(a: T, b: T): int }\n" +
          "class Box[T](value: T)\n" +
          "class Bag[T](value: T)\n" +
          "given [T: Ord] => Ord[Box[T]] {\n" +
          "  def compare(a: Box[T], b: Box[T]): int = 0\n" +
          "}\n" +
          "given Ord[Bag[int]] {\n" +
          "  def compare(a: Bag[int], b: Bag[int]): int = 0\n" +
          "}"
      )
      givenHeads(comp) shouldBe Seq("Ord<Box<$0>>", "Ord<Bag<int>>")
    }

    it("should reject a given whose head is not a trait") {
      val comp = mkFailingCompilation("class Foo()\ngiven Foo[int] { }")
      diagnosticMessages(comp) should contain(
        "Foo is not a trait, so it cannot have a given"
      )
      givenHeads(comp) shouldBe Seq.empty
    }

    it("should reject a given whose head has no type arguments") {
      val comp = mkFailingCompilation(
        "trait Eq[T] { def equals(a: T, b: T): bool }\ngiven Eq { }"
      )
      diagnosticMessages(comp) should contain(
        "Given for Eq needs type arguments"
      )
      givenHeads(comp) shouldBe Seq.empty
    }

    val eqTrait = "trait Eq[T] { def equals(a: T, b: T): bool }\n"
    val ordTrait = "trait Ord[T] { def compare(a: T, b: T): int }\n"
    val eqInt = "given Eq[int] { def equals(a: int, b: int): bool = a == b }\n"
    val same = "def same[K: Eq](a: K, b: K): bool = true\n"

    it("should resolve evidence for a constrained call") {
      mkCompilation(eqTrait + eqInt + same + "val r = same(1, 2)")
    }

    it("should report a constrained call with no matching given") {
      val comp =
        mkFailingCompilation(eqTrait + eqInt + same + "val r = same(true, false)")
      diagnosticMessages(comp) should contain("No given instance for Eq<bool>")
    }

    it("should leave unconstrained generics alone") {
      mkCompilation(eqTrait + eqInt + "def id[K](a: K): K = a\nval r = id(true)")
    }

    /** A constrained class resolves at the `new` site, where its type
      * arguments are concrete (ADR 0005, decision B).
      */
    it("should resolve evidence for a constrained constructor") {
      val setup = eqTrait + eqInt + "class Box[T: Eq](value: T)\n"
      mkCompilation(setup + "val b = new Box[int](1)")

      val comp = mkFailingCompilation(setup + "val b = new Box[bool](true)")
      diagnosticMessages(comp) should contain("No given instance for Eq<bool>")
    }

    /** `Ord[Box[int]]` is proved by the conditional given, which then needs
      * `Ord[int]` — the recursive evidence ADR 0005 describes.
      */
    it("should resolve a conditional given recursively") {
      val boxOrd = ordTrait + "class Box[T](value: T)\n" +
        "given [T: Ord] => Ord[Box[T]] {\n" +
        "  def compare(a: Box[T], b: Box[T]): int = 0\n" +
        "}\n" +
        "def srt[K: Ord](a: K): int = 0\n"
      val ordInt = "given Ord[int] { def compare(a: int, b: int): int = 0 }\n"

      mkCompilation(boxOrd + ordInt + "val r = srt(new Box[int](1))")

      // without Ord[int] the premise fails, and the inner goal is what is
      // reported rather than the outer one
      val comp =
        mkFailingCompilation(boxOrd + "val r = srt(new Box[int](1))")
      diagnosticMessages(comp) should contain("No given instance for Ord<int>")
    }

    it("should let an enclosing constraint discharge a call") {
      mkCompilation(
        eqTrait + eqInt + same +
          "def outer[T: Eq](a: T, b: T): bool = same(a, b)"
      )
    }

    /** The goal mentions a type variable, so there is no given to look for —
      * but the enclosing generic declares no bound either, leaving nothing to
      * forward.
      */
    it("should report a constrained call under an unconstrained parameter") {
      val comp = mkFailingCompilation(
        eqTrait + eqInt + same +
          "def outer[T](a: T, b: T): bool = same(a, b)"
      )
      diagnosticMessages(comp) should contain(
        "No evidence for Eq[T]; the enclosing declaration does not require it"
      )
    }

    /** Which declaration is reported as the duplicate follows binding order,
      * not source order, and traits bind before classes — so the class is the
      * one flagged here even though it is written second. That is pre-existing
      * behaviour between classes, objects and enums; traits just join it.
      */
    it("should report a trait colliding with a class") {
      val comp = mkFailingCompilation(
        "trait Show { def show(): string }\nclass Show()"
      )
      diagnosticMessages(comp) should contain(
        "Duplicate definition of Show at :2:8"
      )
    }
  }
}
