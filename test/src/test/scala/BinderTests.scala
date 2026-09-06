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
      // the prelude's traits and givens follow; they have their own test
    }

    /** The prelude's traits and givens (ADR 0004). Asserted against the root's
      * own members rather than the flattened symbol chain, because lowering
      * adds a temporary local inside every `Show` given — `show` is a call —
      * and that is an artifact of lowering, not part of the declaration.
      */
    it("should create the prelude traits and givens") {
      val comp = mkCompilation("")

      memberSignature(assertSome(comp.root.lookup("Eq"))) shouldBe Seq(
        "TypeParameter(Invariant):T",
        "Method:==",
        "Method:!="
      )
      memberSignature(assertSome(comp.root.lookup("Ord"))) shouldBe Seq(
        "TypeParameter(Invariant):T",
        "Method:<",
        "Method:<=",
        "Method:>",
        "Method:>="
      )
      memberSignature(assertSome(comp.root.lookup("Show"))) shouldBe Seq(
        "TypeParameter(Invariant):T",
        "Method:show"
      )

      // no Ord[bool]: the builtin operator table has no `<` on bool
      preludeGivenHeads(comp) shouldBe Seq(
        "Eq<int>",
        "Eq<string>",
        "Eq<bool>",
        "Eq<char>",
        "Ord<int>",
        "Ord<string>",
        "Ord<char>",
        "Show<int>",
        "Show<string>",
        "Show<bool>",
        "Show<char>"
      )

      memberSignature(
        assertSome(comp.root.lookup("$given$Eq$int"))
      ) shouldBe Seq("Method:==", "Method:!=")
    }

    /** Only `Eq` and `Ord` claim tokens. `Show` declares no operator, so
      * `show` stays an ordinary contextual extension (ADR 0004).
      */
    it("should claim the comparison tokens for the prelude traits") {
      val comp = mkCompilation("")
      val eq = assertSome(comp.root.lookup("Eq"))
      val ord = assertSome(comp.root.lookup("Ord"))

      def owner(kind: Int): Symbol =
        assertSome(comp.binder.operatorTraits.get(kind))

      owner(SyntaxKind.EqualsEqualsToken) shouldBe eq
      owner(SyntaxKind.BangEqualsToken) shouldBe eq
      owner(SyntaxKind.LessThanToken) shouldBe ord
      owner(SyntaxKind.LessThanEqualsToken) shouldBe ord
      owner(SyntaxKind.GreaterThanToken) shouldBe ord
      owner(SyntaxKind.GreaterThanEqualsToken) shouldBe ord

      comp.binder.operatorTraits.get(SyntaxKind.PlusToken) shouldBe Option.None
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
      // a case with no parameters still gets a constructor: it is built once,
      // in `$runtimeInit`, and every mention names that one value
      assertSymbol(symbols, SymbolKind.Class, "Bar")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertSymbol(symbols, SymbolKind.Class, "Baz")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
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
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
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
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
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
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
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
      val comp = mkCompilation("trait Display { def show(): string }")
      val symbols = enumNonBuiltinSymbols(comp)

      assertSymbol(symbols, SymbolKind.Trait, "Display")
      assertSymbol(symbols, SymbolKind.Method, "show")

      assertProgramSymbol(symbols)
      assertMainSymbol(symbols)
      assertNoSymbols(symbols)
    }

    it("should bind generic traits") {
      val comp = mkCompilation("trait Eqv[T] { def equals(a: T, b: T): bool }")
      val symbols = enumNonBuiltinSymbols(comp)

      val eq = assertSymbol(symbols, SymbolKind.Trait, "Eqv")
      assertSymbolType(comp, eq, "Eqv<T>")

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
        "trait Display { def show(): string }\nval x = new Display()"
      )
      diagnosticMessages(comp) should contain(
        "Trait Display cannot be instantiated"
      )
    }

    it("should reject instantiating a generic trait") {
      val comp = mkFailingCompilation(
        "trait Eqv[T] { def equals(a: T, b: T): bool }\nval x = new Eqv[int]()"
      )
      diagnosticMessages(comp) should contain(
        "Trait Eqv cannot be instantiated"
      )
    }

    /** A context bound is stored applied to the type variable it constrains, so
      * `[K: Eqv]` becomes `Eqv<$0>`. That is what makes `Types.substitute` turn
      * it into the resolution goal once `K` is known.
      */
    it("should bind a context bound on a function") {
      val comp = mkCompilation(
        "trait Eqv[T] { def equals(a: T, b: T): bool }\n" +
          "def same[K: Eqv](a: K, b: K): bool = true"
      )
      val program = assertSome(comp.root.lookup("$Program"))
      val same = assertSome(program.lookup("same"))

      assertSymbolType(comp, same, "<K>(a: $0, b: $0) -> bool where Eqv<$0>")
    }

    /** A constrained class carries its constraint on `.ctor`, not on the class
      * type: the constructor is elaborated like any other method and the `new`
      * site is where the type arguments are concrete (ADR 0005, decision B).
      */
    it("should bind a context bound on a class constructor") {
      val comp = mkCompilation(
        "trait Eqv[T] { def equals(a: T, b: T): bool }\n" +
          "class Box[K: Eqv](value: K)"
      )
      val box = assertSome(comp.root.lookup("Box"))
      assertSymbolType(comp, box, "Box<K>")

      val ctor = assertSome(box.lookup(".ctor"))
      assertSymbolType(comp, ctor, "<K>(value: $0) -> unit where Eqv<$0>")
    }

    it("should leave unconstrained parameters alone") {
      val comp = mkCompilation(
        "trait Eqv[T] { def equals(a: T, b: T): bool }\n" +
          "def f[K: Eqv, V](a: K, b: V): bool = true"
      )
      val program = assertSome(comp.root.lookup("$Program"))
      val f = assertSome(program.lookup("f"))

      // one constraint, on $0, with nothing recorded for V
      assertSymbolType(comp, f, "<K, V>(a: $0, b: $1) -> bool where Eqv<$0>")
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
        "trait Display { def show(): string }\ndef f[K: Display](a: K): bool = true"
      )
      diagnosticMessages(comp) should contain(
        "Trait Display takes 0 type parameters; a context bound requires exactly 1"
      )
    }

    it("should reject a context bound naming an unknown type") {
      val comp = mkFailingCompilation("def f[K: Nope](a: K): bool = true")
      diagnosticMessages(comp) should contain("Type Nope not defined")
    }

    it("should register a given globally") {
      val comp = mkCompilation(
        "trait Eqv[T] { def equals(a: T, b: T): bool }\n" +
          "given Eqv[int] { def equals(a: int, b: int): bool = a == b }\n" +
          "given Eqv[string] { def equals(a: string, b: string): bool = a == b }"
      )
      givenHeads(comp) shouldBe Seq("Eqv<int>", "Eqv<string>")
    }

    /** A conditional given records its head applied to its own type variable,
      * so `given [T: Ranked] => Ranked[Box[T]]` registers as `Ranked<Box<$0>>`.
      */
    it("should register a conditional given") {
      val comp = mkCompilation(
        "trait Ranked[T] { def compare(a: T, b: T): int }\n" +
          "class Box[T](value: T)\n" +
          "given [T: Ranked] => Ranked[Box[T]] {\n" +
          "  def compare(a: Box[T], b: Box[T]): int = 0\n" +
          "}"
      )
      givenHeads(comp) shouldBe Seq("Ranked<Box<$0>>")
    }

    it("should reject a second given for the same pair") {
      val comp = mkFailingCompilation(
        "trait Eqv[T] { def equals(a: T, b: T): bool }\n" +
          "given Eqv[int] { def equals(a: int, b: int): bool = a == b }\n" +
          "given Eqv[int] { def equals(a: int, b: int): bool = false }"
      )
      diagnosticMessages(comp) should contain(
        "Given for Eqv<int> overlaps the one at :2:8"
      )
      // only the first survives
      givenHeads(comp) shouldBe Seq("Eqv<int>")
    }

    /** Overlap is unification, not equality: `Ranked[Box[T]]` and `Ranked[Box[int]]`
      * are two givens for one pair as soon as `T` can be `int`. Comparing type
      * arguments structurally would let this pair through.
      */
    it("should reject a concrete given overlapping a conditional one") {
      val comp = mkFailingCompilation(
        "trait Ranked[T] { def compare(a: T, b: T): int }\n" +
          "class Box[T](value: T)\n" +
          "given [T: Ranked] => Ranked[Box[T]] {\n" +
          "  def compare(a: Box[T], b: Box[T]): int = 0\n" +
          "}\n" +
          "given Ranked[Box[int]] {\n" +
          "  def compare(a: Box[int], b: Box[int]): int = 0\n" +
          "}"
      )
      diagnosticMessages(comp) should contain(
        "Given for Ranked<Box<int>> overlaps the one at :3:23"
      )
    }

    it("should allow givens whose heads cannot unify") {
      val comp = mkCompilation(
        "trait Ranked[T] { def compare(a: T, b: T): int }\n" +
          "class Box[T](value: T)\n" +
          "class Bag[T](value: T)\n" +
          "given [T: Ranked] => Ranked[Box[T]] {\n" +
          "  def compare(a: Box[T], b: Box[T]): int = 0\n" +
          "}\n" +
          "given Ranked[Bag[int]] {\n" +
          "  def compare(a: Bag[int], b: Bag[int]): int = 0\n" +
          "}"
      )
      givenHeads(comp) shouldBe Seq("Ranked<Box<$0>>", "Ranked<Bag<int>>")
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
        "trait Eqv[T] { def equals(a: T, b: T): bool }\ngiven Eqv { }"
      )
      diagnosticMessages(comp) should contain(
        "Given for Eqv needs type arguments"
      )
      givenHeads(comp) shouldBe Seq.empty
    }

    val eqTrait = "trait Eqv[T] { def equals(a: T, b: T): bool }\n"
    val ordTrait = "trait Ranked[T] { def compare(a: T, b: T): int }\n"
    val eqInt = "given Eqv[int] { def equals(a: int, b: int): bool = a == b }\n"
    val same = "def same[K: Eqv](a: K, b: K): bool = true\n"

    it("should resolve evidence for a constrained call") {
      mkCompilation(eqTrait + eqInt + same + "val r = same(1, 2)")
    }

    it("should report a constrained call with no matching given") {
      val comp =
        mkFailingCompilation(eqTrait + eqInt + same + "val r = same(true, false)")
      diagnosticMessages(comp) should contain("No given instance for Eqv<bool>")
    }

    it("should leave unconstrained generics alone") {
      mkCompilation(eqTrait + eqInt + "def id[K](a: K): K = a\nval r = id(true)")
    }

    /** A constrained class resolves at the `new` site, where its type
      * arguments are concrete (ADR 0005, decision B).
      */
    it("should resolve evidence for a constrained constructor") {
      val setup = eqTrait + eqInt + "class Box[T: Eqv](value: T)\n"
      mkCompilation(setup + "val b = new Box[int](1)")

      val comp = mkFailingCompilation(setup + "val b = new Box[bool](true)")
      diagnosticMessages(comp) should contain("No given instance for Eqv<bool>")
    }

    /** `Ranked[Box[int]]` is proved by the conditional given, which then needs
      * `Ranked[int]` — the recursive evidence ADR 0005 describes.
      */
    it("should resolve a conditional given recursively") {
      val boxOrd = ordTrait + "class Box[T](value: T)\n" +
        "given [T: Ranked] => Ranked[Box[T]] {\n" +
        "  def compare(a: Box[T], b: Box[T]): int = 0\n" +
        "}\n" +
        "def srt[K: Ranked](a: K): int = 0\n"
      val ordInt = "given Ranked[int] { def compare(a: int, b: int): int = 0 }\n"

      mkCompilation(boxOrd + ordInt + "val r = srt(new Box[int](1))")

      // without Ranked[int] the premise fails, and the inner goal is what is
      // reported rather than the outer one
      val comp =
        mkFailingCompilation(boxOrd + "val r = srt(new Box[int](1))")
      diagnosticMessages(comp) should contain("No given instance for Ranked<int>")
    }

    /** A record is per ground goal, not per given (ADR 0006, decision B), so
      * one conditional given used at two instantiations lays out two — each
      * followed by the record its premise needs.
      */
    it("should intern one record per instantiation") {
      val comp = mkCompilation(
        ordTrait + "class Box[T](value: T)\n" +
          "given [T: Ranked] => Ranked[Box[T]] {\n" +
          "  def compare(a: Box[T], b: Box[T]): int = 0\n" +
          "}\n" +
          "given Ranked[int] { def compare(a: int, b: int): int = 0 }\n" +
          "given Ranked[string] { def compare(a: string, b: string): int = 0 }\n" +
          "def srt[K: Ranked](a: K): int = 0\n" +
          "val r = srt(new Box[int](1))\n" +
          "val s = srt(new Box[string](\"a\"))"
      )
      evidenceRecordGoals(comp) shouldBe Seq(
        "Ranked<Box<int>>",
        "Ranked<int>",
        "Ranked<Box<string>>",
        "Ranked<string>"
      )
    }

    /** Nothing asks for `Ranked[string]`, so no record is laid out for it. */
    it("should lay out no record for a given nothing needs") {
      val comp = mkCompilation(
        ordTrait +
          "given Ranked[int] { def compare(a: int, b: int): int = 0 }\n" +
          "given Ranked[string] { def compare(a: string, b: string): int = 0 }\n" +
          "def srt[K: Ranked](a: K): int = 0\n" +
          "val r = srt(1)"
      )
      evidenceRecordGoals(comp) shouldBe Seq("Ranked<int>")
    }

    it("should let an enclosing constraint discharge a call") {
      mkCompilation(
        eqTrait + eqInt + same +
          "def outer[T: Eqv](a: T, b: T): bool = same(a, b)"
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
        "No evidence for Eqv[T]; the enclosing declaration does not require it"
      )
    }

    /** A context bound becomes a parameter appended after the declared ones, so
      * every declared parameter keeps the argument slot it already had
      * (ADR 0005, decision A).
      */
    it("should append an evidence parameter for a context bound") {
      val comp = mkCompilation(eqTrait + "def same[K: Eqv](a: K, b: K): bool = true")
      val program = assertSome(comp.root.lookup("$Program"))
      val same = assertSome(program.lookup("same"))

      memberSignature(same) shouldBe Seq(
        "TypeParameter(Invariant):K",
        "Parameter:a",
        "Parameter:b",
        "Evidence:$ev$K$Eqv"
      )
    }

    /** Order is type-parameter declaration order. It has to be deterministic:
      * stage 3 compares bytecode from two compilers, and a different parameter
      * order is different bytecode.
      */
    it("should order evidence parameters by type parameter") {
      val comp = mkCompilation(
        eqTrait + ordTrait + "def f[K: Eqv, V: Ranked](a: K, b: V): bool = true"
      )
      val program = assertSome(comp.root.lookup("$Program"))
      val f = assertSome(program.lookup("f"))

      memberSignature(f) shouldBe Seq(
        "TypeParameter(Invariant):K",
        "TypeParameter(Invariant):V",
        "Parameter:a",
        "Parameter:b",
        "Evidence:$ev$K$Eqv",
        "Evidence:$ev$V$Ranked"
      )
    }

    /** Decision B: the constructor is elaborated like any other method, and the
      * class keeps what it received in a field so instance methods reach it
      * through `this`.
      */
    it("should give a constrained class an evidence parameter and field") {
      val comp = mkCompilation(eqTrait + "class Box[T: Eqv](value: T)")
      val box = assertSome(comp.root.lookup("Box"))

      memberSignature(assertSome(box.lookup(".ctor"))) shouldBe Seq(
        "Parameter:value",
        "Evidence:$ev$T$Eqv"
      )
      memberSignature(box) should contain("Field:$ev$T$Eqv")
    }

    it("should not touch an unconstrained generic") {
      val comp = mkCompilation("def id[K](a: K): K = a")
      val program = assertSome(comp.root.lookup("$Program"))

      memberSignature(assertSome(program.lookup("id"))) shouldBe Seq(
        "TypeParameter(Invariant):K",
        "Parameter:a"
      )
    }

    /** The hazard ADR 0005 lists: there are six `reportArgumentCountMismatch`
      * sites, and if evidence were visible to them every call to a constrained
      * generic would report a mismatch. Keeping constraints in `traits` until
      * after binding is what prevents it.
      */
    it("should count arity from the declared parameters only") {
      mkCompilation(eqTrait + eqInt + same + "val r = same(1, 2)")

      val comp =
        mkFailingCompilation(eqTrait + eqInt + same + "val r = same(1)")
      diagnosticMessages(comp) should contain("Expected 2 arguments, but got 1")
    }

    /** ADR 0004's contextual extensions. `equals` does not become a member of
      * `T` — a type parameter has no members at all — it is resolved through
      * the applicable `Eqv[T]` evidence, which is an ordinary symbol in scope.
      */
    it("should resolve a trait member through evidence in scope") {
      mkCompilation(
        eqTrait + eqInt + "def same[T: Eqv](a: T, b: T): bool = a.equals(b)"
      )
    }

    it("should resolve a trait member and then the call to it") {
      mkCompilation(
        eqTrait + eqInt +
          "def same[T: Eqv](a: T, b: T): bool = a.equals(b)\n" +
          "val r = same(1, 2)"
      )
    }

    it("should reject a trait member with no evidence in scope") {
      val comp = mkFailingCompilation(
        eqTrait + eqInt + "def same[T](a: T, b: T): bool = a.equals(b)"
      )
      diagnosticMessages(comp) should contain(
        "Symbol equals not found for type $0"
      )
    }

    it("should reject a member no evidence in scope supplies") {
      val comp = mkFailingCompilation(
        eqTrait + eqInt + "def same[T: Eqv](a: T, b: T): bool = a.compare(b)"
      )
      diagnosticMessages(comp) should contain(
        "Symbol compare not found for type $0"
      )
    }

    /** `a.equals(b)` calls `equals(a, b)`: the trait declares both operands as
      * parameters, so the value left of the dot is the first argument. The
      * count reported is what the user has to write, not the elaborated one.
      */
    it("should count evidence call arity without the receiver") {
      val comp = mkFailingCompilation(
        eqTrait + eqInt + "def same[T: Eqv](a: T, b: T): bool = a.equals(b, b)"
      )
      diagnosticMessages(comp) should contain("Expected 1 arguments, but got 2")
    }

    /** Which declaration is reported as the duplicate follows binding order,
      * not source order, and traits bind before classes — so the class is the
      * one flagged here even though it is written second. That is pre-existing
      * behaviour between classes, objects and enums; traits just join it.
      */
    it("should report a trait colliding with a class") {
      val comp = mkFailingCompilation(
        "trait Display { def show(): string }\nclass Display()"
      )
      diagnosticMessages(comp) should contain(
        "Duplicate definition of Display at :2:8"
      )
    }

    /** An operator member is an ordinary method whose name is the token's own
      * text, so it needs nothing from the binder that `def` did not already
      * have (ADR 0004).
      *
      * `+` throughout, because the prelude already owns the comparisons: these
      * are about a trait a user writes.
      */
    val concat = "trait Concat[T] { operator +(a: T, b: T): T }\n"

    it("should bind a trait operator as a method named after its token") {
      val comp = mkCompilation(concat)
      memberSignature(assertSome(comp.root.lookup("Concat"))) should contain(
        "Method:+"
      )
    }

    it("should claim an operator token for its trait") {
      val comp = mkCompilation(concat)
      assertSome(
        comp.binder.operatorTraits.get(SyntaxKind.PlusToken)
      ) shouldBe assertSome(comp.root.lookup("Concat"))
    }

    /** ADR 0004: a token may be claimed by at most one trait, or `a + b` would
      * need overload resolution across traits.
      */
    it("should reject a second trait claiming the same token") {
      val comp = mkFailingCompilation(
        concat + "trait Join[T] { operator +(a: T, b: T): T }"
      )
      diagnosticMessages(comp) should contain(
        "Operator + is already declared by Concat"
      )
    }

    /** The prelude claims `==` for `Eq`, and the rule does not distinguish
      * where a claim came from.
      */
    it("should reject a trait claiming a token the prelude owns") {
      val comp = mkFailingCompilation(
        "trait Same[T] { operator ==(a: T, b: T): bool }"
      )
      diagnosticMessages(comp) should contain(
        "Operator == is already declared by Eq"
      )
    }

    it("should let one trait claim several tokens") {
      val comp = mkCompilation(
        "trait Arith[T] { operator +(a: T, b: T): T\n" +
          "operator -(a: T, b: T): T }"
      )
      val arith = assertSome(comp.root.lookup("Arith"))

      assertSome(
        comp.binder.operatorTraits.get(SyntaxKind.PlusToken)
      ) shouldBe arith
      assertSome(
        comp.binder.operatorTraits.get(SyntaxKind.DashToken)
      ) shouldBe arith
    }

    /** A given supplies the implementation for a token its trait already owns,
      * so it claims nothing of its own — otherwise the second given for a
      * trait would collide with the first.
      */
    it("should not let a given claim a token") {
      mkCompilation(
        concat +
          "given Concat[int] { operator +(a: int, b: int): int = a + b }\n" +
          "given Concat[string] { operator +(a: string, b: string): string = a + b }"
      )
    }

    it("should bind a given's operator implementation") {
      val comp = mkCompilation(
        concat + "given Concat[int] { operator +(a: int, b: int): int = a + b }"
      )
      givenHeads(comp) shouldBe Seq("Concat<int>")

      val given0 = assertSome(comp.root.lookup("$given$0"))
      memberSignature(given0) should contain("Method:+")
    }

    /** Nothing is declared: `Eq` and its `given Eq[int]` are the prelude's, so
      * `a == b` under a context bound resolves with no setup at all.
      */
    it("should resolve == on a type parameter through its context bound") {
      mkCompilation("def same[T: Eq](a: T, b: T): bool = a == b")
    }

    /** No context bound, so nothing holds evidence for `T`. What the source is
      * missing is the operator, so that is what is reported — the reader has
      * not asked for evidence and should not be told about it.
      *
      * `<` rather than `==`: ADR 0003's identity rule accepts `==` between two
      * unconstrained type parameters, which predates this change and is left
      * as it is here.
      */
    it("should reject an operator on an unconstrained type parameter") {
      val comp = mkFailingCompilation("def lt[T](a: T, b: T): bool = a < b")
      diagnosticMessages(comp) should contain(
        "No operator '<' for operands $0 and $0"
      )
    }

    val boxEq = "class Box(value: int)\n" +
      "given Eq[Box] {\n" +
      "  operator ==(a: Box, b: Box): bool = a.value == b.value\n" +
      "  operator !=(a: Box, b: Box): bool = a.value != b.value\n" +
      "}\n"

    it("should resolve == on a ground type through its given") {
      mkCompilation(boxEq + "val r = new Box(1) == new Box(1)")
    }

    /** ADR 0003's reference-identity rule is now the last resort rather than
      * part of the table lookup. A class with no `Eq` evidence still compares
      * by identity, so nothing that worked before this change stops working.
      */
    it("should still compare reference types by identity with no evidence") {
      mkCompilation(
        "class Box(value: int)\nval r = new Box(1) == new Box(1)"
      )
    }

    /** That evidence wins over identity where both could answer is settled by
      * `VmTests`: two separately constructed `Box(4)` compare equal, which the
      * identity rule would call false.
      */
    it("should reject == on a ground type with no given") {
      val comp = mkFailingCompilation(
        "class Box(value: int)\nval r = new Box(1) == 1"
      )
      diagnosticMessages(comp) should contain(
        "No operator '==' for operands Box and int"
      )
    }

    /** ADR 0004's derivation. The attribute declares a given for each trait it
      * names, owned by the type's own declaration — which is what satisfies
      * the ownership half of coherence for free.
      */
    it("should register a given for each derived trait") {
      val comp = mkCompilation(
        "[derive(Eq, Ord, Show)]\nclass Point(x: int, y: int)"
      )
      givenHeads(comp) shouldBe Seq("Eq<Point>", "Ord<Point>", "Show<Point>")
    }

    it("should give a derived given the trait's own members") {
      val comp = mkCompilation("[derive(Ord)]\nclass Point(x: int, y: int)")

      memberSignature(
        assertSome(comp.root.lookup("$derived$Ord$Point"))
      ) shouldBe Seq("Method:<", "Method:<=", "Method:>", "Method:>=")
    }

    /** Nothing is derived without the attribute, so `a == b` on a class with
      * no `Eq` still means identity rather than a silent structural compare.
      */
    it("should derive nothing without the attribute") {
      val comp = mkCompilation("class Point(x: int, y: int)")
      givenHeads(comp) shouldBe Seq.empty
    }

    it("should reject deriving a trait that is not Eq, Ord or Show") {
      val comp = mkFailingCompilation(
        "trait Printable[T] { def print(a: T): string }\n" +
          "[derive(Printable)]\nclass Point(x: int)"
      )
      diagnosticMessages(comp) should contain(
        "Printable cannot be derived; only Eq, Ord and Show can"
      )
    }

    /** A generic type's derived given is conditional — `Eq[Box[T]]` given
      * `Eq[T]` — which is what ADR 0006 makes expressible. The head applies the
      * class to its own type variable, so it is stated once and instantiated
      * per use.
      */
    it("should derive for a generic type") {
      val comp = mkCompilation("[derive(Eq)]\nclass Box[T](value: T)")
      givenHeads(comp) shouldBe Seq("Eq<Box<$0>>")
    }

    /** The premise is one per type parameter, in declaration order, which is
      * also the dependency half's layout.
      */
    it("should derive for a type with two parameters") {
      val comp = mkCompilation("[derive(Eq)]\nclass Pair[A, B](a: A, b: B)")
      givenHeads(comp) shouldBe Seq("Eq<Pair<$0, $1>>")

      val comp2 = mkCompilation(
        "[derive(Eq)]\nclass Pair[A, B](a: A, b: B)\n" +
          "def same[T: Eq](x: T, y: T): bool = x == y\n" +
          "val r = same(new Pair[int, string](1, \"a\"), " +
          "new Pair[int, string](1, \"a\"))"
      )
      evidenceRecordGoals(comp2) shouldBe Seq(
        "Eq<Pair<int, string>>",
        "Eq<int>",
        "Eq<string>"
      )
    }

    /** The instantiation has to have evidence for what it substitutes in, and
      * the premise is what reports when it does not — `Eq[bool]` exists but
      * `Ord[bool]` deliberately does not.
      */
    it("should report a generic derivation instantiated without evidence") {
      val comp = mkFailingCompilation(
        "[derive(Ord)]\nclass Box[T](value: T)\n" +
          "def srt[T: Ord](x: T): bool = x < x\n" +
          "val r = srt(new Box[bool](true))"
      )
      diagnosticMessages(comp) should contain("No given instance for Ord<bool>")
    }

    /** A parameter whose type is a composite over the type variable —
      * `List[T]` rather than `T` — is neither ground nor a premise, so there is
      * nothing to point at. This is the limit of what ADR 0006 step 5 reaches.
      */
    it("should reject deriving over a composite of a type parameter") {
      val comp = mkFailingCompilation(
        "class Holder[T](value: T)\n" +
          "[derive(Eq)]\nclass Box[T](held: Holder[T])"
      )
      diagnosticMessages(comp) should contain(
        "Cannot derive Eq: no Eq[Holder<$0>] for field held"
      )
    }

    /** Derivation needs evidence for every parameter type and names both the
      * parameter that lacks it and the evidence that would settle it, rather
      * than reporting the type as a whole. The transpiler now derives for over
      * a hundred classes at once, so the report has to be actionable on its
      * own.
      */
    it("should reject deriving over a parameter with no evidence") {
      val comp = mkFailingCompilation(
        "class Inner(v: int)\n[derive(Eq)]\nclass Outer(inner: Inner)"
      )
      diagnosticMessages(comp) should contain(
        "Cannot derive Eq: no Eq[Inner] for field inner"
      )
    }

    /** A parameterless case is one value, so it gets a static field on the
      * program object — the same placement the evidence records use, and for
      * the same reason: it is built once, in `$runtimeInit`.
      */
    it("should give each parameterless enum case a singleton field") {
      val comp = mkCompilation(
        "enum Color {\n  case Red\n  case Green\n}\n" +
          "enum Shape {\n  case Circle(r: int)\n}"
      )
      val program = assertSome(comp.root.lookup("$Program"))

      memberSignature(program) should contain("Field:$case$Color$Red")
      memberSignature(program) should contain("Field:$case$Color$Green")
      // a case with parameters is constructed at each use, not shared
      memberSignature(program) should not contain "Field:$case$Shape$Circle"
    }

    /** An enum derives the same three traits a class does. Its cases are
      * matched first, then their parameters (ADR 0004).
      */
    val shape = "[derive(Eq, Ord, Show)]\n" +
      "enum Shape {\n  case Circle(r: int)\n  case Rect(w: int, h: int)\n}\n"

    it("should register a given for each trait an enum derives") {
      val comp = mkCompilation(shape)
      givenHeads(comp) shouldBe Seq("Eq<Shape>", "Ord<Shape>", "Show<Shape>")
    }

    /** An enum is an alias for the union of its cases, so a generic enum's head
      * is an alias applied to its own type variables. Only unification relates
      * that to an instantiation, which is what `matchType` gained for
      * `Type.Alias`.
      */
    it("should derive for a generic enum") {
      val comp = mkCompilation(
        "[derive(Eq)]\nenum Opt[T] {\n  case Nothing\n  case Has(value: T)\n}"
      )
      givenHeads(comp) shouldBe Seq("Eq<Opt<$0>>")
    }

    /** A recursive case — `tail` has the enum's own type — is discharged by
      * `$ev$self` rather than by a premise, since the goal is what the given
      * itself proves. This is the `List` shape.
      */
    it("should derive for a recursive generic enum") {
      val comp = mkCompilation(
        "[derive(Eq)]\n" +
          "enum Chain[T] {\n" +
          "  case Empty\n" +
          "  case Link(head: T, tail: Chain[T])\n" +
          "}"
      )
      givenHeads(comp) shouldBe Seq("Eq<Chain<$0>>")
    }

    /** The instantiation still has to have evidence for what it substitutes
      * in, and it is the premise that reports when it does not.
      *
      * The annotation is load-bearing: a bare `Opt.Has(true)` infers the case
      * type, and only the operator and extension paths widen a case to its
      * enum. That is a gap in the constrained-call path, and it predates
      * generic enums.
      */
    it("should report a generic enum instantiated without evidence") {
      val comp = mkFailingCompilation(
        "[derive(Ord)]\nenum Opt[T] {\n  case Has(value: T)\n}\n" +
          "def srt[T: Ord](x: T): bool = x < x\n" +
          "val v: Opt[bool] = Opt.Has(true)\n" +
          "val r = srt(v)"
      )
      diagnosticMessages(comp) should contain("No given instance for Ord<bool>")
    }

    /** Evidence is declared for the enum, not per case, so a field declared as
      * one case has to look past it. `evidenceTypes` already did this for the
      * operator paths; `widenGoal` is the same rule for a goal.
      */
    it("should derive over a field typed as an enum case") {
      val comp = mkCompilation(
        "[derive(Eq)]\nenum Shape {\n  case Circle(r: int)\n  case Rect(w: int)\n}\n" +
          "[derive(Eq)]\nclass Holder(shape: Shape.Circle)"
      )
      diagnosticMessages(comp) shouldBe empty
    }

    /** One record per enum rather than one per case: the widened goal is what
      * resolution interns under, so two cases of one enum share it.
      */
    it("should intern one record for every case of an enum") {
      val comp = mkCompilation(
        "[derive(Eq)]\nenum Shape {\n  case Circle(r: int)\n  case Rect(w: int)\n}\n" +
          "[derive(Eq)]\nclass Two(a: Shape.Circle, b: Shape.Rect)"
      )
      // `Eq<int>` first: Shape's own derived body resolves it before anything
      // asks for Shape
      evidenceRecordGoals(comp) shouldBe Seq("Eq<int>", "Eq<Shape>")
    }

    /** Widening is only a fallback. A case that proves something in its own
      * right keeps its own evidence, which is what stops the enum's given
      * silently taking over a more specific one.
      */
    it("should prefer a given on the case over the enum's") {
      val comp = mkCompilation(
        "[derive(Eq)]\nenum Shape {\n  case Circle(r: int)\n  case Rect(w: int)\n}\n" +
          "given Eq[Shape.Circle] {\n" +
          "  operator ==(a: Shape.Circle, b: Shape.Circle): bool = true\n" +
          "  operator !=(a: Shape.Circle, b: Shape.Circle): bool = false\n" +
          "}\n" +
          "[derive(Eq)]\nclass Holder(shape: Shape.Circle)"
      )
      // the case's own given, not the enum's — `Eq<int>` is Shape's own body
      evidenceRecordGoals(comp) shouldBe Seq("Eq<int>", "Eq<Shape.Circle>")
    }

    it("should reject deriving a trait an enum has no rule for") {
      val comp = mkFailingCompilation(
        "trait Printable[T] { def print(a: T): string }\n" +
          "[derive(Printable)]\nenum Shape {\n  case Circle(r: int)\n}"
      )
      diagnosticMessages(comp) should contain(
        "Printable cannot be derived; only Eq, Ord and Show can"
      )
    }

    /** A case's type is not the enum's, and evidence is declared for the enum —
      * one `Eq[Shape]`, not one per case — so resolution has to look past the
      * case to find it.
      */
    it("should resolve an enum's evidence from a case type") {
      mkCompilation(shape + "val r = Shape.Circle(1) == Shape.Circle(1)")
      mkCompilation(shape + "val r = Shape.Circle(1) < Shape.Rect(1, 1)")
      mkCompilation(shape + "val r = Shape.Circle(1).show()")
    }

    /** A type whose parameter is another derived type composes: the inner
      * given is registered before any body is built, so the outer one finds
      * it.
      */
    it("should derive over a parameter that is itself derived") {
      mkCompilation(
        "[derive(Eq)]\nclass Inner(v: int)\n" +
          "[derive(Eq)]\nclass Outer(inner: Inner)"
      )
    }

    /** A user given for a type the prelude has no given for is fine; one for a
      * type it does have collides, which is coherence doing its job.
      */
    it("should reject a user given overlapping a prelude given") {
      val comp = mkFailingCompilation(
        "given Eq[int] { operator ==(a: int, b: int): bool = false\n" +
          "operator !=(a: int, b: int): bool = true }"
      )
      diagnosticMessages(comp) should contain(
        "Given for Eq<int> overlaps the one at :1:1"
      )
    }
  }
}
