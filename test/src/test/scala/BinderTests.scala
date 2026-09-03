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
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertProgramSymbol(symbols)
      assertMainSymbol(symbols)
      assertNoSymbols(symbols)
    }

    it("should bind classes with args") {
      val comp = mkCompilation("class Foo(x: int, y: int)")
      val symbols = enumNonBuiltinSymbols(comp)
      assertSymbol(symbols, SymbolKind.Class, "Foo")
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

    it("should bind out as covariant and in as contravariant") {
      val comp = mkCompilation(
        "class Producer[out T]()\n" +
          "class Consumer[in T]()\n" +
          "class Fixed[T]()"
      )
      val symbols = enumNonBuiltinSymbols(comp)
      assertSymbol(symbols, SymbolKind.Class, "Producer")
      assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Covariant), "T")
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertSymbol(symbols, SymbolKind.Class, "Consumer")
      assertSymbol(
        symbols,
        SymbolKind.TypeParameter(Variance.Contravariant),
        "T"
      )
      assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
      assertSymbol(symbols, SymbolKind.Class, "Fixed")
      assertSymbol(symbols, SymbolKind.TypeParameter(Variance.Invariant), "T")
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
  }
}
