import TestHelpers.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BinderTests extends AnyFlatSpec with Matchers {

  "Binder" should "create builtin symbols" in {
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

  it should "bind top level fields" in {
    val comp = mkCompilation("val x = 12")
    val symbols = enumNonBuiltinSymbols(comp)
    assertProgramSymbol(symbols)
    assertSymbol(symbols, SymbolKind.Field, "x")
    assertSymbol(symbols, SymbolKind.Method, "main")
    assertNoSymbols(symbols)
  }

  it should "bind methods" in {
    val comp = mkCompilation("def foo() = 12")
    val symbols = enumNonBuiltinSymbols(comp)

    assertProgramSymbol(symbols)
    assertSymbol(symbols, SymbolKind.Method, "foo")
    assertMainSymbol(symbols)
    assertNoSymbols(symbols)
  }

  it should "bind classes without args" in {
    val comp = mkCompilation("class Foo()")
    val symbols = enumNonBuiltinSymbols(comp)
    assertSymbol(symbols, SymbolKind.Class, "Foo")
    assertSymbol(symbols, SymbolKind.Constructor, ".ctor")
    assertProgramSymbol(symbols)
    assertMainSymbol(symbols)
    assertNoSymbols(symbols)
  }

  it should "bind classes with args" in {
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

  it should "bind class fields" in {
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

  it should "bind enums without args" in {
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

  it should "bind enums with args" in {
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

  it should "create locals for extract patterns with variables" in {
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

  it should "not create named locals for discard patterns" in {
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

  it should "handle mixed extract and discard patterns" in {
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
