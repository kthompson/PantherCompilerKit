import Helpers._
import panther._

object TypeTests {
  def run(): unit = {
    primitives()
    binary()
    unary()
    groups()
    variables()
    ifs()
    casts()
    whiles()
    blocks()
    calls()
    // FIXME: matches()

    methods()

    enums()
    classes()
  }

  def primitives(): unit = {
    assertExprType("12", "int")
    assertExprType("0", "int")
    assertExprType("true", "bool")
    assertExprType("false", "bool")
    assertExprType("\"hello\"", "string")
    assertExprType("'a'", "char")
    assertExprType("()", "unit")
  }

  def binary(): unit = {
    assertExprType("1 + 2", "int")
    assertExprType("1 - 2", "int")
    assertExprType("1 * 2", "int")
    assertExprType("1 / 2", "int")
    assertExprType("1 % 2", "int")
    assertExprType("1 == 2", "bool")
    assertExprType("1 != 2", "bool")
    assertExprType("1 < 2", "bool")
    assertExprType("1 <= 2", "bool")
    assertExprType("1 > 2", "bool")
    assertExprType("1 >= 2", "bool")
    assertExprType("true && false", "bool")
    assertExprType("true || false", "bool")
  }

  def unary(): unit = {
    assertExprType("-1", "int")
    assertExprType("+1", "int")
// FIXME:   assertExprType("~7", "int")
    assertExprType("!true", "bool")
  }

  def groups(): unit = {
    assertExprType("(12)", "int")
    assertExprType("(true)", "bool")
  }

  def variables(): unit = {
    assertExprTypeWithSetup("val x = 12", "x", "int")
    assertExprTypeWithSetup("val x = true", "x", "bool")
    assertExprTypeWithSetup("val x = \"hello\"", "x", "string")
    assertExprTypeWithSetup("val x = 'a'", "x", "char")
    assertExprTypeWithSetup("val x = 12", "x + 12", "int")
    assertExprTypeWithSetup("val x = 12", "12 == x", "bool")
    assertExprTypeWithSetup("var x = 12", "x = 10", "unit")
  }

  def calls(): unit = {
    assertExprType("println(12)", "unit")
    assertExprType("print(12)", "unit")
    assertExprType("print(\"hello\")", "unit")
    assertExprType("print('a')", "unit")
    assertExprType("print(true)", "unit")
    assertExprType("print(12 + 12)", "unit")
    assertExprType("print(12 == 12)", "unit")
    assertExprType("print(12 < 12)", "unit")
    assertExprType("print(true && false)", "unit")
    assertExprType("print(true || false)", "unit")
  }

  def casts(): unit = {
    assertExprType("char('a')", "char")
    assertExprType("char(12)", "char")
    assertExprType("int('a')", "int")
    assertExprType("int(12)", "int")
    assertExprType("string('a')", "string")
    assertExprType("string(12)", "string")
    assertExprType("string(\"hello\")", "string")
    assertExprType("string(true)", "string")
  }

  def blocks(): unit = {
    assertExprType("{ 1 }", "int")
    assertExprType("{ true }", "bool")
    assertExprType("{  }", "unit")
    assertExprType(
      "{\n" +
        "  1\n" +
        "  2\n" +
        "}",
      "int"
    )
    assertExprType(
      "{\n" +
        "true\n" +
        "false\n" +
        "}",
      "bool"
    )
    assertExprType(
      "{\n" +
        "  1\n" +
        "  true\n" +
        "}",
      "bool"
    )
    assertExprType(
      "{\n" +
        "  true\n" +
        "  1\n" +
        "}",
      "int"
    )
  }

  def ifs(): unit = {
    assertExprType("if (true) 1 else 2", "int")
    assertExprType("if (false) 1 else 2", "int")
    assertExprType("if (true) true else false", "bool")
    assertExprType("if (false) true else false", "bool")
    assertExprType("if (false) true", "unit")
  }

  def whiles(): unit = {
    assertExprType("while (true) 1", "unit")
    assertExprType("while (false) 1", "unit")
  }

  def matches(): unit = {
    assertExprType("1 match { case 1 => 1 }", "int")
    assertExprType("1 match { case 2 => 1 }", "int")
    assertExprType("1 match { case 1 => true }", "bool")
    assertExprType("1 match { case 2 => true }", "bool")
    assertExprType("1 match { case 1 => }", "unit")
  }

  def methods(): unit = {
    methodWithoutReturnType()
    methodWithParameters()
    methodWithGenericArguments()
  }

  def methodWithoutReturnType(): Unit = {
    val comp = mkCompilation("def foo() = 12")
    val symbols = enumNonBuiltinSymbols(comp)
    val foo = assertSymbol(symbols, SymbolKind.Method, "foo")
    assertSymbolType(comp, foo, "() -> int")
    assertNoSymbol(symbols)
  }

  def methodWithParameters(): Unit = {
    val comp = mkCompilation("def foo(x: int, y: int) = 12")
    val symbols = enumNonBuiltinSymbols(comp)
    val foo = assertSymbol(symbols, SymbolKind.Method, "foo")
    assertSymbolType(comp, foo, "(x: int, y: int) -> int")
    val x = assertSymbol(symbols, SymbolKind.Parameter, "x")
    assertSymbolType(comp, x, "int")
    val y = assertSymbol(symbols, SymbolKind.Parameter, "y")
    assertSymbolType(comp, y, "int")
    assertNoSymbol(symbols)
  }

  def methodWithGenericArguments(): Unit = {
//    FIXME: this is throwing an error atm
//    val comp = mkCompilation("def foo[T](x: T) = 12")
//    val symbols = enumNonBuiltinSymbols(comp)
//    val foo = assertSymbol(symbols, SymbolKind.Method, "foo")
//    assertSymbolType(comp, foo, "[T](x: T) -> int")
//    val x = assertSymbol(symbols, SymbolKind.Parameter, "x")
//    assertSymbolType(comp, x, "T")
//    assertNoSymbol(symbols)
  }

  def enums(): unit = {
    enumWithoutArgs()
    enumWithArgs()
    enumCheckAssignments()
  }

  def enumWithoutArgs(): unit = {
    val setup = "enum Foo {\n" +
      "  case Bar\n" +
      "  case Baz\n" +
      "}"
    assertExprTypeWithSetup(setup, "Foo.Bar", "Foo.Bar")
    assertExprTypeWithSetup(setup, "Foo.Baz", "Foo.Baz")
  }

  def enumWithArgs(): unit = {
    val setup = "enum Foo {\n" +
      "  case Bar(x: int)\n" +
      "  case Baz(y: string)\n" +
      "}"
    assertExprTypeWithSetup(setup, "Foo.Bar(12)", "Foo.Bar")
    assertExprTypeWithSetup(setup, "Foo.Baz(\"taco\")", "Foo.Baz")
    assertExprTypeWithSetup(setup, "new Foo.Bar(12)", "Foo.Bar")
    assertExprTypeWithSetup(setup, "new Foo.Baz(\"taco\")", "Foo.Baz")
  }

  def enumCheckAssignments(): unit = {
    val setup = "enum Foo {\n" +
      "  case Bar(x: int)\n" +
      "  case Baz(y: string)\n" +
      "}"
    assertExprAssignableWithSetup(setup, "Foo.Bar(12)", "Foo")
    assertExprAssignableWithSetup(setup, "Foo.Baz(\"taco\")", "Foo")
  }

  def classes(): unit = {
    classWithoutArgs()
    classWithArgs()
  }

  def classWithoutArgs(): Unit = {
    val setup = "class Foo()"
    assertExprTypeWithSetup(setup, "new Foo()", "Foo")
  }

  def classWithArgs(): unit = {
    val setup = "class Foo(x: int, y: string)"
    assertExprTypeWithSetup(setup, "new Foo(12, \"taco\")", "Foo")
  }

}
