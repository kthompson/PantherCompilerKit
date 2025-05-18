import Helpers._
import panther._
import TestFramework._

object TypeTests {
  def run(): unit = {
    suite("Type Tests")
    primitives()
    binary()
    unary()
    groups()
    variables()
    conversions()
    ifs()
    casts()
    whiles()
    blocks()
    calls()
    // FIXME: matches()

    methods()

    enums()
    classes()

//    generics()
  }

  def primitives(): unit = {
    assertExprTypeTest("12", "int")
    assertExprTypeTest("0", "int")
    assertExprTypeTest("true", "bool")
    assertExprTypeTest("false", "bool")
    assertExprTypeTest("\"hello\"", "string")
    assertExprTypeTest("'a'", "char")
    assertExprTypeTest("()", "unit")
  }

  def binary(): unit = {
    assertExprTypeTest("1 + 2", "int")
    assertExprTypeTest("1 - 2", "int")
    assertExprTypeTest("1 * 2", "int")
    assertExprTypeTest("1 / 2", "int")
    assertExprTypeTest("1 % 2", "int")
    assertExprTypeTest("1 == 2", "bool")
    assertExprTypeTest("1 != 2", "bool")
    assertExprTypeTest("1 < 2", "bool")
    assertExprTypeTest("1 <= 2", "bool")
    assertExprTypeTest("1 > 2", "bool")
    assertExprTypeTest("1 >= 2", "bool")
    assertExprTypeTest("true && false", "bool")
    assertExprTypeTest("true || false", "bool")
  }

  def unary(): unit = {
    assertExprTypeTest("-1", "int")
    assertExprTypeTest("+1", "int")
// FIXME:   assertExprType("~7", "int")
    assertExprTypeTest("!true", "bool")
  }

  def groups(): unit = {
    assertExprTypeTest("(12)", "int")
    assertExprTypeTest("(true)", "bool")
  }

  def variables(): unit = {
    test("int variable")
    assertExprTypeWithSetup("val x = 12", "x", "int")

    test("bool variable")
    assertExprTypeWithSetup("val x = true", "x", "bool")

    test("string variable")
    assertExprTypeWithSetup("val x = \"hello\"", "x", "string")

    test("char variable")
    assertExprTypeWithSetup("val x = 'a'", "x", "char")

    test("variable addition")
    assertExprTypeWithSetup("val x = 12", "x + 12", "int")

    test("variable equality")
    assertExprTypeWithSetup("val x = 12", "12 == x", "bool")

    test("variable assignment")
    assertExprTypeWithSetup("var x = 12", "x = 10", "unit")
  }

  def conversions(): unit = {
    assertAssignableTo("12", "any")
    assertAssignableTo("true", "any")
    assertAssignableTo("\"hello\"", "any")
    assertAssignableTo("'a'", "any")
  }

  def calls(): unit = {
    assertExprTypeTest("println(12)", "unit")
    assertExprTypeTest("print(12)", "unit")
    assertExprTypeTest("print(\"hello\")", "unit")
    assertExprTypeTest("print('a')", "unit")
    assertExprTypeTest("print(true)", "unit")
    assertExprTypeTest("print(12 + 12)", "unit")
    assertExprTypeTest("print(12 == 12)", "unit")
    assertExprTypeTest("print(12 < 12)", "unit")
    assertExprTypeTest("print(true && false)", "unit")
    assertExprTypeTest("print(true || false)", "unit")
  }

  def casts(): unit = {
    assertExprTypeTest("char('a')", "char")
    assertExprTypeTest("char(12)", "char")
    assertExprTypeTest("int('a')", "int")
    assertExprTypeTest("int(12)", "int")
    assertExprTypeTest("string('a')", "string")
    assertExprTypeTest("string(12)", "string")
    assertExprTypeTest("string(\"hello\")", "string")
    assertExprTypeTest("string(true)", "string")
  }

  def blocks(): unit = {
    assertExprTypeTest("{ 1 }", "int")
    assertExprTypeTest("{ true }", "bool")
    assertExprTypeTest("{  }", "unit")
    assertExprTypeTest(
      "{\n" +
        "  1\n" +
        "  2\n" +
        "}",
      "int"
    )
    assertExprTypeTest(
      "{\n" +
        "true\n" +
        "false\n" +
        "}",
      "bool"
    )
    assertExprTypeTest(
      "{\n" +
        "  1\n" +
        "  true\n" +
        "}",
      "bool"
    )
    assertExprTypeTest(
      "{\n" +
        "  true\n" +
        "  1\n" +
        "}",
      "int"
    )
  }

  def ifs(): unit = {
    assertExprTypeTest("if (true) 1 else 2", "int")
    assertExprTypeTest("if (false) 1 else 2", "int")
    assertExprTypeTest("if (true) true else false", "bool")
    assertExprTypeTest("if (false) true else false", "bool")
    assertExprTypeTest("if (false) true", "unit")
  }

  def whiles(): unit = {
    assertExprTypeTest("while (true) 1", "unit")
    assertExprTypeTest("while (false) 1", "unit")
  }

  def matches(): unit = {
    assertExprTypeTest("1 match { case 1 => 1 }", "int")
    assertExprTypeTest("1 match { case 2 => 1 }", "int")
    assertExprTypeTest("1 match { case 1 => true }", "bool")
    assertExprTypeTest("1 match { case 2 => true }", "bool")
    assertExprTypeTest("1 match { case 1 => }", "unit")
  }

  def methods(): unit = {
    methodWithoutReturnType()
    methodWithParameters()
  }

  def generics(): unit = {
    methodWithGenericArguments()
    enumWithGenericType()
  }

  def enumWithGenericType(): unit = {
    val setup = "enum Option<T> {\n" +
      "  case Some(value: T)\n" +
      "  case None\n" +
      "}"
    assertExprTypeWithSetup(setup, "new Option.Some(12)", "Option<int>")
    assertExprTypeWithSetup(setup, "Option.None", "Option<never>")
  }

  def methodWithoutReturnType(): Unit = {
    val comp = mkCompilation("def foo() = 12")
    val symbols = enumNonBuiltinSymbols(comp)
    assertProgramSymbol(symbols)
    val foo = assertSymbol(symbols, SymbolKind.Method, "foo")
    assertSymbolType(comp, foo, "() -> int")
    assertMainSymbol(symbols)
    assertNoSymbols(symbols)
  }

  def methodWithParameters(): Unit = {
    val comp = mkCompilation("def foo(x: int, y: int) = 12")
    val symbols = enumNonBuiltinSymbols(comp)
    assertProgramSymbol(symbols)
    val foo = assertSymbol(symbols, SymbolKind.Method, "foo")
    assertSymbolType(comp, foo, "(x: int, y: int) -> int")
    val x = assertSymbol(symbols, SymbolKind.Parameter, "x")
    assertSymbolType(comp, x, "int")
    val y = assertSymbol(symbols, SymbolKind.Parameter, "y")
    assertSymbolType(comp, y, "int")
    assertMainSymbol(symbols)
    assertNoSymbols(symbols)
  }

  def methodWithGenericArguments(): Unit = {
//    FIXME: this is throwing an error atm
    val comp = mkCompilation("def foo[T](x: T) = 12")
    val symbols = enumNonBuiltinSymbols(comp)
    val foo = assertSymbol(symbols, SymbolKind.Method, "foo")
    assertSymbolType(comp, foo, "[T](x: T) -> int")
    val x = assertSymbol(symbols, SymbolKind.Parameter, "x")
    assertSymbolType(comp, x, "T")
    assertNoSymbols(symbols)
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
    assertAssignableToWithSetup(setup, "Foo.Bar(12)", "Foo")
    assertAssignableToWithSetup(setup, "Foo.Baz(\"taco\")", "Foo")
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
