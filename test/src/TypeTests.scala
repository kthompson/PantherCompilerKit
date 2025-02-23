import Helpers._
import panther._

object TypeTests {
  def run(): unit = {
    primitives()
    binary()
    unary()
    groups()
    ifs()
    whiles()
    blocks()

    methods()
  }

  def primitives(): unit = {
    assertExprType("12", "int")
    assertExprType("0", "int")
    assertExprType("true", "bool")
    assertExprType("false", "bool")
    assertExprType("\"hello\"", "string")
    assertExprType("'a'", "char")
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
    assertExprType("!true", "bool")
  }

  def groups(): unit = {
    assertExprType("(12)", "int")
    assertExprType("(true)", "bool")
  }

  def blocks(): unit = {
    assertExprType("{ 1 }", "int")
    assertExprType("{ true }", "bool")
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
  }

  def whiles(): unit = {
    assertExprType("while (true) 1", "unit")
    assertExprType("while (false) 1", "unit")
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

}
