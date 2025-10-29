import panther.*
import TestHelpers.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeTests extends AnyFlatSpec with Matchers {

  "Type checker" should "handle primitive types" in {
    assertExprTypeTest("12", "int")
    assertExprTypeTest("0", "int")
    assertExprTypeTest("true", "bool")
    assertExprTypeTest("false", "bool")
    assertExprTypeTest("\"hello\"", "string")
    assertExprTypeTest("'a'", "char")
    assertExprTypeTest("()", "unit")
  }

  it should "handle binary expressions" in {
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

  it should "handle unary expressions" in {
    assertExprTypeTest("-1", "int")
    assertExprTypeTest("+1", "int")
    // FIXME: assertExprType("~7", "int")
    assertExprTypeTest("!true", "bool")
  }

  it should "handle grouped expressions" in {
    assertExprTypeTest("(12)", "int")
    assertExprTypeTest("(true)", "bool")
  }

  it should "handle int variables" in {
    assertExprTypeWithSetup("val x = 12", "x", "int")
  }

  it should "handle bool variables" in {
    assertExprTypeWithSetup("val x = true", "x", "bool")
  }

  it should "handle string variables" in {
    assertExprTypeWithSetup("val x = \"hello\"", "x", "string")
  }

  it should "handle char variables" in {
    assertExprTypeWithSetup("val x = 'a'", "x", "char")
  }

  it should "handle variable addition" in {
    assertExprTypeWithSetup("val x = 12", "x + 12", "int")
  }

  it should "handle variable equality" in {
    assertExprTypeWithSetup("val x = 12", "12 == x", "bool")
  }

  it should "handle variable assignment" in {
    assertExprTypeWithSetup("var x = 12", "x = 10", "unit")
  }

  it should "handle conversions to any" in {
    assertAssignableTo("12", "any")
    assertAssignableTo("true", "any")
    assertAssignableTo("\"hello\"", "any")
    assertAssignableTo("'a'", "any")
  }

  it should "handle function calls" in {
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

  it should "handle casts" in {
    assertExprTypeTest("'a' as char", "char")
    assertExprTypeTest("12 as char", "char")
    assertExprTypeTest("'a' as int", "int")
    assertExprTypeTest("12 as int", "int")
    assertExprTypeTest("'a' as string", "string")
    assertExprTypeTest("12 as string", "string")
    assertExprTypeTest("\"hello\" as string", "string")
    assertExprTypeTest("true as string", "string")
  }

  it should "handle is expressions" in {
    assertExprTypeTest("12 is int", "bool")
    assertExprTypeTest("'a' is char", "bool")
    assertExprTypeTest("\"hello\" is string", "bool")
    assertExprTypeTest("true is bool", "bool")
    assertExprTypeTest("12 is bool", "bool") // should return false at runtime
  }

  it should "handle block expressions" in {
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

  it should "handle if expressions" in {
    assertExprTypeTest("if (true) 1 else 2", "int")
    assertExprTypeTest("if (false) 1 else 2", "int")
    assertExprTypeTest("if (true) true else false", "bool")
    assertExprTypeTest("if (false) true else false", "bool")
    assertExprTypeTest("if (false) true", "unit")
  }

  it should "handle while expressions" in {
    assertExprTypeTest("while (true) 1", "unit")
    assertExprTypeTest("while (false) 1", "unit")
  }

  it should "handle literal patterns" in {
    assertExprTypeTest("1 match { case 1 => 2 }", "int")
    assertExprTypeTest("1 match { case 2 => 3 }", "int")
    assertExprTypeTest("true match { case true => false }", "bool")
    assertExprTypeTest(
      "\"hello\" match { case \"world\" => \"hi\" }",
      "string"
    )
    assertExprTypeTest("'a' match { case 'b' => 'c' }", "char")
  }

  it should "handle wildcard patterns" in {
    assertExprTypeTest("1 match { case _ => 2 }", "int")
    assertExprTypeTest("true match { case _ => false }", "bool")
    assertExprTypeTest("\"hello\" match { case _ => \"world\" }", "string")
  }

  it should "handle multiple cases with same type" in {
    assertExprTypeTest(
      "1 match { case 1 => 10\n case 2 => 20 }",
      "int"
    )
    assertExprTypeTest(
      "true match { case true => 1\n case false => 0 }",
      "int"
    )
  }

  it should "handle multiple cases with different types" in {
    // When cases have different types, result should be 'any' (least upper bound)
    assertExprTypeTest(
      "1 match { case 1 => 42\n case 2 => \"hello\" }",
      "int | string"
    )
    assertExprTypeTest(
      "1 match { case 1 => true\n case 2 => 123 }",
      "bool | int"
    )
  }

  it should "handle unit result cases" in {
    assertExprTypeTest("1 match { case x: int => () }", "unit")
    assertExprTypeTest("1 match { case 1 => println(\"test\") }", "unit")
  }

  it should "handle nested matches" in {
    assertExprTypeTest(
      "1 match { case x: int => x match { case y: int => y * 2 } }",
      "int"
    )
  }

  it should "handle match with blocks" in {
    assertExprTypeTest(
      "1 match { case x: int => { val y = x + 1\n y * 2 } }",
      "int"
    )
    assertExprTypeTest(
      "true match { case b: bool => { println(\"test\")\n b } }",
      "bool"
    )
  }

  it should "handle methods without return type" in {
    val comp = mkCompilation("def foo() = 12")
    val symbols = enumNonBuiltinSymbols(comp)
    assertProgramSymbol(symbols)
    val foo = assertSymbol(symbols, SymbolKind.Method, "foo")
    assertSymbolType(comp, foo, "() -> int")
    assertMainSymbol(symbols)
    assertNoSymbols(symbols)
  }

  it should "handle methods with parameters" in {
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

  it should "handle simple identity generic method" in {
    val setup = "def identity[T](x: T): T = x"

    assertExprTypeWithSetup(setup, "identity(42)", "int")
    assertExprTypeWithSetup(setup, "identity(true)", "bool")
    assertExprTypeWithSetup(setup, "identity(\"hello\")", "string")
    assertExprTypeWithSetup(setup, "identity('a')", "char")
  }

  it should "handle generic method returning concrete type" in {
    val setup = "def getValue[T](x: T): int = 42"

    // These should work because the return type is concrete
    assertExprTypeWithSetup(setup, "getValue(42)", "int")
    assertExprTypeWithSetup(setup, "getValue(\"hello\")", "int")
    assertExprTypeWithSetup(setup, "getValue(true)", "int")
  }

  it should "handle generic parameter with concrete return" in {
    // Test methods that accept generic parameters but return concrete types
    val setup = "def stringify[T](x: T): string = string(x)"

    assertExprTypeWithSetup(setup, "stringify(42)", "string")
    assertExprTypeWithSetup(setup, "stringify(true)", "string")
    assertExprTypeWithSetup(setup, "stringify('a')", "string")
  }

  it should "handle generic container creation" in {
    val containerSetup = "class Container[T](value: T)\n" +
      "def wrap[T](x: T): Container[T] = new Container(x)"

    assertExprTypeWithSetup(containerSetup, "wrap(42)", "Container<int>")
    assertExprTypeWithSetup(containerSetup, "wrap(true)", "Container<bool>")
    assertExprTypeWithSetup(
      containerSetup,
      "wrap(\"test\")",
      "Container<string>"
    )
  }

  it should "handle enums without args" in {
    val setup = "enum Foo {\n" +
      "  case Bar\n" +
      "  case Baz\n" +
      "}"
    assertExprTypeWithSetup(setup, "Foo.Bar", "Foo.Bar")
    assertExprTypeWithSetup(setup, "Foo.Baz", "Foo.Baz")
  }

  it should "handle enums with args" in {
    val setup = "enum Foo {\n" +
      "  case Bar(x: int)\n" +
      "  case Baz(y: string)\n" +
      "}"
    assertExprTypeWithSetup(setup, "Foo.Bar(12)", "Foo.Bar")
    assertExprTypeWithSetup(setup, "Foo.Baz(\"taco\")", "Foo.Baz")
    assertExprTypeWithSetup(setup, "new Foo.Bar(12)", "Foo.Bar")
    assertExprTypeWithSetup(setup, "new Foo.Baz(\"taco\")", "Foo.Baz")
  }

  it should "check enum assignments" in {
    val setup = "enum Foo {\n" +
      "  case Bar(x: int)\n" +
      "  case Baz(y: string)\n" +
      "}"
    assertAssignableToWithSetup(setup, "Foo.Bar(12)", "Foo")
    assertAssignableToWithSetup(setup, "Foo.Baz(\"taco\")", "Foo")
  }

  it should "handle enums with generic type" in {
    val setup = "enum Option[T] {\n" +
      "  case Some(value: T)\n" +
      "  case None\n" +
      "}"
    assertAssignableToWithSetup(setup, "new Option.Some(12)", "Option[int]")
    assertAssignableToWithSetup(
      setup,
      "new Option.Some(12)",
      "Option.Some[int]"
    )
    assertAssignableToWithSetup(setup, "Option.Some(12)", "Option[int]")
    assertAssignableToWithSetup(setup, "Option.None", "Option[int]")
    assertAssignableToWithSetup(setup, "Option.None", "Option[never]")
  }

  it should "handle list examples" in {
    val setup = "enum List[T] {\n" +
      "  case Cons(head: T, tail: List[T])\n" +
      "  case Nil\n" +
      "}"
    assertAssignableToWithSetup(
      setup,
      "new List.Cons(1, List.Nil)",
      "List[int]"
    )
    assertAssignableToWithSetup(
      setup,
      "List.Cons(1, List.Nil)",
      "List[int]"
    )
    assertAssignableToWithSetup(
      setup,
      "new List.Cons(\"hello\", List.Nil)",
      "List[string]"
    )
    assertAssignableToWithSetup(
      setup,
      "List.Cons(\"hello\", List.Nil)",
      "List[string]"
    )
    assertAssignableToWithSetup(setup, "List.Nil", "List[int]")
    assertAssignableToWithSetup(setup, "List.Nil", "List[string]")
  }

  it should "handle classes without args" in {
    val setup = "class Foo()"
    assertExprTypeWithSetup(setup, "new Foo()", "Foo")
  }

  it should "handle classes with args" in {
    val setup = "class Foo(x: int, y: string)"
    assertExprTypeWithSetup(setup, "new Foo(12, \"taco\")", "Foo")
  }

  it should "handle array length type" in {
    val setup = "val array = new Array[int](0)"
    assertExprTypeWithSetup(setup, "array.length", "int")
  }

  it should "handle array apply with method call" in {
    val setup = "val array = new Array[int](1)"
    assertExprTypeWithSetup(setup, "array.apply(0)", "int")
  }

  it should "handle array apply with indexer syntax for basic types" in {
    val setup = "val intArray = new Array[int](1)"
    assertExprTypeWithSetup(setup, "intArray(0)", "int")

    val boolSetup = "val boolArray = new Array[bool](1)"
    assertExprTypeWithSetup(boolSetup, "boolArray(0)", "bool")

    val stringSetup = "val stringArray = new Array[string](1)"
    assertExprTypeWithSetup(stringSetup, "stringArray(0)", "string")

    val charSetup = "val charArray = new Array[char](1)"
    assertExprTypeWithSetup(charSetup, "charArray(0)", "char")
  }

  it should "handle array indexing in expressions" in {
    val setup = "val array = new Array[int](5)"
    assertExprTypeWithSetup(setup, "array(0) + array(1)", "int")
    assertExprTypeWithSetup(setup, "array(2) * 3", "int")
    assertExprTypeWithSetup(setup, "array(0) == array(1)", "bool")
  }

  it should "handle array indexing with computed indices" in {
    val setup = "val array = new Array[int](10)\nval i = 5"
    assertExprTypeWithSetup(setup, "array(i)", "int")
    assertExprTypeWithSetup(setup, "array(1 + 2)", "int")
    assertExprTypeWithSetup(setup, "array(i * 2)", "int")
  }

  it should "handle array indexing assignment - LHS binding fix" in {
    // This tests the specific fix for "NewExpression in bindLHS" error
    val setup = "var array = new Array[int](5)"
    assertExprTypeWithSetup(setup, "array(0) = 42", "unit")
    assertExprTypeWithSetup(setup, "array(1) = array(0) + 1", "unit")
  }

  it should "handle primitive casts" in {
    // Basic numeric casting
    assertExprTypeTest("42 as int", "int")
    assertExprTypeTest("42 as bool", "bool")
    assertExprTypeTest("42 as char", "char")
    assertExprTypeTest("42 as string", "string")

    // Boolean casting
    assertExprTypeTest("true as int", "int")
    assertExprTypeTest("true as bool", "bool")
    assertExprTypeTest("false as string", "string")

    // Character casting
    assertExprTypeTest("'a' as int", "int")
    assertExprTypeTest("'a' as char", "char")
    assertExprTypeTest("'a' as string", "string")

    // String casting
    assertExprTypeTest("\"hello\" as string", "string")
    assertExprTypeTest("\"hello\" as any", "any")
  }

  it should "handle variable casts" in {
    val setup =
      "val x = 42\nval flag = true\nval ch = 'a'\nval text = \"hello\""

    assertExprTypeWithSetup(setup, "x as bool", "bool")
    assertExprTypeWithSetup(setup, "x as char", "char")
    assertExprTypeWithSetup(setup, "x as string", "string")

    assertExprTypeWithSetup(setup, "flag as int", "int")
    assertExprTypeWithSetup(setup, "flag as string", "string")

    assertExprTypeWithSetup(setup, "ch as int", "int")
    assertExprTypeWithSetup(setup, "ch as string", "string")

    assertExprTypeWithSetup(setup, "text as any", "any")
  }

  it should "handle cast to any type" in {
    assertExprTypeTest("42 as any", "any")
    assertExprTypeTest("true as any", "any")
    assertExprTypeTest("\"hello\" as any", "any")
    assertExprTypeTest("'a' as any", "any")
    assertExprTypeTest("() as any", "any")
  }

  it should "handle cast from any type" in {
    val setup = "val obj: any = 42"

    assertExprTypeWithSetup(setup, "obj as int", "int")
    assertExprTypeWithSetup(setup, "obj as bool", "bool")
    assertExprTypeWithSetup(setup, "obj as char", "char")
    assertExprTypeWithSetup(setup, "obj as string", "string")
    assertExprTypeWithSetup(setup, "obj as unit", "unit")
  }

  it should "handle expression casts" in {
    // Cast results of binary operations
    assertExprTypeTest("(1 + 2) as bool", "bool")
    assertExprTypeTest("(true && false) as int", "int")
    assertExprTypeTest("(1 == 2) as string", "string")

    // Cast results of unary operations
    assertExprTypeTest("(!true) as int", "int")
    assertExprTypeTest("(-42) as bool", "bool")
  }

  it should "handle cast with parentheses" in {
    assertExprTypeTest("(42) as string", "string")
    assertExprTypeTest("(true) as int", "int")
    assertExprTypeTest("(\"hello\") as any", "any")
  }

  it should "handle chained operations with casts" in {
    val setup = "val x = 42"

    // Cast should have appropriate precedence
    assertExprTypeWithSetup(setup, "x as bool == true", "bool")
    assertExprTypeWithSetup(setup, "(x as bool) == true", "bool")
  }

  it should "handle basic string conversions" in {
    // Convert literals to string
    assertExprTypeTest("string(42)", "string")
    assertExprTypeTest("string(0)", "string")
    assertExprTypeTest("string(-123)", "string")
    assertExprTypeTest("string(true)", "string")
    assertExprTypeTest("string(false)", "string")
    assertExprTypeTest("string('a')", "string")
    assertExprTypeTest("string('z')", "string")
    assertExprTypeTest("string(())", "string")
  }

  it should "handle string conversions with variables" in {
    val intSetup = "val num = 123"
    assertExprTypeWithSetup(intSetup, "string(num)", "string")

    val boolSetup = "val flag = true"
    assertExprTypeWithSetup(boolSetup, "string(flag)", "string")

    val charSetup = "val ch = 'A'"
    assertExprTypeWithSetup(charSetup, "string(ch)", "string")

    val unitSetup = "val nothing = ()"
    assertExprTypeWithSetup(unitSetup, "string(nothing)", "string")
  }

  it should "handle string conversions with expressions" in {
    // Arithmetic expressions
    assertExprTypeTest("string(1 + 2)", "string")
    assertExprTypeTest("string(10 - 5)", "string")
    assertExprTypeTest("string(3 * 4)", "string")
    assertExprTypeTest("string(15 / 3)", "string")
    assertExprTypeTest("string(17 % 5)", "string")

    // Boolean expressions
    assertExprTypeTest("string(true && false)", "string")
    assertExprTypeTest("string(true || false)", "string")
    assertExprTypeTest("string(!true)", "string")
    assertExprTypeTest("string(5 > 3)", "string")
    assertExprTypeTest("string(5 == 5)", "string")
    assertExprTypeTest("string(5 != 3)", "string")

    // Unary expressions
    assertExprTypeTest("string(-42)", "string")
    assertExprTypeTest("string(+42)", "string")
  }

  it should "handle string conversions in complex expressions" in {
    val setup = "val x = 42\nval y = true"

    // String conversions in comparisons
    assertExprTypeWithSetup(setup, "string(x) == \"42\"", "bool")
    assertExprTypeWithSetup(setup, "string(y) == \"true\"", "bool")
    assertExprTypeWithSetup(setup, "string(x) != \"0\"", "bool")

    // String conversions in arithmetic context
    assertExprTypeWithSetup(setup, "string(x + 10)", "string")
    assertExprTypeWithSetup(setup, "string(x * 2)", "string")
  }

  it should "handle string conversions with method calls" in {
    // Convert results of method calls to string
    assertExprTypeTest("string(println(\"test\"))", "string")
    assertExprTypeTest("string(print(42))", "string")
  }

  it should "handle string conversions with control flow" in {
    // String conversions with if expressions
    assertExprTypeTest("string(if (true) 1 else 2)", "string")
    assertExprTypeTest("string(if (false) true else false)", "string")

    // String conversions with block expressions
    assertExprTypeTest("string({ 42 })", "string")
    assertExprTypeTest("string({ true })", "string")
  }

  it should "handle nested string conversions" in {
    val setup = "val x = 42"

    // String conversion of string (should still work)
    assertExprTypeTest("string(\"hello\")", "string")
    assertExprTypeWithSetup(setup, "string(string(x))", "string")

    // String conversions in nested expressions
    assertExprTypeWithSetup(setup, "string(string(x) == \"42\")", "string")
  }

  it should "handle int conversions" in {
    // Convert various types to int
    assertExprTypeTest("int(42)", "int")
    assertExprTypeTest("int(true)", "int")
    assertExprTypeTest("int(false)", "int")
    assertExprTypeTest("int('a')", "int")

    // Int conversions with variables and expressions
    val setup = "val flag = true\nval ch = 'A'"
    assertExprTypeWithSetup(setup, "int(flag)", "int")
    assertExprTypeWithSetup(setup, "int(ch)", "int")
    assertExprTypeTest("int(1 + 2)", "int")
  }

  it should "handle bool conversions" in {
    // Convert various types to bool
    assertExprTypeTest("bool(true)", "bool")
    assertExprTypeTest("bool(false)", "bool")
    assertExprTypeTest("bool(42)", "bool")
    assertExprTypeTest("bool(0)", "bool")

    // Bool conversions with variables and expressions
    val setup = "val num = 123"
    assertExprTypeWithSetup(setup, "bool(num)", "bool")
    assertExprTypeTest("bool(5 > 3)", "bool")
  }

  it should "handle char conversions" in {
    // Convert various types to char
    assertExprTypeTest("char('a')", "char")
    assertExprTypeTest("char(65)", "char")

    // Char conversions with variables
    val setup = "val ascii = 97"
    assertExprTypeWithSetup(setup, "char(ascii)", "char")
  }
}
