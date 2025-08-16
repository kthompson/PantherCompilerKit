import utest._

object AllTests extends TestSuite {
  val tests = Tests {
    test("Lexer Tests") - LexerTests.tests
    test("Parser Tests") - ParserTests.tests
    test("Binder Tests") - BinderTests.tests
    test("Type Tests") - TypeTests.tests
    test("Metadata Tests") - MetadataTests.tests
    test("VM Tests") - VmTests.tests
  }
}
