import panther._

object Tests {
  def main(args: Array[String]): Unit = {
    LexerTests.run()
    ParserTests.run()
    BinderTests.run()
    TypeTests.run()
    println("All tests passed")
  }
}
