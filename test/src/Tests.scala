import panther._
import TestFramework._

object Tests {
  def main(args: Array[String]): Unit = {
    LexerTests.run()
    ParserTests.run()
    BinderTests.run()
    TypeTests.run()

    complete()
  }
}
