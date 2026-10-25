// The compiler's runtime defines root-package Option/Either that would otherwise
// shadow the Scala ones; explicit imports outrank package members.
import scala.{Either, Left, None, Option, Right, Some}

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}

/** The `// error: ...` comments an `expect-error` block carries.
  *
  * Each diagnostic is shown as a trailing comment on the line it points at.
  * Comments do not change what the compiler sees, and a trailing comment does
  * not move any other line, so annotating a block is idempotent: the
  * diagnostics of the annotated block are the ones that produced it.
  */
object ErrorComments {

  private val Trailing = """\s*// error: .*$""".r

  def strip(line: String): String = Trailing.replaceFirstIn(line, "")

  /** The block's code with exactly one error comment per line that has a
    * diagnostic, and none elsewhere. Several diagnostics on one line are joined
    * with `; `; a diagnostic past the end of the block, such as an unexpected
    * end of input, goes on the last line.
    */
  def annotate(block: DocBlock, problems: Seq[BlockProblem]): String = {
    val lines = block.code.split("\n", -1).map(strip)
    val byLine = LinkedHashMap[Int, ArrayBuffer[String]]()
    problems.foreach { problem =>
      val index = (problem.line - block.codeLine)
        .max(0)
        .min(lines.length - 1)
      val messages = byLine.getOrElseUpdate(index, ArrayBuffer[String]())
      if (!messages.contains(problem.message)) messages += problem.message
    }
    byLine.foreach { case (index, messages) =>
      lines(index) = lines(index) + "  // error: " + messages.mkString("; ")
    }
    lines.mkString("\n")
  }
}
