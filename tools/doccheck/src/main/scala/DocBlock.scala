// The compiler's runtime defines root-package Option/Either that would otherwise
// shadow the Scala ones; explicit imports outrank package members.
import scala.{None, Option, Some}

/** How a documentation code block should be treated by the checker.
  *
  * Set with an HTML comment on the line before the fence, which markdown
  * renderers drop from the output:
  *
  * {{{
  * <!-- panther-check: expect-error -->
  * }}}
  */
enum Directive {

  /** Parse and bind the block; any diagnostic is a failure. The default. */
  case Check

  /** Parse the block but do not bind it. For snippets that are syntactically
    * complete but reference symbols the checker cannot supply.
    */
  case ParseOnly

  /** The block deliberately shows a compile error. It must produce at least one
    * diagnostic, and every diagnostic must be written into the block as a
    * trailing `// error: ...` comment on the line it points at, so the reader
    * sees exactly what the compiler says. `--update` writes them.
    */
  case ExpectError

  /** The block fails for a reason the docs do not intend, usually a feature
    * that is not implemented yet. It is still compiled, but its diagnostics are
    * not failures. Once it compiles cleanly the run fails, so the directive is
    * removed rather than left to go stale. Requires a reason.
    */
  case Ignore(reason: String)

  /** Do not check the block at all. Requires a reason. */
  case Skip(reason: String)
}

/** Hidden code from a `<!-- panther-context ... -->` comment. Every later block
  * on the page compiles against it, as a separate source file.
  *
  * @param line
  *   1-based line in the markdown file of the first line of `code`
  */
case class ContextCode(line: Int, code: String)

/** An ```` ```output ```` fence directly after a `panther` block: what the
  * block prints when it runs.
  *
  * @param fenceLine
  *   1-based line number of the opening fence
  * @param text
  *   the fence's contents; each printed line, without a trailing newline
  * @param lineCount
  *   how many lines lie between the opening and closing fences
  */
case class OutputFence(fenceLine: Int, text: String, lineCount: Int)

/** A fenced `panther` block extracted from a markdown file.
  *
  * @param file
  *   repository-relative path of the markdown file
  * @param ordinal
  *   1-based index of this block among the panther blocks in `file`
  * @param fenceLine
  *   1-based line number of the opening fence
  * @param meta
  *   the info string after the language tag, e.g. `title="hello.pn"`
  * @param code
  *   the block's contents, without the fences
  * @param context
  *   the hidden context comments that precede the block on its page
  * @param output
  *   the `output` fence that follows the block, if any; the block is then run
  *   and must print exactly that
  */
case class DocBlock(
    file: String,
    ordinal: Int,
    fenceLine: Int,
    meta: String,
    directive: Directive,
    code: String,
    context: Seq[ContextCode] = Seq.empty,
    output: Option[OutputFence] = None
) {

  /** Stable identifier used in reports and in the baseline file.
    *
    * Deliberately an ordinal rather than a line number so that editing prose
    * above a block does not invalidate its baseline entry.
    */
  val id: String = file + "#" + ordinal

  /** 1-based line number in the markdown file of the block's first code line.
    */
  val codeLine: Int = fenceLine + 1

  /** Maps a 1-based line inside the block back to its line in the markdown
    * file, so editors can jump straight to the offending snippet.
    */
  def sourceLine(blockLine: Int): Int = codeLine + blockLine - 1
}
