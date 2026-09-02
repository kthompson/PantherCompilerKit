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

  /** The block must produce at least one diagnostic. For snippets that
    * deliberately show a compile error.
    */
  case ExpectError

  /** Do not check the block at all. Requires a reason. */
  case Skip(reason: String)
}

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
  */
case class DocBlock(
    file: String,
    ordinal: Int,
    fenceLine: Int,
    meta: String,
    directive: Directive,
    code: String
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
