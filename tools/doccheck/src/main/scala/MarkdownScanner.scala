// The compiler's runtime defines root-package Option/Either that would otherwise
// shadow the Scala ones; explicit imports outrank package members.
import scala.{Either, Left, None, Option, Right, Some}

import scala.collection.mutable.ArrayBuffer

/** Pulls fenced `panther` code blocks out of markdown files.
  *
  * This is a deliberately small CommonMark subset: fences opened with three or
  * more backticks or tildes, indented by at most three spaces, closed by a
  * fence of the same character that is at least as long. That is enough for the
  * docs site, and keeping it small means the checker has no markdown dependency
  * to keep in step with VitePress.
  */
object MarkdownScanner {

  private val Fence = """^ {0,3}(`{3,}|~{3,})(.*)$""".r
  private val DirectiveComment =
    """^\s*<!--\s*panther-check:\s*(.*?)\s*-->\s*$""".r
  private val ContextOpen = """^\s*<!--\s*panther-context\s*$""".r
  private val CommentClose = """^\s*-->\s*$""".r
  private val Attribute = """([A-Za-z][\w-]*)="([^"]*)"""".r

  /** True when a fence's info string opens a Panther block. Matches `panther`
    * and `panther title="x"` but not `pantherfoo`.
    */
  private def isPantherInfo(info: String): Boolean = {
    val trimmed = info.trim
    val lang = trimmed.takeWhile(c => !c.isWhitespace)
    lang.equalsIgnoreCase("panther")
  }

  private def metaOf(info: String): String =
    info.trim.dropWhile(c => !c.isWhitespace).trim

  /** Parses the body of a `panther-check:` comment. Unknown modes and a `skip`
    * without a reason are reported as errors rather than silently ignored — a
    * typo in a directive would otherwise quietly disable checking.
    */
  def parseDirective(body: String): Either[String, Directive] = {
    val mode = body.takeWhile(c => !c.isWhitespace)
    val attributes = Attribute
      .findAllMatchIn(body)
      .map(m => m.group(1) -> m.group(2))
      .toMap

    mode match {
      case "check"        => Right(Directive.Check)
      case "parse-only"   => Right(Directive.ParseOnly)
      case "expect-error" => Right(Directive.ExpectError)
      case "ignore" =>
        attributes.get("reason") match {
          case Some(reason) if reason.nonEmpty =>
            Right(Directive.Ignore(reason))
          case _ =>
            Left("`ignore` requires a reason, e.g. ignore reason=\"...\"")
        }
      case "skip" =>
        attributes.get("reason") match {
          case Some(reason) if reason.nonEmpty => Right(Directive.Skip(reason))
          case _ =>
            Left("`skip` requires a reason, e.g. skip reason=\"...\"")
        }
      case other =>
        Left(
          s"unknown mode '$other' (expected check, parse-only, expect-error, ignore or skip)"
        )
    }
  }

  /** Extracts every Panther block in `text`.
    *
    * @param file
    *   path recorded on each block; only used for reporting
    * @param problems
    *   receives malformed directives, which are reported as failures by the
    *   caller instead of aborting the scan
    */
  def scan(
      file: String,
      text: String,
      problems: ArrayBuffer[String]
  ): Seq[DocBlock] = {
    // Some doc files are still CRLF. Split on either ending and rejoin block
    // bodies with LF so the lexer never sees a stray carriage return.
    val lines = text.split("\r\n|\n|\r", -1)
    val blocks = ArrayBuffer[DocBlock]()

    var index = 0
    var ordinal = 0
    var pending: Option[Directive] = None
    val context = ArrayBuffer[ContextCode]()
    // The block an `output` fence would belong to: the last panther block,
    // provided only blank lines have come since.
    var outputOwner: Option[Int] = None

    while (index < lines.length) {
      val line = lines(index)

      line match {
        case ContextOpen() =>
          val openLine = index + 1
          val body = ArrayBuffer[String]()
          index += 1
          var closed = false
          while (index < lines.length && !closed) {
            lines(index) match {
              case CommentClose() => closed = true
              case other          => body += other
            }
            index += 1
          }
          if (closed) context += ContextCode(openLine + 1, body.mkString("\n"))
          else
            problems += s"$file:$openLine: unterminated panther-context comment"
          pending = None

        case DirectiveComment(body) =>
          parseDirective(body) match {
            case Right(directive) => pending = Some(directive)
            case Left(message) =>
              problems += s"$file:${index + 1}: $message"
          }
          index += 1

        case Fence(marker, info) =>
          val fenceLine = index + 1
          val body = ArrayBuffer[String]()
          index += 1

          var closed = false
          while (index < lines.length && !closed) {
            lines(index) match {
              case Fence(closing, trailing)
                  if closing.charAt(0) == marker.charAt(0) &&
                    closing.length >= marker.length &&
                    trailing.trim.isEmpty =>
                closed = true
              case other =>
                body += other
            }
            index += 1
          }

          val isOutput = info.trim.equalsIgnoreCase("output")
          if (isOutput) {
            if (!closed)
              problems += s"$file:$fenceLine: unterminated output fence"
            outputOwner match {
              case Some(owner) if pending.isEmpty =>
                blocks(owner) = blocks(owner).copy(output =
                  Some(OutputFence(fenceLine, body.mkString("\n"), body.length))
                )
              case _ =>
                problems += s"$file:$fenceLine: an output fence must directly follow a panther block"
            }
            outputOwner = None
          } else if (isPantherInfo(info)) {
            ordinal += 1
            if (!closed) {
              problems += s"$file:$fenceLine: unterminated panther code fence"
            }
            blocks += DocBlock(
              file = file,
              ordinal = ordinal,
              fenceLine = fenceLine,
              meta = metaOf(info),
              directive = pending.getOrElse(Directive.Check),
              code = body.mkString("\n"),
              context = context.toSeq
            )
            outputOwner = Some(blocks.length - 1)
          } else {
            if (pending.isDefined)
              problems += s"$file:$fenceLine: panther-check directive precedes a non-panther fence"
            outputOwner = None
          }

          pending = None

        case _ =>
          if (line.trim.nonEmpty) {
            pending = None
            outputOwner = None
          }
          index += 1
      }
    }

    blocks.toSeq
  }
}
