// The compiler's runtime defines root-package Option/Either that would otherwise
// shadow the Scala ones; explicit imports outrank package members.
import scala.{Either, Left, None, Option, Right, Some}

import scala.collection.mutable.ArrayBuffer

/** How far a block is taken through the compiler pipeline. */
enum Stage {
  case Parse
  case Bind
}

object Stage {
  def parse(name: String): Option[Stage] = name match {
    case "parse" => Some(Stage.Parse)
    case "bind"  => Some(Stage.Bind)
    case _       => None
  }
}

/** One compiler complaint about a block, with the line rewritten to point at
  * the markdown file rather than at the extracted snippet.
  */
case class BlockProblem(
    line: Int,
    column: Int,
    message: String,
    inContext: Boolean = false
) {
  def render(block: DocBlock): String =
    s"${block.file}:$line:$column: $message"
}

enum Outcome {

  /** The block behaved as its directive says it should. */
  case Ok

  /** Not checked. */
  case Skipped(reason: String)

  /** Compiled, and rejected, but marked `ignore`. */
  case Ignored(reason: String)

  /** Marked `ignore`, but the block now compiles cleanly. */
  case IgnoredButClean

  /** The block was rejected by the compiler. */
  case Failed(problems: Seq[BlockProblem])

  /** The compiler threw instead of reporting a diagnostic. Always a compiler
    * bug: errors are supposed to travel through the DiagnosticBag.
    */
  case Crashed(detail: String)

  /** `expect-error` was declared but the block compiled cleanly. */
  case UnexpectedlyClean

  /** `expect-error` was declared and the block was rejected, but its
    * `// error:` comments do not match the diagnostics. `expected` is the block
    * as `--update` would write it.
    */
  case StaleErrorComments(expected: String, problems: Seq[BlockProblem])

  /** The block compiled and ran, but printed something other than its `output`
    * fence. `actual` is what `--update` would write.
    */
  case StaleOutput(actual: String)

  /** The block compiled but did not run to completion. */
  case RunFailed(detail: String)

  /** The block's markup is inconsistent, e.g. an `output` fence on a block that
    * is not compiled normally.
    */
  case Invalid(message: String)
}

case class BlockResult(block: DocBlock, outcome: Outcome) {
  val failed: Boolean = outcome match {
    case Outcome.Ok | Outcome.Skipped(_) | Outcome.Ignored(_) => false
    case _                                                    => true
  }
}

/** Runs a single documentation snippet through the compiler front end. */
object BlockChecker {

  private val settings = CompilerSettingsFactory.default

  private def collect(
      diagnostics: Diagnostics,
      into: ArrayBuffer[Diagnostic]
  ): Unit =
    diagnostics match {
      case Diagnostics.Empty => ()
      case Diagnostics.Node(left, head, right) =>
        collect(left, into)
        into += head
        collect(right, into)
    }

  /** The source actually compiled: the page's context comments, then the block,
    * in one file. One file rather than several because the compiler allows
    * top-level statements, including a top-level `def` or `val`, in only one
    * file, and a context is mostly made of those.
    *
    * `contextLines` is how many lines precede the block's first line.
    */
  private case class Combined(code: String, contextLines: Int)

  private def combine(block: DocBlock, stage: Stage): Combined = {
    val contexts = if (stage == Stage.Parse) Seq.empty else block.context
    val prefix = contexts.map(_.code + "\n").mkString
    Combined(prefix + block.code, prefix.count(_ == '\n'))
  }

  /** Maps a 0-based line of the combined source back to the markdown file.
    * Diagnostics in a context comment point at the comment's own lines, and are
    * flagged so they are never mistaken for the block's.
    */
  private def problemsOf(
      block: DocBlock,
      combined: Combined,
      diagnostics: Diagnostics
  ): Seq[BlockProblem] = {
    val collected = ArrayBuffer[Diagnostic]()
    collect(diagnostics, collected)
    collected.map { diagnostic =>
      val location = diagnostic.location
      val column = location.startCharacter + 1
      val line = location.startLine
      if (line >= combined.contextLines)
        BlockProblem(
          line = block.sourceLine(line - combined.contextLines + 1),
          column = column,
          message = diagnostic.message
        )
      else {
        var remaining = line
        var found: Option[BlockProblem] = None
        block.context.foreach { context =>
          val count = context.code.count(_ == '\n') + 1
          if (found.isEmpty && remaining < count)
            found = Some(
              BlockProblem(
                line = context.line + remaining,
                column = column,
                message = "in panther-context: " + diagnostic.message,
                inContext = true
              )
            )
          remaining -= count
        }
        found.get
      }
    }.toSeq
  }

  /** The compiler prints progress and, in a few paths, debugging output. Doc
    * checking runs it hundreds of times, so that noise is swallowed and only
    * the checker's own report reaches the terminal.
    */
  private def quietly[A](body: => A): A = {
    val sink = new java.io.PrintStream(java.io.OutputStream.nullOutputStream())
    Console.withOut(sink)(Console.withErr(sink)(body))
  }

  def check(block: DocBlock, stage: Stage): BlockResult = {
    val outcome = (block.directive, block.output) match {
      case (Directive.Check, Some(expected)) =>
        if (stage == Stage.Parse) strict(attempt(block, stage))
        else
          strict(attempt(block, stage)) match {
            case Outcome.Ok => execute(block, expected)
            case other      => other
          }
      case (_, Some(_)) =>
        Outcome.Invalid(
          "an output fence can only follow a block that is checked normally"
        )
      case _ => checkOnly(block, stage)
    }
    BlockResult(block, outcome)
  }

  private def checkOnly(block: DocBlock, stage: Stage): Outcome =
    block.directive match {
      case Directive.Skip(reason) => Outcome.Skipped(reason)
      case Directive.ParseOnly    => strict(attempt(block, Stage.Parse))
      case Directive.Check        => strict(attempt(block, stage))

      case Directive.ExpectError =>
        attempt(block, stage) match {
          case Left(detail) => Outcome.Crashed(detail)
          case Right(problems) if problems.exists(_.inContext) =>
            Outcome.Failed(problems.filter(_.inContext))
          case Right(problems) if problems.isEmpty => Outcome.UnexpectedlyClean
          case Right(problems) =>
            val expected = ErrorComments.annotate(block, problems)
            if (expected == block.code) Outcome.Ok
            else Outcome.StaleErrorComments(expected, problems)
        }

      case Directive.Ignore(reason) =>
        attempt(block, stage) match {
          case Right(problems) if problems.isEmpty => Outcome.IgnoredButClean
          case _                                   => Outcome.Ignored(reason)
        }
    }

  /** Runs a block that compiled cleanly and compares what it printed with its
    * `output` fence. Trailing newlines are not significant.
    */
  private def execute(block: DocBlock, expected: OutputFence): Outcome = {
    val buffer = new java.io.ByteArrayOutputStream()
    val sink = new java.io.PrintStream(java.io.OutputStream.nullOutputStream())
    val ran =
      try {
        val compilation = quietly(compile(block, combine(block, Stage.Bind)))
        val result = Console.withOut(buffer)(Console.withErr(sink) {
          compilation.exec()
        })
        result match {
          case InterpretResult.RuntimeError =>
            Left("runtime error")
          case InterpretResult.CompileError =>
            Left("compile error")
          case InterpretResult.Exit(code) if code != 0 =>
            Left(s"exited with code $code")
          case _ => Right(())
        }
      } catch {
        case error: Throwable => Left(describeThrowable(error))
      }
    val printed = buffer.toString("utf-8").replace("\r\n", "\n")
    val actual = printed.reverse.dropWhile(_ == '\n').reverse
    ran match {
      case Left(detail) =>
        Outcome.RunFailed(
          if (actual.isEmpty) detail else s"$detail after printing:\n$actual"
        )
      case Right(_) if actual == expected.text => Outcome.Ok
      case Right(_)                            => Outcome.StaleOutput(actual)
    }
  }

  private def describeThrowable(error: Throwable): String =
    Option(error.getMessage) match {
      case Some(message) => s"${error.getClass.getName}: $message"
      case None          => error.getClass.getName
    }

  private def strict(attempt: Either[String, Seq[BlockProblem]]): Outcome =
    attempt match {
      case Left(detail)                        => Outcome.Crashed(detail)
      case Right(problems) if problems.isEmpty => Outcome.Ok
      case Right(problems)                     => Outcome.Failed(problems)
    }

  /** The block's diagnostics, or what the compiler threw instead. */
  private def attempt(
      block: DocBlock,
      stage: Stage
  ): Either[String, Seq[BlockProblem]] =
    try {
      val combined = combine(block, stage)
      Right(
        problemsOf(block, combined, quietly(diagnose(block, combined, stage)))
      )
    } catch {
      case error: Throwable => Left(describeThrowable(error))
    }

  private def diagnose(
      block: DocBlock,
      combined: Combined,
      stage: Stage
  ): Diagnostics = {
    val source = new SourceFile(combined.code, block.file)
    val tree = MakeSyntaxTree.parseSourceFile(source, settings)

    // Binding a tree that did not parse produces cascading nonsense, so stop
    // at the first stage that has something to say.
    val parseFailed = tree.diagnostics match {
      case Diagnostics.Empty => false
      case _                 => true
    }

    stage match {
      case Stage.Parse               => tree.diagnostics
      case Stage.Bind if parseFailed => tree.diagnostics
      case Stage.Bind                => compile(block, combined).diagnostics
    }
  }

  private def compile(block: DocBlock, combined: Combined): Compilation = {
    val source = new SourceFile(combined.code, block.file)
    val tree = MakeSyntaxTree.parseSourceFile(source, settings)
    Stdlib.withSources(ListModule.one(tree), settings) match {
      case Result.Success(trees) => MakeCompilation.create(trees, settings)
      case Result.Error(message) => throw new IllegalStateException(message)
    }
  }
}
