// The compiler's runtime defines root-package Option/Either that would otherwise
// shadow the Scala ones; explicit imports outrank package members.
import scala.{Either, Left, Option, Right}

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
case class BlockProblem(line: Int, column: Int, message: String) {
  def render(block: DocBlock): String =
    s"${block.file}:$line:$column: $message"
}

enum Outcome {

  /** The block behaved as its directive says it should. */
  case Ok

  /** Not checked. */
  case Skipped(reason: String)

  /** The block was rejected by the compiler. */
  case Failed(problems: Seq[BlockProblem])

  /** The compiler threw instead of reporting a diagnostic. Always a compiler
    * bug: errors are supposed to travel through the DiagnosticBag.
    */
  case Crashed(detail: String)

  /** `expect-error` was declared but the block compiled cleanly. */
  case UnexpectedlyClean
}

case class BlockResult(block: DocBlock, outcome: Outcome) {
  val failed: Boolean = outcome match {
    case Outcome.Ok | Outcome.Skipped(_) => false
    case _                               => true
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

  private def problemsOf(
      block: DocBlock,
      diagnostics: Diagnostics
  ): Seq[BlockProblem] = {
    val collected = ArrayBuffer[Diagnostic]()
    collect(diagnostics, collected)
    collected.map { diagnostic =>
      val location = diagnostic.location
      BlockProblem(
        line = block.sourceLine(location.startLine + 1),
        column = location.startCharacter + 1,
        message = diagnostic.message
      )
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
    val outcome = block.directive match {
      case Directive.Skip(reason) => Outcome.Skipped(reason)
      case Directive.ParseOnly   => run(block, Stage.Parse, expectError = false)
      case Directive.ExpectError => run(block, stage, expectError = true)
      case Directive.Check       => run(block, stage, expectError = false)
    }
    BlockResult(block, outcome)
  }

  private def run(
      block: DocBlock,
      stage: Stage,
      expectError: Boolean
  ): Outcome = {
    val attempt =
      try Right(quietly(diagnose(block, stage)))
      catch {
        case error: Throwable =>
          Left(Option(error.getMessage) match {
            case Some(message) => s"${error.getClass.getName}: $message"
            case None          => error.getClass.getName
          })
      }

    attempt match {
      case Left(detail) => Outcome.Crashed(detail)
      case Right(diagnostics) =>
        val problems = problemsOf(block, diagnostics)
        if (expectError) {
          if (problems.isEmpty) Outcome.UnexpectedlyClean else Outcome.Ok
        } else {
          if (problems.isEmpty) Outcome.Ok else Outcome.Failed(problems)
        }
    }
  }

  private def diagnose(block: DocBlock, stage: Stage): Diagnostics = {
    val source = new SourceFile(block.code, block.file)
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
      case Stage.Bind =>
        MakeCompilation.create(ListModule.one(tree), settings).diagnostics
    }
  }
}
