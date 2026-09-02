// The compiler's runtime defines root-package Option/Either that would otherwise
// shadow the Scala ones; explicit imports outrank package members.
import scala.{Either, Left, Option, Right}

import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

case class DocCheckOptions(
    roots: Seq[Path],
    stage: Stage,
    baseline: Option[Path],
    updateBaseline: Boolean,
    showPassing: Boolean,
    showKnown: Boolean,
    maxProblemsPerBlock: Int
)

/** Checks that every `panther` code block in the documentation is accepted by
  * the compiler.
  *
  * The docs are the language's public contract, and nothing has been enforcing
  * that the snippets in them are valid Panther. This walks the markdown,
  * extracts the blocks, and pushes each one through the real front end.
  *
  * Most of the docs predate the current language, so
  * `tools/doccheck/baseline.txt` records the blocks that are known not to
  * compile and is picked up automatically. A normal run therefore reports only
  * what has changed.
  *
  * Usage:
  * {{{
  * sbt "doccheck/run docs/src/content/docs"
  * sbt "doccheck/run --no-baseline docs/src/content/docs"
  * }}}
  */
object DocCheck {

  private val Usage =
    """Usage: doccheck [options] <path>...
      |
      |Scans markdown files for ```panther code blocks and checks each one
      |against the compiler. Paths may be files or directories.
      |
      |Most of the docs predate the current language, so known failures are
      |tracked in tools/doccheck/baseline.txt. That file is used automatically
      |when it exists: only new failures are reported.
      |
      |Options:
      |  --stage <parse|bind>     How far to take each block (default: bind)
      |  --baseline <file>        Use <file> instead of the default baseline
      |  --no-baseline            Ignore the baseline and report every failure
      |  --update-baseline        Rewrite the baseline from this run's failures
      |  --show-known             Also detail failures already in the baseline
      |  --show-passing           List blocks that passed as well as those that failed
      |  --max-problems <count>   Diagnostics to show per block (default: 5, 0 for all)
      |  -h, --help               Show this message
      |
      |Per-block directives go in an HTML comment on the line before the fence:
      |  <!-- panther-check: parse-only -->
      |  <!-- panther-check: expect-error -->
      |  <!-- panther-check: skip reason="..." -->
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    parseArgs(args.toSeq) match {
      case Left(error) =>
        println(s"error: $error")
        println()
        print(Usage)
        sys.exit(2)

      case Right(None) =>
        print(Usage)

      case Right(Some(options)) =>
        sys.exit(run(options))
    }
  }

  private def parseArgs(
      args: Seq[String]
  ): Either[String, Option[DocCheckOptions]] = {
    val roots = ArrayBuffer[Path]()
    var stage: Stage = Stage.Bind
    var baseline: Option[Path] = None
    var noBaseline = false
    var updateBaseline = false
    var showPassing = false
    var showKnown = false
    var maxProblems = 5
    var help = args.isEmpty
    var error: Option[String] = None

    var index = 0
    while (index < args.length && error.isEmpty) {
      def value(flag: String): Option[String] =
        if (index + 1 < args.length) Some(args(index + 1))
        else {
          error = Some(s"$flag requires a value")
          None
        }

      args(index) match {
        case "-h" | "--help" =>
          help = true
          index += 1
        case "--show-passing" =>
          showPassing = true
          index += 1
        case "--show-known" =>
          showKnown = true
          index += 1
        case "--no-baseline" =>
          noBaseline = true
          index += 1
        case "--update-baseline" =>
          updateBaseline = true
          index += 1
        case flag @ "--stage" =>
          value(flag).foreach { name =>
            Stage.parse(name) match {
              case Some(parsed) => stage = parsed
              case None         => error = Some(s"unknown stage '$name'")
            }
          }
          index += 2
        case flag @ "--baseline" =>
          value(flag).foreach(path => baseline = Some(Paths.get(path)))
          index += 2
        case flag @ "--max-problems" =>
          value(flag).foreach { count =>
            count.toIntOption match {
              case Some(parsed) if parsed >= 0 => maxProblems = parsed
              case _ =>
                error = Some(s"--max-problems expects a count, got '$count'")
            }
          }
          index += 2
        case flag if flag.startsWith("-") =>
          error = Some(s"unknown option '$flag'")
        case path =>
          roots += Paths.get(path)
          index += 1
      }
    }

    val resolved =
      if (noBaseline) None else baseline.orElse(defaultBaseline)

    error match {
      case Some(message) => Left(message)
      case None if help  => Right(None)
      case None if roots.isEmpty =>
        Left("no paths given")
      case None if noBaseline && baseline.isDefined =>
        Left("--no-baseline and --baseline are mutually exclusive")
      case None if noBaseline && updateBaseline =>
        Left("--no-baseline and --update-baseline are mutually exclusive")
      case None if updateBaseline && resolved.isEmpty =>
        Left(
          "--update-baseline needs a baseline path: pass --baseline, or run from the repository root"
        )
      case None =>
        Right(
          Some(
            DocCheckOptions(
              roots = roots.toSeq,
              stage = stage,
              baseline = resolved,
              updateBaseline = updateBaseline,
              showPassing = showPassing,
              showKnown = showKnown,
              maxProblemsPerBlock = maxProblems
            )
          )
        )
    }
  }

  /** The repo's baseline, used unless the caller says otherwise.
    *
    * Without this the bare `doccheck <path>` invocation reports all 157 known
    * doc failures and exits non-zero, which reads as the tool being broken.
    * Resolved relative to the working directory, and only when it exists, so
    * running the tool elsewhere still behaves sensibly.
    */
  private def defaultBaseline: Option[Path] = {
    val path = Paths.get("tools/doccheck/baseline.txt")
    if (Files.isRegularFile(path)) Some(path) else None
  }

  private def markdownFiles(root: Path): Seq[Path] =
    if (Files.isRegularFile(root)) Seq(root)
    else if (!Files.isDirectory(root)) Seq.empty
    else {
      val stream = Files.walk(root)
      try
        stream
          .iterator()
          .asScala
          .filter(Files.isRegularFile(_))
          .filter { path =>
            val name = path.getFileName.toString
            name.endsWith(".md") || name.endsWith(".mdx")
          }
          .toSeq
          .sortBy(_.toString)
      finally stream.close()
    }

  private def readBaseline(path: Path): Set[String] =
    if (!Files.exists(path)) Set.empty
    else
      Files
        .readAllLines(path)
        .asScala
        .map(_.trim)
        .filter(line => line.nonEmpty && !line.startsWith("#"))
        .toSet

  private def writeBaseline(path: Path, ids: Seq[String]): Unit = {
    val header = Seq(
      "# Documentation blocks that do not yet compile.",
      "# Regenerate with: sbt \"doccheck/run --update-baseline docs/src/content/docs\"",
      "# This list is a ratchet: it should only ever get shorter. It is",
      "# currently empty -- every panther block in docs/ compiles.",
      ""
    )
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.write(path, (header ++ sortIds(ids)).asJava)
  }

  /** Sorts by file then by block number, so `#2` lands before `#11` and the
    * baseline diffs readably as blocks are fixed.
    */
  private def sortIds(ids: Seq[String]): Seq[String] =
    ids.sortBy { id =>
      id.lastIndexOf('#') match {
        case -1 => (id, 0)
        case at =>
          (id.substring(0, at), id.substring(at + 1).toIntOption.getOrElse(0))
      }
    }

  /** Repository-relative path where possible, so block ids stay stable between
    * machines and readable in the baseline. Files outside the working directory
    * keep their absolute path rather than a wall of `../`.
    */
  private def displayPath(cwd: Path, file: Path): String = {
    val relative = cwd.relativize(file.toAbsolutePath.normalize).toString
    if (relative.startsWith("..")) file.toAbsolutePath.normalize.toString
    else relative
  }

  private def describe(
      result: BlockResult,
      maxProblems: Int
  ): Seq[String] = {
    val block = result.block
    result.outcome match {
      case Outcome.Ok => Seq(s"  ok       ${block.id}")

      case Outcome.Skipped(reason) =>
        Seq(s"  skipped  ${block.id}  ($reason)")

      case Outcome.UnexpectedlyClean =>
        Seq(
          s"  FAIL     ${block.id}  (line ${block.codeLine})",
          "    expected a compile error but the block was accepted"
        )

      case Outcome.Crashed(detail) =>
        Seq(
          s"  CRASH    ${block.id}  (line ${block.codeLine})",
          s"    the compiler threw instead of reporting a diagnostic: $detail"
        )

      case Outcome.Failed(problems) =>
        val shown =
          if (maxProblems == 0) problems else problems.take(maxProblems)
        val lines = ArrayBuffer[String]()
        lines += s"  FAIL     ${block.id}  (line ${block.codeLine})"
        shown.foreach(problem => lines += s"    ${problem.render(block)}")
        val hidden = problems.length - shown.length
        if (hidden > 0) lines += s"    ... and $hidden more"
        lines.toSeq
    }
  }

  private def run(options: DocCheckOptions): Int = {
    val cwd = Paths.get("").toAbsolutePath
    val problems = ArrayBuffer[String]()
    val results = ArrayBuffer[BlockResult]()
    var fileCount = 0

    for {
      root <- options.roots
      file <- markdownFiles(root)
    } {
      fileCount += 1
      val text = Files.readString(file)
      val blocks = MarkdownScanner.scan(displayPath(cwd, file), text, problems)
      blocks.foreach(block =>
        results += BlockChecker.check(block, options.stage)
      )
    }

    val skipped = results.count(isSkipped)
    val failures = results.filter(_.failed).toSeq
    val passing = results.length - failures.length - skipped
    val failingIds = failures.map(_.block.id)

    // Most of the docs predate the current language, so without a baseline
    // every run is a wall of expected failures. `known` is what we already
    // accept; everything else is what the reader actually needs to see.
    val known = options.baseline match {
      case Some(path) if !options.updateBaseline => readBaseline(path)
      case _                                     => Set.empty[String]
    }
    val regressions = failures.filterNot(r => known.contains(r.block.id))
    val fixed = sortIds(known.diff(failingIds.toSet).toSeq)

    println(s"panther doc check  (stage: ${options.stage})")
    println(s"  $fileCount files, ${results.length} panther blocks")
    println()

    val detailed =
      if (options.showPassing) results.toSeq
      else if (options.showKnown)
        results.toSeq.filter(r => r.failed || isSkipped(r))
      else regressions ++ results.toSeq.filter(isSkipped)

    detailed.foreach { result =>
      describe(result, options.maxProblemsPerBlock).foreach(println)
    }
    if (detailed.nonEmpty) println()

    problems.foreach(problem => println(s"  MALFORMED  $problem"))
    if (problems.nonEmpty) println()

    println(
      s"  ${results.length} blocks: $passing ok, ${failures.length} failing, $skipped skipped"
    )

    options.baseline match {
      case Some(path) if options.updateBaseline =>
        writeBaseline(path, failingIds)
        println(s"  wrote ${failingIds.length} entries to $path")
        if (problems.isEmpty) 0 else 1

      case Some(path) =>
        val knownFailing = failures.length - regressions.length
        if (knownFailing > 0 && !options.showKnown) {
          println(
            s"  $knownFailing of those are known failures from $path (--show-known to list them)"
          )
        }

        if (fixed.nonEmpty) {
          println()
          println(s"  ${plural(fixed.length, "baselined block")} now passing:")
          fixed.foreach(id => println(s"    $id"))
          println(s"  drop them from $path with --update-baseline")
        }
        if (regressions.nonEmpty) {
          println()
          println(s"  ${plural(regressions.length, "block")} newly failing:")
          regressions.foreach(result => println(s"    ${result.block.id}"))
        }
        if (regressions.isEmpty && fixed.isEmpty && problems.isEmpty) {
          println("  no new failures")
        }

        if (regressions.nonEmpty || fixed.nonEmpty || problems.nonEmpty) 1
        else 0

      case None =>
        if (failures.nonEmpty || problems.nonEmpty) 1 else 0
    }
  }

  private def plural(count: Int, noun: String): String =
    if (count == 1) s"$count $noun" else s"$count ${noun}s"

  private def isSkipped(result: BlockResult): Boolean =
    result.outcome match {
      case Outcome.Skipped(_) => true
      case _                  => false
    }
}
