import panther.{assert => _, *}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Paths}

class StdlibTests extends AnyFunSpec with Matchers {
  private def compile(
      source: string,
      settings: CompilerSettings = CompilerSettingsFactory.default
  ): Compilation = {
    val user = MakeSyntaxTree.parseContent(source, settings)
    val trees = Stdlib.withSources(ListModule.one(user), settings) match {
      case Result.Success(trees) => trees
      case Result.Error(message) => fail(message)
    }
    MakeCompilation.create(trees, settings)
  }

  private val program =
    Files.readString(Paths.get("test/fixtures/stdlib/user.pn"))

  describe("user standard library") {
    it("binds and executes the bundled library without filesystem lookup") {
      val compilation = compile(program)
      withClue(TestHelpers.diagnosticMessages(compilation).mkString("\n")) {
        compilation.diagnostics.count() shouldBe 0
      }
      compilation.exec() shouldBe InterpretResult.Exit(7)
    }

    it("includes library bodies in standalone emitted images") {
      val compilation = compile(program)
      withClue(TestHelpers.diagnosticMessages(compilation).mkString("\n")) {
        compilation.diagnostics.count() shouldBe 0
      }
      val image = Files.createTempFile("panther-stdlib", ".pnb")
      try {
        compilation.emit(image.toString)
        ImageRunner.exec(
          image.toString,
          CompilerSettingsFactory.default
        ) shouldBe InterpretResult.Exit(7)
      } finally Files.deleteIfExists(image)
    }

    it("can disable library inclusion for isolated compilation") {
      compile(
        "val x = Option.Some(1)",
        CompilerSettingsFactory.default.copy(useStdlib = false)
      ).diagnostics.count() should be > 0
      compile(
        "enum Option[T] { case Some(value: T) }",
        CompilerSettingsFactory.default.copy(useStdlib = false)
      ).diagnostics.count() shouldBe 0
    }

    it("reports conflicting user declarations") {
      compile("enum Option[T] { case Some(value: T) }").diagnostics
        .count() should be > 0
    }

    it("loads the replacement Panther source bundle") {
      val settings = CompilerSettingsFactory.default.copy(stdlibDirectory =
        Paths.get("stdlib/src").toAbsolutePath.toString
      )
      val compilation = compile(program, settings)
      withClue(TestHelpers.diagnosticMessages(compilation).mkString("\n")) {
        compilation.diagnostics.count() shouldBe 0
      }
      compilation.exec() shouldBe InterpretResult.Exit(7)
    }

    it("reports the missing file in an incomplete replacement bundle") {
      val directory = Files.createTempDirectory("panther-missing-stdlib")
      try {
        Stdlib.load(
          CompilerSettingsFactory.default
            .copy(stdlibDirectory = directory.toString)
        ) shouldBe
          Result.Error(
            "Standard library source not found: " + directory.resolve(
              "Dictionary.pn"
            )
          )
      } finally Files.deleteIfExists(directory)
    }

    it("continues execution after checking existing and missing source files") {
      val compilation = compile("""
        assert(File.exists("stdlib/src/Option.pn"), "existing source")
        assert(!File.exists("stdlib/src/missing.pn"), "missing source")
        exit(7)
      """)
      compilation.diagnostics.count() shouldBe 0
      compilation.exec() shouldBe InterpretResult.Exit(7)
    }

    it("does not load the library during transpilation") {
      Stdlib.load(
        CompilerSettingsFactory.default
          .copy(transpile = true, stdlibDirectory = "missing")
      ) shouldBe Result.Success(List.Nil)
    }
  }
}
