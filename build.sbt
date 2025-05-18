import sbt.internal.inc.Analysis

ThisBuild / scalaVersion := "3.3.3"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-Xfatal-warnings",
  "-feature",
  "-unchecked"
)

lazy val transpile =
  taskKey[Unit]("Transpile Scala source code to Panther source code")

/** Panther Standard Library */
lazy val runtime = project

/** Metadata library for reading/writing metadata */
lazy val metadata = project
  .dependsOn(runtime)

/** Panther text processing library */
lazy val text = project
  .dependsOn(runtime)

/** Panther Compiler in Scala
  *
  * Compiles to PVM bytecode
  */
lazy val pncs = project
  .dependsOn(runtime, metadata, text, panthers)
  .settings(
    mainClass := Some("Program$"),
    transpile := {
      val log = streams.value.log
      val sourceFiles = (Compile / sources).value.map(_.getAbsolutePath)
      val mainCls =
        (Compile / mainClass).value.getOrElse(sys.error("No main class found"))
      val cp =
        (Runtime / fullClasspath).value.files
          .mkString(java.io.File.pathSeparator)

      val pncTargetDir =
        ((LocalProject("pnc") / baseDirectory).value / "src")

      log.info(s"Output directory: ${pncTargetDir.getAbsolutePath}")

      log.info(s"Running $mainCls with ${sourceFiles.length} source files")

      val result = Fork.java
        .fork(
          ForkOptions().withWorkingDirectory(baseDirectory.value),
          Seq("-cp", cp, mainCls, "-t", pncTargetDir.getAbsolutePath) ++
            sourceFiles
        )
        .exitValue()

      if (result != 0) sys.error(s"Transpile failed with exit code $result")
    }
  )

/** Panther Compiler in Panther
  *
  * Compiles to PVM bytecode
  *
  * TODO: add compile logic for pnc(probably run pncs using .pn sources)
  */
lazy val pnc = project
  .dependsOn(runtime, metadata, text, panthers, pncs)
  .settings(
    // Prevent normal Scala compilation
    Compile / sources := Seq.empty,
    Compile / unmanagedSourceDirectories := Seq.empty,

    // Define .pn source directory
    sourceDirectory := baseDirectory.value / "src",

    // Custom compile task
    Compile / compile := Def.taskDyn {
      (pncs / transpile).map { _ =>
        val log = streams.value.log
        val sourceDir = sourceDirectory.value
        val pnFiles = (sourceDir ** "*.pn").get
        val cp = (pncs / Runtime / fullClasspath).value.files
          .mkString(java.io.File.pathSeparator)
        val mainCls = (pncs / Compile / mainClass).value
          .getOrElse(sys.error("No main class in pncs"))

        if (pnFiles.isEmpty) {
          log.warn("No .pn files found to compile.")
        } else {
          val outDir = target.value.getAbsolutePath

          log.info(
            s"Compiling ${pnFiles.size} .pn files to $outDir using $mainCls"
          )

          val result = Fork.java
            .fork(
              ForkOptions().withWorkingDirectory(baseDirectory.value),
              Seq("-cp", cp, mainCls, outDir) ++ pnFiles.map(_.getAbsolutePath)
            )
            .exitValue()

          if (result != 0) sys.error(s"pncs failed with exit code $result")
        }

        // Fake Analysis to satisfy SBT
        Analysis.Empty
      }
    }.value
  )

/** Panther Interpreter in Scala
  *
  * Executes PVM bytecode
  */
lazy val panthers = project
  .dependsOn(runtime, metadata, text)

lazy val test = project
  .dependsOn(runtime, pncs, panthers)
