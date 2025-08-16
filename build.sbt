import sbt.internal.inc.Analysis

ThisBuild / scalaVersion := "3.3.6"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-Xfatal-warnings",
  "-feature",
  "-unchecked"
)

lazy val transpile =
  taskKey[Unit]("Transpile Scala source code to Panther source code")

lazy val bootstrap = taskKey[Unit]("Bootstrap the project")

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
  * Compiles to PVM bytecode, Executes PVM bytecode
  */
lazy val pncs = project
  .dependsOn(runtime, metadata, text)
  .settings(
    mainClass := Some("Program$"),
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.4" % Test,
    testFrameworks += new TestFramework("utest.runner.Framework"),
    transpile := {
      val log = streams.value.log
      val sourceFiles = ((Compile / sources).value.map(_.getAbsolutePath) ++
        (runtime / Compile / sources).value.map(_.getAbsolutePath) ++
        (metadata / Compile / sources).value.map(_.getAbsolutePath) ++
        (text / Compile / sources).value.map(_.getAbsolutePath))
        .filterNot(_.endsWith("panther.scala"))

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
    },
    bootstrap := {
      val log = streams.value.log
      val sourceFiles = ((Compile / sources).value.map(_.getAbsolutePath) ++
        (runtime / Compile / sources).value.map(_.getAbsolutePath) ++
        (metadata / Compile / sources).value.map(_.getAbsolutePath) ++
        (text / Compile / sources).value.map(_.getAbsolutePath))
        .filterNot(_.endsWith("panther.scala"))

      val mainCls =
        (Compile / mainClass).value.getOrElse(sys.error("No main class found"))
      val cp =
        (Runtime / fullClasspath).value.files
          .mkString(java.io.File.pathSeparator)

      val outputFile = target.value / (name.value + ".pnb")

      log.info(s"Output directory: ${outputFile.getAbsolutePath}")
      log.info(s"name: ${name.value}")

      log.info(s"Running $mainCls with ${sourceFiles.length} source files")

      val result = Fork.java
        .fork(
          ForkOptions().withWorkingDirectory(baseDirectory.value),
          Seq("-cp", cp, mainCls, outputFile.getAbsolutePath) ++
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
  .dependsOn(runtime, metadata, text, pncs)
  .settings(
    // Prevent normal Scala compilation
    Compile / sources := Seq.empty,
    Compile / unmanagedSourceDirectories := Seq.empty,

    // Define .pn source directory
    sourceDirectory := baseDirectory.value / "src",

    // Custom compile task
    Compile / compile := Def.taskDyn {
      val cp = (pncs / Runtime / fullClasspath).value.files
        .mkString(java.io.File.pathSeparator)
      val log = streams.value.log

      (pncs / transpile).map { _ =>
        val sourceDir = sourceDirectory.value
        val pnFiles = (sourceDir ** "*.pn").get
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

lazy val test = project
  .dependsOn(runtime, pncs)
  .settings(
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.4" % Test,
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
