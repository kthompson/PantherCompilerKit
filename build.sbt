ThisBuild / scalaVersion := "3.3.3"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-Xfatal-warnings",
  "-feature",
  "-unchecked"
)

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
    mainClass := Some("Program$")
  )

/** Panther Compiler in Panther
  *
  * Compiles to PVM bytecode
  *
  * TODO: add compile logic for pnc(probably run pncs using .pn sources)
  */
lazy val pnc = project
  .dependsOn(runtime, metadata, text, panthers)

/** Panther Interpreter in Scala
  *
  * Executes PVM bytecode
  */
lazy val panthers = project
  .dependsOn(runtime, metadata, text)

lazy val test = project
  .dependsOn(runtime, pncs, panthers)
