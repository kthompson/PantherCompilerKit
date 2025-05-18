lazy val transpile =
  taskKey[Unit]("Transpile Scala source code to Panther source code")

transpile := {

  val log = streams.value.log
  val sourceFiles = (Compile / sources).value.map(_.getAbsolutePath)
  val mainCls = (Compile / mainClass).value.getOrElse(sys.error("No main class found"))
  val cp = (Runtime / fullClasspath).value.files.mkString(java.io.File.pathSeparator)

  val pncTargetDir = (LocalProject("pnc") / baseDirectory).value.getAbsolutePath
  log.info(s"Output directory: $pncTargetDir")


  log.info(s"Running $mainCls with ${sourceFiles.length} source files")

  val result = Fork.java.fork(
    ForkOptions().withWorkingDirectory(baseDirectory.value),
    Seq("-cp", cp, mainCls, "-t", pncTargetDir) ++ sourceFiles
  ).exitValue()

  if (result != 0) sys.error(s"Transpile failed with exit code $result")
}