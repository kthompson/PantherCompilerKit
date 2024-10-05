import mill._
import os.RelPath
import scalalib._

trait PantherCompilerKitModule extends ScalaModule {
  def scalaVersion = "3.3.3"

  override def scalacOptions: T[Seq[String]] = Seq("-deprecation", "-Xfatal-warnings")
}

object runtime extends PantherCompilerKitModule {

}

/**
 * Panther Compiler in Scala
 *
 * Compiles to PVM bytecode
 */
object pncs extends PantherCompilerKitModule {
  override def mainClass: T[Option[String]] = Some("Program")

  override def moduleDeps: Seq[JavaModule] = Seq(runtime, metadata)

  def transpileOutputPath = T.source(pnc.millSourcePath / "src")

  def transpileSources = T {
    super
      .sources()
      .map(_.path)
      .flatMap(os.list(_))
      .filterNot(_.endsWith(RelPath("SyntaxVisitor.scala")))
      .toList
  }

  def transpile = T {
    run(T.task {
      println("transpile0: " + transpileOutputPath().path.toString)
      Args(Seq("-t" :: transpileOutputPath().path.toString() :: transpileSources().map(_.toString)))
    })()

    println("transpile1: " + transpileOutputPath().path.toString)
    transpileOutputPath()
  }
}

/**
 * Panther Compiler in Panther
 */
object pnc extends TaskModule with Module {

  def sources = pncs.transpile.map(path =>
    os.walk(path.path).filter(_.ext == "pn").map(_.toString).toList)

  def compile(): Command[Unit] = T.command {
    pncs.run(T.task {
      Args(Seq(T.dest.toString() :: sources()))
    })()
  }

  override def defaultCommandName() = "compile"
}

/**
 * Panther IR shared code in Scala
 */
object metadata extends PantherCompilerKitModule {
  override def moduleDeps: Seq[JavaModule] = Seq(runtime)
}

/**
 * Panther Interpreter in Scala
 *
 * Runs PVM bytecode
 */
object `panthers` extends PantherCompilerKitModule {
  override def moduleDeps: Seq[JavaModule] = Seq(runtime, metadata)
}

///**
// * Panther Interpreter in Panther
// */
//object `panther` extends Module {
//
//}