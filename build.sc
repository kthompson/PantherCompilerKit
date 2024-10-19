import mill._
import os.RelPath
import scalalib._

trait PantherCompilerKitModule extends ScalaModule {
  def scalaVersion = "3.3.3"

  object test extends ScalaTests {
    override def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.8.4")

    def testFramework = "utest.runner.Framework"
  }

  override def scalacOptions: T[Seq[String]] = Seq("-deprecation", "-Xfatal-warnings")
}

object mvu extends PantherCompilerKitModule {
  override def moduleDeps: Seq[JavaModule] = Seq(runtime)
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
    (super
      .sources() ++ metadata.sources())
      .map(_.path)
      .flatMap(os.list(_))
      .filterNot(_.endsWith(RelPath("SyntaxVisitor.scala")))
      .toList
  }

  def transpile = T {
    run(T.task {
      Args(Seq("-t" :: transpileOutputPath().path.toString() :: transpileSources().map(_.toString)))
    })()

    transpileOutputPath()
  }
}

/**
 * Panther Compiler in Panther
 *
 * Compiles to PVM bytecode
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