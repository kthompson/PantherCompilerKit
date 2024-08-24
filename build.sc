import mill._, scalalib._

trait PantherCompilerKitModule extends ScalaModule {
  def scalaVersion = "2.13.11"

  override def scalacOptions: T[Seq[String]] = Seq("-deprecation", "-Xfatal-warnings")
}

object runtime extends PantherCompilerKitModule {

}

/**
 * Panther Compiler in Scala
 */
object pncs extends PantherCompilerKitModule {
  override def moduleDeps: Seq[JavaModule] = Seq(runtime)
}

/**
 * Panther Compiler in Panther
 */
object pnc extends Module {

}

/**
 * Panther Interpreter in Scala
 */
object `panther-bootstrap` extends PantherCompilerKitModule {
  override def moduleDeps: Seq[JavaModule] = Seq(runtime)
}

/**
 * Panther Interpreter in Panther
 */
object `panther` extends Module {

}