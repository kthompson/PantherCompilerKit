import mill._, scalalib._

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

  def topvm: Target[PathRef] = T {
    run(T.task {
      val sources = super
        .sources()
        .map(_.path)
        .flatMap(os.list(_))
        .map(_.toString())
        .filterNot(_.endsWith("SyntaxVisitor.scala"))

      Args(Seq(T.dest.toString() :: sources.toList))
    })()

    PathRef(T.dest)
  }

  def transpile(): Command[PathRef] = T.command {
    run(T.task {
      val sources = super
        .sources()
        .map(_.path)
        .flatMap(os.list(_))
        .map(_.toString())
        .filterNot(_.endsWith("SyntaxVisitor.scala"))

      //      sources.foreach(println)

      Args(Seq("-t" :: T.dest.toString() :: sources.toList))
    })()

    PathRef(T.dest)
  }
}

///**
// * Panther Compiler in Panther
// */
//object pnc extends TaskModule with RunModule {
//
////  def sources = T(pncs.transpile())
//
////  def allSourceFiles: T[Seq[PathRef]] = T.sources {
////    Seq(PathRef(pncs.transpile()().path))
////  }
//
//  override def mainClass = T { Some("Program") }
//
//  def compileArgs = pncs.transpile().zip(T{T.dest}).map { case (transpileOutput, dest) =>
//    val args = os.list(transpileOutput.path)
//      .map(_.toString())
//      .toList
//    println("tacooooossss")
//    println(transpileOutput.path)
//    println(dest.toString())
//    args.foreach(println)
//    println("tacooooossss")
//
//    Args(Seq(dest.toString() :: args))
//  }
//
//
//  // compile pnc with pncs
//  def compile = T {
//    pncs.run(compileArgs)()
//
//    PathRef(T.dest)
//  }
//
//  override def defaultCommandName() = "run"
//}

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