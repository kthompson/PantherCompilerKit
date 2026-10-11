import sbt._

/** Embed the canonical library sources in both compiler implementations. */
object StdlibBundle {
  val names = Seq("Dictionary", "Either", "List", "Option", "Result", "Tuple2")

  def generate(root: File, managed: File): Seq[File] = {
    def literal(value: String): String = "\"" + value.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c => c.toString
    } + "\""
    val cases = names.zipWithIndex.map { case (name, index) =>
      s"      case $index => ${literal(IO.read(root / "stdlib" / "src" / (name + ".pn")))}"
    }.mkString("\n")
    val nameCases = names.zipWithIndex.map { case (name, index) =>
      s"      case $index => ${literal(name)}"
    }.mkString("\n")
    val output = managed / "BundledStdlib.scala"
    IO.write(output, s"""import panther._

// Generated from stdlib/src by StdlibBundle. Do not edit.
object BundledStdlib {
  val count: int = ${names.size}
  def name(index: int): string = index match {
$nameCases
      case _ => panic("Invalid stdlib index")
  }
  def source(index: int): string = index match {
$cases
      case _ => panic("Invalid stdlib index")
  }
}
""")
    Seq(output)
  }
}
