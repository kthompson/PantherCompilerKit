import panther._

object system {
  object io {
    object File {
      def write_all_text(file: string, text: string): unit = {
        val writer = new java.io.PrintWriter(file)
        writer.write(text)
        writer.close()
      }

      def read_all_text(file: string): string = {
        val source = scala.io.Source.fromFile(file)
        val lines = try source.mkString finally source.close()
        lines
      }
    }

    object Path {
      // TODO: support path separators other than '/'
      val separator = '/'

      def combine(path1: string, path2: string): string = {
        if (path1 == "") path2
        else if (path2 == "") path1
        else if (path1(path1.length - 1) == separator) path1 + path2
        else path1 + separator + path2
      }

      def nameWithoutExtension(path: string): string = {
        val lastSeparator = path.lastIndexOf(separator)
        val lastDot = path.lastIndexOf('.')
        if (lastDot < lastSeparator) path
        else path.substring(lastSeparator + 1, lastDot)
      }

      def directoryName(path: string): string = {
        val lastSeparator = path.lastIndexOf(separator)
        if (lastSeparator == -1) ""
        else path.substring(0, lastSeparator)
      }


    }
  }
}

object panther {
  type bool = scala.Boolean
  type char = scala.Char
  type int = scala.Int
  type string = java.lang.String
  type unit = scala.Unit
  type any = scala.Any
  type nothing = scala.Nothing
  
  def string(any: Any): string = any.toString
  def char(any: Any): char = any match {
    case c: char => c
    case i: int => i.toChar
    case _ => throw new Exception("not a char")
  }
  def int(any: Any): int = any match {
    case i: int => i
    case c: char => c.toInt
    case s: string => s.toInt
    case _ => throw new Exception("not an int")
  }

  def panic(message: string): nothing = throw new Exception(message)

  // scala to panther translate
  // * replace Array[ ] with Array< >
  // * replace Option[ ] with Option< >
  // * remove "`" from wrapped identifiers
  // * remove `case` from case class
  // * drop annotations
  // * import ns._ => using ns
  // * need to convert array item access from array(n) => array[n]
  // * remove override keywords
}