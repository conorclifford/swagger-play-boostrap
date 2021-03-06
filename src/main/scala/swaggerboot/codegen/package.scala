package swaggerboot

package object codegen {
  val ReservedNames = Seq(
    "abstract", "case", "catch", "class", "def", "do", "else", "extends",
    "false", "final", "finally", "for", "forSome", "if", "implicit",
    "import", "lazy", "match", "new", "null", "object", "override", "package",
    "private", "protected", "return", "sealed", "super", "this", "throw",
    "trait", "try", "true", "type", "val", "var", "while", "with", "yield"
  )

  def toScalaName(n: String) = {
    if (ReservedNames contains n) {
      s"`$n`"
    } else {
      n
    }
  }

  def toCamelScalaName(n: String) = toScalaName(camelCaseOf(n.replace("-", "_")))

  def toPascalCaseScalaName(n: String) = toScalaName(pascalCaseOf(n.replace("-", "_")))

  def camelCaseOf(name: String): String = lowerFirst(pascalCaseOf(name))

  def pascalCaseOf(name: String): String = name.split("_").map { s => s.head.toString.toUpperCase ++ s.tail}.mkString("")

  def lowerFirst(str: String): String = str.take(1).toLowerCase ++ str.tail
}
