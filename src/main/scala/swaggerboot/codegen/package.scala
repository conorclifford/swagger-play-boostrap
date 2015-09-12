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
}