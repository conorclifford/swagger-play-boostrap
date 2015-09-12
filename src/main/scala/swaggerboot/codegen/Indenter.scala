package swaggerboot.codegen

object Indenter {
  def indent(text: String): String = text.split("\n").map(s => s"  $s").mkString("\n")
}
