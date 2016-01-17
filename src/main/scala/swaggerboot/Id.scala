package swaggerboot

case class Id(tag: String, name: String, scalaType: String)

object Id {
  private val AppropriateTypes = Set("String", "Long", "Int")
  private val AppropriateSeqTypes = AppropriateTypes.map(t => s"Seq[$t]")

  def apply(attrName: String, scalaType: String): Option[Id] = {
    import swaggerboot.codegen._
    val name = toPascalCaseScalaName(attrName)
    if (attrName.endsWith("_id") && AppropriateTypes.contains(scalaType)) {
      val tag = toPascalCaseScalaName(attrName.replaceAll("_id$", ""))
      Option(Id(tag, name, scalaType))
    } else if (attrName.endsWith("_ids") && AppropriateSeqTypes.contains(scalaType)) {
      val tag = toPascalCaseScalaName(attrName.replaceAll("_ids$", ""))
      Option(Id(tag, name.dropRight(1), scalaType.drop(4).dropRight(1)))
    } else {
      None
    }
  }
}
