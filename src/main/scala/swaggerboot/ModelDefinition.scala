package swaggerboot

case class ModelDefinition(name: String, attributes: Seq[ModelAttribute], supportPatch: Boolean) {
  lazy val scalaClassImpl =
    s"""
       |case class $name(
       |  ${attributes.map(mainClassAttribute).mkString(",\n  ")}
       |)
       |${scalaPatchClassImpl.getOrElse("")}
     """.stripMargin

  private def mainClassAttribute(attr: ModelAttribute): String = attr match {
    case ModelAttribute(name, scalaType, false) => s"$name: Option[$scalaType]"
    case ModelAttribute(name, scalaType, true) => s"$name: $scalaType"
  }

  lazy val scalaPatchClassImpl: Option[String] = if (!supportPatch) {
    None
  } else {
    Some(
      s"""case class Patch$name(
         |  ${attributes.map { case ModelAttribute(name, scalaType, _) => s"$name: Option[$scalaType]" }.mkString(",\n  ")}
         |)
       """.stripMargin
    )
  }

  lazy val playJsonImpl =
    s"""
       |  implicit val reads$name = Json.reads[$name]
       |  implicit val writes$name = Json.writes[$name]
       |  ${scalaPatchJsonImpl.getOrElse("")}
     """.stripMargin

  lazy val scalaPatchJsonImpl: Option[String] = if (!supportPatch) {
    None
  } else {
    Some(
      s"""implicit val readsPatch$name = Json.reads[Patch$name]
         |  implicit val writesPatch$name = Json.writes[Patch$name]
       """.stripMargin
    )
  }
}
