package swaggerboot

case class ModelDefinition(name: String, attributes: Seq[ModelAttribute], supportPatch: Boolean, cyclicReferences: Option[Set[String]] = None) {
  private val ReservedNames = Seq("type",  "package", "case", "class", "def", "val", "var", "protected", "private", "lazy", "match", "with", "extends", "if", "else", "while", "for")

  lazy val scalaClassImpl =
    s"""
       |case class $scalaName(
       |  ${attributes.map(mainClassAttribute).mkString(",\n  ")}
       |)
       |${scalaPatchClassImpl.getOrElse("")}
     """.stripMargin

  private def mainClassAttribute(attr: ModelAttribute): String = attr match {
    case ModelAttribute(name, scalaType, false, _) => s"${toScalaName(name)}: Option[$scalaType] = None"
    case ModelAttribute(name, scalaType, true, _) => s"${toScalaName(name)}: $scalaType"
  }

  private val scalaName = toScalaName(name)

  private def toScalaName(n: String) = {
    if (ReservedNames contains n) {
      s"`$n`"
    } else {
      n
    }
  }

  lazy val scalaPatchClassImpl: Option[String] = if (!supportPatch) {
    None
  } else {
    Some(
      s"""case class Patch$name(
         |  ${attributes.map { case ModelAttribute(name, scalaType, _, _) => s"${toScalaName(name)}: Option[$scalaType] = None" }.mkString(",\n  ")}
         |)
         |""".stripMargin
    )
  }

  lazy val playJsonImpl = if (cyclicReferences.isEmpty) {
    s"""
     |  implicit val reads$name = Json.reads[$scalaName]
     |  implicit val writes$name = Json.writes[$scalaName]
     |  ${scalaPatchJsonImpl.getOrElse("")}
     |""".
      stripMargin
    } else {
    s"""
       |  implicit val reads$name: Reads[$scalaName] = (
       |    ${attributes.map(generateExplicit("Read", _)).mkString(" and\n    ")}
       |  )($scalaName)
       |
       |  implicit val writes$name: Writes[$scalaName] = (
       |    ${attributes.map(generateExplicit("Write", _)).mkString(" and\n    ")}
       |  )(unlift($scalaName.unapply))
     """.stripMargin
  }

  private def generateExplicit(ftype: String, attribute: ModelAttribute): String = {
    val fcall = if (cyclicReferences.exists(_.contains(attribute.scalaType))) {
      val fn = if (attribute.required) s"lazy$ftype" else s"lazy${ftype}Nullable"
      s"$fn(${ftype.toLowerCase}s${attribute.scalaType})"
    } else {
      val fn = if (attribute.required) s"${ftype.toLowerCase}" else s"${ftype.toLowerCase}Nullable"
      s"$fn[${attribute.scalaType}]"
    }
    s"""(__ \\ "${attribute.name}").$fcall"""
  }

  lazy val scalaPatchJsonImpl: Option[String] = if (!supportPatch) {
    None
  } else if (cyclicReferences.isEmpty) {
    Some(
      s"""
       |  implicit val readsPatch$name = Json.reads[Patch$scalaName]
       |  implicit val writesPatch$name = Json.writes[Patch$scalaName]
       |""".stripMargin
    )
  } else {
    Some(
      s"""
         |  implicit val readsPatch$name: Reads[Patch$scalaName] = (
         |    ${attributes.map(generateExplicit("Read", _)).mkString(" and\n    ")}
         |  )($scalaName)
         |
         |  implicit val writesPatch$name: WritesPatch[$scalaName] = (
         |    ${attributes.map(generateExplicit("Write", _)).mkString(" and\n    ")}
         |  )(unlift($scalaName.unapply))
     """.stripMargin
    )
  }
}
