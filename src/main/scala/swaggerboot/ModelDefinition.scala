package swaggerboot

case class ModelDefinition(name: String, attributes: Seq[ModelAttribute], supportPatch: Boolean, cyclicReferences: Option[Set[String]] = None) {
  private val ReservedNames = Seq(
    "abstract", "case", "catch", "class", "def", "do", "else", "extends",
    "false", "final", "finally", "for", "forSome", "if", "implicit",
    "import", "lazy", "match", "new", "null", "object", "override", "package",
    "private", "protected", "return", "sealed", "super", "this", "throw",
    "trait", "try", "true", "type", "val", "var", "while", "with", "yield"
  )

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

  //
  // Json reads/writes generated in semi-verbose manner here intentionally as this gives direct visibility into what is
  // being genated/serialised, and also allows for easier manipulation of generated code (for whatever reason, even if this
  // should really not be necessary...
  //

  private lazy val playJsonDeclType = if (cyclicReferences.isEmpty) "val" else "def"
  lazy val playJsonImpl =
    s"""
       |  implicit $playJsonDeclType reads$name: Reads[$scalaName] = (
       |    ${attributes.map(generateExplicit("Read", _)).mkString(" and\n    ")}
       |  )($scalaName)
       |
       |  implicit $playJsonDeclType writes$name: Writes[$scalaName] = (
       |    ${attributes.map(generateExplicit("Write", _)).mkString(" and\n    ")}
       |  )(unlift($scalaName.unapply))
       |  ${scalaPatchJsonImpl.getOrElse("")}
     """.stripMargin

  private def generateExplicit(ftype: String, attribute: ModelAttribute, forceOptional: Boolean = false): String = {
    val fcall = if (cyclicReferences.exists(_.contains(attribute.scalaType))) {
      val fn = if (!forceOptional && attribute.required) s"lazy$ftype" else s"lazy${ftype}Nullable"
      s"$fn(${ftype.toLowerCase}s${attribute.scalaType})"
    } else {
      val fn = if (!forceOptional && attribute.required) s"${ftype.toLowerCase}" else s"${ftype.toLowerCase}Nullable"
      s"$fn[${attribute.scalaType}]"
    }
    s"""(__ \\ "${attribute.name}").$fcall"""
  }

  lazy val scalaPatchJsonImpl: Option[String] = if (!supportPatch) {
    None
  } else {
    // Note, Patch object have every attribute as Optional...
    Some(
      s"""
         |  implicit $playJsonDeclType readsPatch$name: Reads[Patch$scalaName] = (
         |    ${attributes.map(generateExplicit("Read", _, forceOptional = true)).mkString(" and\n    ")}
         |  )(Patch$scalaName)
         |
         |  implicit $playJsonDeclType writesPatch$name: Writes[Patch$scalaName] = (
         |    ${attributes.map(generateExplicit("Write", _, forceOptional = true)).mkString(" and\n    ")}
         |  )(unlift(Patch$scalaName.unapply))
     """.stripMargin
    )
  }
}
