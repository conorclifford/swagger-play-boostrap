package swaggerboot.codegen

import swaggerboot.{ModelAttribute, ModelDefinition}

object JsonOps {
  def generate(packageName: String, definitions: Seq[ModelDefinition]): String = {
      s"""package $packageName
          |
          |import play.api.libs.json._
          |import play.api.libs.functional.syntax._
          |
          |object JsonOps {
          |  ${definitions.map(playJsonImpl).mkString("  \n")}
          |}
       """.stripMargin
  }

  //
  // Json reads/writes generated in semi-verbose manner here intentionally as this gives direct visibility into what is
  // being genated/serialised, and also allows for easier manipulation of generated code (for whatever reason, even if this
  // should really not be necessary...
  //

  def playJsonImpl(definition: ModelDefinition) = {

    def generateRead(attribute: ModelAttribute, forceOptional: Boolean = false): String = {
      val nullable = !attribute.required || forceOptional
      val lazily = definition.cyclicReferences.exists(_.contains(attribute.scalaType))
      val jodaDate = "org.joda.time.DateTime" == attribute.scalaType

      (nullable, lazily, jodaDate) match {
        case (false, false, false) => generatePlainRead(attribute)
        case (true,  false, false) => generatePlainReadNullable(attribute)
        case (false, true,  false) => generateLazyRead(attribute)
        case (true,  true,  false) => generateLazyReadNullable(attribute)
        case (false, false, true ) => generatePlainReadJodaDate(attribute)
        case (true,  false, true ) => generatePlainReadNullableJodaDate(attribute)
        case (false, true,  true ) => sys.error("Should never need to process writing a DateTime lazily")
        case (true,  true,  true ) => sys.error("Should never need to process writing a DateTime lazily")
      }
    }

    def generatePlainRead(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").read[${attribute.scalaType}]"""
    def generatePlainReadNullable(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").readNullable[${attribute.scalaType}]"""

    def generateLazyRead(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyRead(reads${attribute.scalaType})"""
    def generateLazyReadNullable(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyReadNullable(reads${attribute.scalaType})"""

    def generatePlainReadJodaDate(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").read[String].map(new org.joda.time.DateTime(_))"""
    def generatePlainReadNullableJodaDate(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").readNullable[String].map(_.map(new org.joda.time.DateTime(_)))"""

    def generateWrite(attribute: ModelAttribute, forceOptional: Boolean = false): String = {
      val nullable = !attribute.required || forceOptional
      val lazily = definition.cyclicReferences.exists(_.contains(attribute.scalaType))
      val jodaDate = "org.joda.time.DateTime" == attribute.scalaType

      (nullable, lazily, jodaDate) match {
        case (false, false, false) => generatePlainWrite(attribute)
        case (true,  false, false) => generatePlainWriteNullable(attribute)
        case (false, true,  false) => generateLazyWrite(attribute)
        case (true,  true,  false) => generateLazyWriteNullable(attribute)
        case (false, false, true ) => generatePlainWriteJodaDate(attribute)
        case (true,  false, true ) => generatePlainWriteNullableJodaDate(attribute)
        case (false, true,  true ) => sys.error("Should never need to process writing a DateTime lazily")
        case (true,  true,  true ) => sys.error("Should never need to process writing a DateTime lazily")
      }
    }

    def generatePlainWrite(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").write[${attribute.scalaType}]"""
    def generatePlainWriteNullable(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").writeNullable[${attribute.scalaType}]"""
    def generateLazyWrite(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyWrite(writes${attribute.scalaType})"""
    def generateLazyWriteNullable(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyWriteNullable(writes${attribute.scalaType})"""

    def generatePlainWriteJodaDate(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").write[String].contramap[org.joda.time.DateTime](_.toString)"""
    def generatePlainWriteNullableJodaDate(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").writeNullable[String].contramap[Option[org.joda.time.DateTime]](_.map(_.toString))"""

    val playJsonDeclType = if (definition.cyclicReferences.isEmpty) "val" else "def"
    val attributes = definition.attributes
    val name = definition.name
    val scalaName = toScalaName(name)

    val scalaPatchJsonImpl = if (!definition.supportPatch) {
      ""
    } else {
      // Note, Patch object have every attribute as Optional...
      s"""
         |  implicit $playJsonDeclType readsPatch$name: Reads[Patch$scalaName] = (
         |    ${attributes.map(generateRead(_, forceOptional = true)).mkString(" and\n    ")}
         |  )(Patch$scalaName)
         |
         |  implicit $playJsonDeclType writesPatch$name: Writes[Patch$scalaName] = (
         |    ${attributes.map(generateWrite(_, forceOptional = true)).mkString(" and\n    ")}
         |  )(unlift(Patch$scalaName.unapply))
     """.stripMargin
    }

    s"""
       |  implicit $playJsonDeclType reads$name: Reads[$scalaName] = (
       |    ${attributes.map(generateRead(_)).mkString(" and\n    ")}
       |  )($scalaName)
       |
       |  implicit $playJsonDeclType writes$name: Writes[$scalaName] = (
       |    ${attributes.map(generateWrite(_)).mkString(" and\n    ")}
       |  )(unlift($scalaName.unapply))${scalaPatchJsonImpl}
     """.stripMargin
  }
}
