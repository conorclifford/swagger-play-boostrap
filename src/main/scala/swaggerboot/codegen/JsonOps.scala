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
      val isEnum = attribute.modeledEnum.nonEmpty

      (nullable, lazily, jodaDate, isEnum) match {
        case (false, false, false, false) => generatePlainRead(attribute)
        case (true,  false, false, false) => generatePlainReadNullable(attribute)
        // Cannot have enum and lazy in same case.
        case (false, true,  false, _) => generateLazyRead(attribute)
        case (true,  true,  false, _) => generateLazyReadNullable(attribute)
        // cannot have JodaDate and either lazy or enum in same case
        case (false, _, true, _ ) => generatePlainReadJodaDate(attribute)
        case (true,  _, true, _ ) => generatePlainReadNullableJodaDate(attribute)
        case (false, _, _, true) => generateReadEnum(attribute)
        case (true, _, _, true) => generateReadNullableEnum(attribute)
      }
    }

    def generatePlainRead(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").read[${attribute.scalaType}]"""
    def generatePlainReadNullable(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").readNullable[${attribute.scalaType}]"""

    def generateLazyRead(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyRead(reads${attribute.scalaType})"""
    def generateLazyReadNullable(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyReadNullable(reads${attribute.scalaType})"""

    def generatePlainReadJodaDate(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").read[String].map(new org.joda.time.DateTime(_))"""
    def generatePlainReadNullableJodaDate(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").readNullable[String].map(_.map(new org.joda.time.DateTime(_)))"""

    def generateReadEnum(attribute: ModelAttribute): String = {
      // Get is assumed to be safe in this case...
      val enumValue = attribute.modeledEnum.get
      s"""(__ \\ "${attribute.name}").read[String].map(${Enums.wrappingObjectName(definition.name, attribute.name)}.apply)"""
    }

    def generateReadNullableEnum(attribute: ModelAttribute): String = {
      // Get is assumed to be safe in this case...
      val enumValue = attribute.modeledEnum.get
      s"""(__ \\ "${attribute.name}").readNullable[String].map(_.map(${Enums.wrappingObjectName(definition.name, attribute.name)}.apply))"""
    }

    def generateWrite(attribute: ModelAttribute, forceOptional: Boolean = false): String = {
      val nullable = !attribute.required || forceOptional
      val lazily = definition.cyclicReferences.exists(_.contains(attribute.scalaType))
      val jodaDate = "org.joda.time.DateTime" == attribute.scalaType
      val isEnum = attribute.modeledEnum.nonEmpty

      (nullable, lazily, jodaDate, isEnum) match {
        case (false, false, false, false) => generatePlainWrite(attribute)
        case (true,  false, false, false) => generatePlainWriteNullable(attribute)
        case (false, true,  false, _) => generateLazyWrite(attribute)
        case (true,  true,  false, _) => generateLazyWriteNullable(attribute)
        case (false, _, true, _ ) => generatePlainWriteJodaDate(attribute)
        case (true,  _, true, _ ) => generatePlainWriteNullableJodaDate(attribute)
        case (false, _, _, true) => generateWriteEnum(attribute)
        case (true, _, _, true) => generateWriteNullableEnum(attribute)
      }
    }

    def generatePlainWrite(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").write[${attribute.scalaType}]"""
    def generatePlainWriteNullable(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").writeNullable[${attribute.scalaType}]"""
    def generateLazyWrite(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyWrite(writes${attribute.scalaType})"""
    def generateLazyWriteNullable(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyWriteNullable(writes${attribute.scalaType})"""

    def generatePlainWriteJodaDate(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").write[String].contramap[org.joda.time.DateTime](_.toString)"""
    def generatePlainWriteNullableJodaDate(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").writeNullable[String].contramap[Option[org.joda.time.DateTime]](_.map(_.toString))"""

    def generateWriteEnum(attribute: ModelAttribute): String = {
      // Get is assumed to be safe in this case...
      val enumValue = attribute.modeledEnum.get
      s"""(__ \\ "${attribute.name}").write[String].contramap[${Enums.fqn(definition.name, attribute.name)}](${Enums.wrappingObjectName(definition.name, attribute.name)}.unapply)"""
    }

    def generateWriteNullableEnum(attribute: ModelAttribute): String = {
      // Get is assumed to be safe in this case...
      val enumValue = attribute.modeledEnum.get
      s"""(__ \\ "${attribute.name}").writeNullable[String].contramap[Option[${Enums.fqn(definition.name, attribute.name)}]](_.map(${Enums.wrappingObjectName(definition.name, attribute.name)}.unapply))"""
    }

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
