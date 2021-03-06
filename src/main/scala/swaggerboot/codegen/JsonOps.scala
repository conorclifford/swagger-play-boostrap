package swaggerboot.codegen

import swaggerboot.{Controller, Id, ModelAttribute, ModelDefinition}

object JsonOps {
  def generate(providedIdClass: Option[String], parentPackageName: String, packageName: String, definitions: Seq[ModelDefinition], controllers: Seq[Controller])
              (implicit ids: Map[String, Id]): String = {
    val idClass = providedIdClass.getOrElse("ids.Id")
    s"""//
        |// This is generated code. Do not edit this file.
        |//
        |
        |package $parentPackageName.$packageName
        |
        |import $parentPackageName.ids
        |
        |import play.api.data.validation.ValidationError
        |import play.api.libs.json._
        |import play.api.libs.functional.syntax._
        |import scalaz._
        |
        |object JsonOps {
        |
        |  implicit def idFormat[V, T](implicit vf: Format[V]) = Format[$idClass[V, T]](Reads(_.validate[V].map($idClass[V, T])), Writes(v => Json.toJson(v.value)))
        |  ${Enums.enumObjectNames(definitions, controllers).map(enumFormatImplicit).mkString("\n  ")}
        |
        |  ${definitions.filterNot(_.attributes.isEmpty).map(playJsonImpl).mkString("\n")}
        |
        |  private def createEnumFormat[T <: NamedEnum](fn: String => EnumError \\/ T): Format[T] = {
        |    new Format[T] {
        |      override def reads(json: JsValue): JsResult[T] = {
        |        json.validate[String].flatMap { x =>
        |          fn(x).fold(
        |            error => JsError(Seq(JsPath() -> Seq(ValidationError(error.message)))),
        |            value => JsSuccess(value)
        |          )
        |        }
        |      }
        |
        |      override def writes(o: T): JsValue = JsString(o.name)
        |    }
        |  }
        |}
     """.stripMargin
  }

  private def enumFormatImplicit(enumObjectName: String): String = {
    s"implicit val ${lowerFirst(enumObjectName)}Format = createEnumFormat($enumObjectName.apply)"
  }

  //
  // Json reads/writes generated in semi-verbose manner here intentionally as this gives direct visibility into what is
  // being generated/serialised, and also allows for easier manipulation of generated code (for whatever reason, even if this
  // should really not be necessary...
  //

  def playJsonImpl(definition: ModelDefinition)(implicit ids: Map[String, Id]) = {

    def possiblyId(name: String, alternative: String): String = {
      val tname = if (name == "id") s"${definition.name}_id" else name
      ids.get(tname).map { id =>
        val tsig = s"ids.${id.name}"
        if (alternative.startsWith("Seq[")) {
          s"Seq[$tsig]"
        } else {
          tsig
        }
      }.getOrElse(alternative)
    }

    def generateRead(attribute: ModelAttribute, forceOptional: Boolean = false): String = {
      val nullable = !attribute.required || forceOptional
      val lazily = definition.cyclicReferences.exists(_.contains(attribute.scalaType))
      val lazySeq = definition.cyclicReferences.exists(_.exists(r => s"Seq[$r]" == attribute.scalaType))
      val jodaDate = "org.joda.time.DateTime" == attribute.scalaType
      val isEnum = attribute.modeledEnum.nonEmpty

      (nullable, lazily || lazySeq, jodaDate, isEnum) match {
        case (false, false, false, false) => generatePlainRead(attribute)
        case (true,  false, false, false) => generatePlainReadNullable(attribute)
        // Cannot have enum and lazy in same case.
        case (false, true,  false, _) if !lazySeq => generateLazyRead(attribute)
        case (true,  true,  false, _) if !lazySeq => generateLazyReadNullable(attribute)
        case (false, true,  false, _) if lazySeq => generateLazySeqRead(possiblyId(attribute.name, attribute.scalaType.drop(4).dropRight(1)), attribute)
        case (true,  true,  false, _) if lazySeq => generateLazySeqReadNullable(possiblyId(attribute.name, attribute.scalaType.drop(4).dropRight(1)), attribute)
        // cannot have JodaDate and either lazy or enum in same case
        case (false, _, true, _ ) => generatePlainReadJodaDate(attribute)
        case (true,  _, true, _ ) => generatePlainReadNullableJodaDate(attribute)
        case (false, _, _, true) => generateReadEnum(attribute)
        case (true, _, _, true) => generateReadNullableEnum(attribute)
      }
    }

    def generatePlainRead(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").read[${possiblyId(attribute.name, attribute.scalaType)}]"""
    def generatePlainReadNullable(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").readNullable[${possiblyId(attribute.name, attribute.scalaType)}]"""

    def generateLazyRead(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyRead(reads${attribute.scalaType})"""
    def generateLazyReadNullable(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyReadNullable(reads${attribute.scalaType})"""

    def generateLazySeqRead(name: String, attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyRead(Reads.seq[$name](reads${name}))"""
    def generateLazySeqReadNullable(name: String, attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyReadNullable(Reads.seq[$name](reads${name}))"""

    def generatePlainReadJodaDate(attribute: ModelAttribute): String = {
      s"""(__ \\ "${attribute.name}").read[String].map(new org.joda.time.DateTime(_))"""
    }
    def generatePlainReadNullableJodaDate(attribute: ModelAttribute): String = {
      s"""(__ \\ "${attribute.name}").readNullable[String].map(_.map(new org.joda.time.DateTime(_)))"""
    }

    def generateReadEnum(attribute: ModelAttribute): String = {
      s"""(__ \\ "${attribute.name}").read[${Enums.fqn(definition.name, attribute.name)}]"""
    }

    def generateReadNullableEnum(attribute: ModelAttribute): String = {
      s"""(__ \\ "${attribute.name}").readNullable[${Enums.fqn(definition.name, attribute.name)}]"""
    }

    def generateWrite(attribute: ModelAttribute, forceOptional: Boolean = false): String = {
      val nullable = !attribute.required || forceOptional
      val lazily = definition.cyclicReferences.exists(_.contains(attribute.scalaType))
      val lazySeq = definition.cyclicReferences.exists(_.exists(r => s"Seq[$r]" == attribute.scalaType))

      val jodaDate = "org.joda.time.DateTime" == attribute.scalaType
      val isEnum = attribute.modeledEnum.nonEmpty

      (nullable, lazily || lazySeq, jodaDate, isEnum) match {
        case (false, false, false, false) => generatePlainWrite(attribute)
        case (true,  false, false, false) => generatePlainWriteNullable(attribute)
        case (false, true,  false, _) if !lazySeq => generateLazyWrite(attribute)
        case (true,  true,  false, _) if !lazySeq => generateLazyWriteNullable(attribute)
        case (false, true,  false, _) if lazySeq => generateLazySeqWrite(possiblyId(attribute.name, attribute.scalaType.drop(4).dropRight(1)), attribute)
        case (true,  true,  false, _) if lazySeq => generateLazySeqWriteNullable(possiblyId(attribute.name, attribute.scalaType.drop(4).dropRight(1)), attribute)
        case (false, _, true, _ ) => generatePlainWriteJodaDate(attribute)
        case (true,  _, true, _ ) => generatePlainWriteNullableJodaDate(attribute)
        case (false, _, _, true) => generateWriteEnum(attribute)
        case (true, _, _, true) => generateWriteNullableEnum(attribute)
      }
    }

    def generatePlainWrite(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").write[${possiblyId(attribute.name, attribute.scalaType)}]"""
    def generatePlainWriteNullable(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").writeNullable[${possiblyId(attribute.name, attribute.scalaType)}]"""

    def generateLazyWrite(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyWrite(writes${attribute.scalaType})"""
    def generateLazyWriteNullable(attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyWriteNullable(writes${attribute.scalaType})"""

    def generateLazySeqWrite(name: String, attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyWrite(Writes.seq[$name](writes$name))"""
    def generateLazySeqWriteNullable(name: String, attribute: ModelAttribute): String = s"""(__ \\ "${attribute.name}").lazyWriteNullable(Writes.seq[$name](writes$name))"""

    def generatePlainWriteJodaDate(attribute: ModelAttribute): String = {
      // Assume date-time otherwise
      val formatFunction = if (attribute.swaggerType.format.exists(_ == "date")) "date" else "dateTime"
      val mapFunction = s"org.joda.time.format.ISODateTimeFormat.$formatFunction().print"
      s"""(__ \\ "${attribute.name}").write[String].contramap[org.joda.time.DateTime]($mapFunction)"""
    }
    def generatePlainWriteNullableJodaDate(attribute: ModelAttribute): String = {
      // Assume date-time otherwise
      val formatFunction = if (attribute.swaggerType.format.exists(_ == "date")) "date" else "dateTime"
      val mapFunction = s"org.joda.time.format.ISODateTimeFormat.$formatFunction().print"
      s"""(__ \\ "${attribute.name}").writeNullable[String].contramap[Option[org.joda.time.DateTime]](_.map($mapFunction))"""
    }

    def generateWriteEnum(attribute: ModelAttribute): String = {
      s"""(__ \\ "${attribute.name}").write[${Enums.fqn(definition.name, attribute.name)}]"""
    }

    def generateWriteNullableEnum(attribute: ModelAttribute): String = {
      s"""(__ \\ "${attribute.name}").writeNullable[${Enums.fqn(definition.name, attribute.name)}]"""
    }

    val playJsonDeclType = if (definition.cyclicReferences.isEmpty) "val" else "def"
    val attributes = definition.attributes
    val name = definition.name
    val scalaName = toScalaName(name)
    val singleAttribute = attributes.size == 1

    def buildImplicits(namePrefix: String, forceOptional: Boolean) = {
      if (singleAttribute) {
        val attr = attributes.head
        s"""
           |  implicit $playJsonDeclType reads$namePrefix$name: Reads[$namePrefix$scalaName] =
           |    ${generateRead(attr, forceOptional = forceOptional)}.map($namePrefix$scalaName(_))
           |
           |  implicit $playJsonDeclType writes$namePrefix$name: Writes[$namePrefix$scalaName] =
           |    ${generateWrite(attr, forceOptional = forceOptional)}.contramap(_.${attr.name})
         """.stripMargin
        } else {
        s"""
           |  implicit $playJsonDeclType reads$namePrefix$name: Reads[$namePrefix$scalaName] = (
           |    ${attributes.map(generateRead(_, forceOptional = forceOptional)).mkString(" and\n    ")}
           |  )($namePrefix$scalaName)
           |
           |  implicit $playJsonDeclType writes$namePrefix$name: Writes[$namePrefix$scalaName] = (
           |    ${attributes.map(generateWrite(_, forceOptional = forceOptional)).mkString(" and\n    ")}
           |  )(unlift($namePrefix$scalaName.unapply))
         """.stripMargin
      }
    }

    val scalaPatchJsonImpl = if (!definition.supportPatch) {
      ""
    } else {
      // Note, Patch object have every attribute as Optional...
      buildImplicits("Patch", true)
    }

    s"""
       |${buildImplicits("", false)}
       |$scalaPatchJsonImpl
     """.stripMargin
  }
}
