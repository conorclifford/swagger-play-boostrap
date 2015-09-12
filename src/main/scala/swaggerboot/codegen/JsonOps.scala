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

    def generateExplicit(ftype: String,
                         attribute: ModelAttribute,
                         forceOptional: Boolean = false): String = {
      val fcall = if (definition.cyclicReferences.exists(_.contains(attribute.scalaType))) {
        val fn = if (!forceOptional && attribute.required) s"lazy$ftype" else s"lazy${ftype}Nullable"
        s"$fn(${ftype.toLowerCase}s${attribute.scalaType})"
      } else {
        val fn = if (!forceOptional && attribute.required) s"${ftype.toLowerCase}" else s"${ftype.toLowerCase}Nullable"
        s"$fn[${attribute.scalaType}]"
      }
      s"""(__ \\ "${attribute.name}").$fcall"""
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
         |    ${attributes.map(generateExplicit("Read", _, forceOptional = true)).mkString(" and\n    ")}
         |  )(Patch$scalaName)
         |
         |  implicit $playJsonDeclType writesPatch$name: Writes[Patch$scalaName] = (
         |    ${attributes.map(generateExplicit("Write", _, forceOptional = true)).mkString(" and\n    ")}
         |  )(unlift(Patch$scalaName.unapply))
     """.stripMargin
    }

    s"""
       |  implicit $playJsonDeclType reads$name: Reads[$scalaName] = (
       |    ${attributes.map(generateExplicit("Read", _)).mkString(" and\n    ")}
       |  )($scalaName)
       |
       |  implicit $playJsonDeclType writes$name: Writes[$scalaName] = (
       |    ${attributes.map(generateExplicit("Write", _)).mkString(" and\n    ")}
       |  )(unlift($scalaName.unapply))${scalaPatchJsonImpl}
     """.stripMargin
  }
}
