package swaggerboot.codegen

import swaggerboot.swaggerops.Id
import swaggerboot.{ModelAttribute, ModelDefinition}

object Models {
  def generate(parentPackageName: String, packageName: String, definitions: Seq[ModelDefinition])(implicit ids: Map[String, Id]): String = {
    s"""//
        |// This is generated code. Do not edit this file.
        |//
        |package $parentPackageName.$packageName
        |
        |import $parentPackageName.ids
        |
        |${definitions.filterNot(_.attributes.isEmpty).map(generateCaseClass).mkString("\n")}
         """.stripMargin
  }

  private def possiblyId(definition: ModelDefinition, attr: ModelAttribute, alternative: String)(implicit ids: Map[String, Id]): String = {
    val name = attr.name
    val tname = if (name == "id") s"${definition.name}_id" else name
    ids.get(tname).map { id =>
      val tsig = s"ids.${id.name}"
      if (name.endsWith("_ids") && attr.scalaType.startsWith("Seq[")) {
        s"Seq[$tsig]"
      } else {
        tsig
      }
    }.getOrElse(alternative)
  }

  private def generateCaseClass(definition: ModelDefinition)(implicit ids: Map[String, Id]): String = {
    val scalaName = toScalaName(definition.name)

    def attributeLine(attr: ModelAttribute): String = attr match {
      case ModelAttribute(name, _, scalaType, false, _, None) =>
        s"${toCamelScalaName(name)}: Option[${possiblyId(definition, attr, scalaType)}] = None"
      case ModelAttribute(name, _, scalaType, true, _, None)  =>
        s"${toCamelScalaName(name)}: ${possiblyId(definition, attr, scalaType)}"
      case ModelAttribute(name, _, scalaType, false, _, Some(enumValue)) =>
        s"${toCamelScalaName(name)}: Option[${Enums.fqn(definition.name, attr.name)}] = None"
      case ModelAttribute(name, _, scalaType, true, _, Some(enumValue)) =>
        s"${toCamelScalaName(name)}: ${Enums.fqn(definition.name, attr.name)}"
    }

    val scalaPatchClassImpl = if (!definition.supportPatch) {
      ""
    } else {
      s"""
          |case class Patch${definition.name}(
          |  ${
                definition.attributes.map(a => attributeLine(a.copy(required = false))).mkString(",\n  ")
              }
          |)
          |""".stripMargin
    }

    s"""
       |case class $scalaName(
       |  ${definition.attributes.map(attributeLine).mkString(",\n  ")}
       |)${scalaPatchClassImpl}
     """.stripMargin
  }
}
