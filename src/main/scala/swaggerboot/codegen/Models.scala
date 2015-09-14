package swaggerboot.codegen

import swaggerboot.{ModelAttribute, ModelDefinition}

object Models {
  def generate(packageName: String, definitions: Seq[ModelDefinition]): String = {
    s"""package $packageName
        |
        |${definitions.map(generateCaseClass).mkString("\n")}
         """.stripMargin
  }

  private def generateCaseClass(definition: ModelDefinition): String = {
    val scalaName = toScalaName(definition.name)

    def attributeLine(attr: ModelAttribute): String = attr match {
      case ModelAttribute(name, _, scalaType, false, _, None) =>
        s"${toScalaName(name)}: Option[$scalaType] = None"
      case ModelAttribute(name, _, scalaType, true, _, None)  =>
        s"${toScalaName(name)}: $scalaType"
      case ModelAttribute(name, _, scalaType, false, _, Some(enumValue)) =>
        s"${toScalaName(name)}: Option[${Enums.fqn(definition.name, attr.name)}] = None"
      case ModelAttribute(name, _, scalaType, true, _, Some(enumValue)) =>
        s"${toScalaName(name)}: ${Enums.fqn(definition.name, attr.name)}"
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
