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

    def mainClassAttribute(attr: ModelAttribute): String = attr match {
      case ModelAttribute(name, scalaType, false, _) => s"${toScalaName(name)}: Option[$scalaType] = None"
      case ModelAttribute(name, scalaType, true, _) => s"${toScalaName(name)}: $scalaType"
    }

    val scalaPatchClassImpl = if (!definition.supportPatch) {
      ""
    } else {
      s"""
          |case class Patch${definition.name}(
          |  ${
                definition.attributes.map {
                  case ModelAttribute(name, scalaType, _, _) => s"${toScalaName(name)}: Option[$scalaType] = None"
                }.mkString(",\n  ")
              }
          |)
          |""".stripMargin
    }

    s"""
       |case class $scalaName(
       |  ${definition.attributes.map(mainClassAttribute).mkString(",\n  ")}
       |)${scalaPatchClassImpl}
     """.stripMargin
  }
}
