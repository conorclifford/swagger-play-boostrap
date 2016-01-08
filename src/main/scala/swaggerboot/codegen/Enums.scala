package swaggerboot.codegen

import swaggerboot.{ModelDefinition, ModeledEnum}

object Enums {

  def generate(packageName: String, definitions: Seq[ModelDefinition], allowUnknown: Boolean): String = {
    val enumImpls: Seq[String] = for {
      definition <- definitions
      attribute <- definition.attributes
      modeledEnum <- attribute.modeledEnum.toSeq
    } yield generate(definition.name, attribute.name, modeledEnum, allowUnknown)

    s"""package $packageName
       |
       |import scalaz._
       |import Scalaz._
       |
       |${enumImpls.mkString("\n")}
     """.stripMargin
  }

  private def generate(definitionName: String, propertyName: String, modeledEnum: ModeledEnum, allowUnknown: Boolean): String = {
    val traitName = sealedTraitName(definitionName, propertyName)

    def instanceCaseObject(name: String) = s"""case object ${munge(name)} extends $traitName { override val name = "$name" }"""
    val unknownValueDefinitionApply = if (allowUnknown) s"""case class UnknownValue(value: String) extends $traitName { override val name = s"UnknownValue($$value)" }""" else ""

    def applyCaseEntry(name: String) = s"""case ${munge(name)}.name => ${munge(name)}.right"""

    s"""
       |object ${wrappingObjectName(definitionName, propertyName)} {
       |  sealed trait $traitName {
       |    def name: String
       |    override def toString(): String = name
       |  }
       |
       |  ${modeledEnum.values.map(instanceCaseObject).mkString("\n  ")}
       |  $unknownValueDefinitionApply
       |
       |  def safeApply(name: String): String \\/ $traitName = name match {
       |    ${modeledEnum.values.map(applyCaseEntry).mkString("\n    ")}
       |    case _ => s"Unknown value '$$name' for '$propertyName' enum".left
       |  }
       |
       |  def apply(name: String): $traitName = safeApply(name) match {
       |    case -\\/(e) => throw new IllegalArgumentException(e)
       |    case \\/-(status) => status
       |  }
       |}
     """.stripMargin
  }

  def wrappingObjectName(definitionName: String, propertyName: String): String = munge(s"${sealedTraitName(definitionName, propertyName)}Enum")

  private def sealedTraitName(definitionName: String, propertyName: String): String = munge(definitionName ++ (propertyName.head.toUpper +: propertyName.tail))

  def fqn(definitionName: String, propertyName: String): String = s"${wrappingObjectName(definitionName, propertyName)}.${sealedTraitName(definitionName, propertyName)}"

  private def munge(name: String) = camelOf(name.replaceAll("-", "_").replaceAll(" ", "_").replaceAll("[^a-zA-Z0-9_]", ""))
}
