package swaggerboot.codegen

import swaggerboot.{Controller, ModelDefinition, ModeledEnum}

object Enums {

  def generate(packageName: String, definitions: Seq[ModelDefinition], controllers: Seq[Controller], clientSide: Boolean): String = {
    val definitionImpls = for {
      definition <- definitions
      attribute <- definition.attributes
      modeledEnum <- attribute.modeledEnum.toSeq
    } yield generate(definition.name, attribute.name, modeledEnum, clientSide)

    val controllerImpls = for {
      controller  <- controllers
      method      <- controller.methods
      param       <- (method.params ++ method.headerParams)
      modeledEnum <- param.modeledEnum.toSeq
    } yield generate(controller.name, param.name, modeledEnum, false)

    val enumImpls = (definitionImpls ++ controllerImpls).toSet

    s"""package $packageName
       |
       |import scalaz._
       |import Scalaz._
       |
       |trait EnumError {
       |  def message: String
       |}
       |case class BadValue(message: String) extends EnumError
       |
       |sealed trait NamedEnum {
       |  def name: String
       |  override def toString(): String = name
       |}
       |
       |${enumImpls.mkString("\n")}
     """.stripMargin
  }

  def enumObjectNames(definitions: Seq[ModelDefinition], controllers: Seq[Controller]): Set[String] = {
    val definitionNames = for {
      definition <- definitions
      attribute <- definition.attributes if attribute.modeledEnum.nonEmpty
    } yield wrappingObjectName(definition.name, attribute.name)

    val controllerNames = for {
      controller <- controllers
      method <- controller.methods
      param <- method.params if param.modeledEnum.nonEmpty
    } yield wrappingObjectName(controller.name, param.name)

    (definitionNames ++ controllerNames).toSet
  }

  private def generate(definitionName: String, propertyName: String, modeledEnum: ModeledEnum, allowUnknown: Boolean): String = {
    val traitName = sealedTraitName(definitionName, propertyName)

    def instanceCaseObject(name: String) = s"""case object ${munge(name)} extends $traitName { override val name = "$name" }"""
    val unknownValueDefinitionApply = if (allowUnknown) s"""case class UnknownValue(value: String) extends $traitName { override val name = s"UnknownValue($$value)" }""" else ""

    val unknownCase = if (allowUnknown) {
      s"case _ => UnknownValue(name).right"
    } else {
      s"""case _ => BadValue(s"Unknown value '$$name' for '$propertyName' enum").left"""
    }

    def applyCaseEntry(name: String) = s"""case ${munge(name)}.name => ${munge(name)}.right"""

    s"""
       |object ${wrappingObjectName(definitionName, propertyName)} {
       |  sealed trait $traitName extends NamedEnum
       |
       |  ${modeledEnum.values.map(instanceCaseObject).mkString("\n  ")}
       |  $unknownValueDefinitionApply
       |
       |  def apply(name: String): EnumError \\/ $traitName = name match {
       |    ${modeledEnum.values.map(applyCaseEntry).mkString("\n    ")}
       |    $unknownCase
       |  }
       |}
     """.stripMargin
  }

  def wrappingObjectName(definitionName: String, propertyName: String): String = munge(s"${sealedTraitName(definitionName, propertyName)}Enum")

  private def sealedTraitName(definitionName: String, propertyName: String): String = munge(definitionName ++ (propertyName.head.toUpper +: propertyName.tail))

  def fqn(definitionName: String, propertyName: String): String = s"${wrappingObjectName(definitionName, propertyName)}.${sealedTraitName(definitionName, propertyName)}"

  def fqValue(definitionName: String, propertyName: String, value: String): String = s"${wrappingObjectName(definitionName, propertyName)}.${munge(value)}"

  private def munge(name: String) = pascalCaseOf(name.replaceAll("-", "_").replaceAll(" ", "_").replaceAll("[^a-zA-Z0-9_]", ""))
}
