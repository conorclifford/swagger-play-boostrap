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
       |${enumImpls.mkString("\n")}
     """.stripMargin
  }

  private def generate(definitionName: String, propertyName: String, modeledEnum: ModeledEnum, allowUnknown: Boolean): String = {
    val traitName = sealedTraitName(definitionName, propertyName)

    def instanceCaseObject(name: String) = s"case object ${munge(name)} extends $traitName"
    val unknownValueDefinitionApply = if (allowUnknown) s"case class UnknownValue(value: String) extends $traitName" else ""

    def applyCaseEntry(name: String) = s"""case "$name" => ${munge(name)}"""

    def unapplyCaseEntry(name: String) = s"""case ${munge(name)} => "$name""""
    val unknownValueHandlerUnapply = if (allowUnknown) s"""case UnknownValue(_) => throw new IllegalArgumentException("Cannot serialise unknown values for enums")""" else ""

    s"""
       |object ${wrappingObjectName(definitionName, propertyName)} {
       |  sealed trait $traitName
       |
       |  ${modeledEnum.values.map(instanceCaseObject).mkString("\n  ")}
       |  $unknownValueDefinitionApply
       |
       |  def apply(name: String): $traitName = name match {
       |    ${modeledEnum.values.map(applyCaseEntry).mkString("\n    ")}
       |    case x => ${
              if (allowUnknown) {
                "UnknownValue(x)"
              } else {
                s"""throw new IllegalArgumentException(s"Unknown value '$$x' for '$propertyName' enum")"""
              }
            }
       |  }
       |
       |  def unapply(op: $traitName): String = op match {
       |    ${modeledEnum.values.map(unapplyCaseEntry).mkString("\n    ")}
       |    $unknownValueHandlerUnapply
       |  }
       |}
     """.stripMargin
  }

  def wrappingObjectName(definitionName: String, propertyName: String): String = s"${sealedTraitName(definitionName, propertyName)}Enum"

  private def sealedTraitName(definitionName: String, propertyName: String): String = definitionName ++ (propertyName.head.toUpper +: propertyName.tail)

  def fqn(definitionName: String, propertyName: String): String = s"${wrappingObjectName(definitionName, propertyName)}.${sealedTraitName(definitionName, propertyName)}"

  private def munge(name: String) = name.replaceAll(" ", "_").replaceAll("[^a-zA-Z0-9_]", "")
}