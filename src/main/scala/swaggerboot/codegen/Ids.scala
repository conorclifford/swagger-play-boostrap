package swaggerboot.codegen

import swaggerboot.SwaggerRep

object Ids {
  def generatePackageObject(providedIdClass: Option[String], parentPackageName: String, swagger: SwaggerRep): String = {
    val ids = swagger.ids
    val idClassName = providedIdClass.getOrElse("Id")

    s"""
       |package $parentPackageName
       |
       |package object ids {
       |  ${ids.values.toSeq.distinct.map(id => s"type ${id.name} = $idClassName[${id.scalaType}, ids.${id.tag}]").mkString("\n  ")}
       |}
       |""".stripMargin
  }

  def generate(providedIdClass: Option[String], packageName: String, swagger: SwaggerRep): String = {
    val ids = swagger.ids

    val idClass = if (providedIdClass.nonEmpty) {
      s"""case class Id[V, T](value: V) extends AnyVal {
         |  override def toString = value.toString
         |  def convert[O](f: V => O): O = f(value)
         |  def as[T2]: Id[V, T2] = convert(Id[V, T2])
         |}
         |""".stripMargin
    } else {
      ""
    }

    s"""
       |package $packageName.ids
       |
       |$idClass
       |${ids.values.toSeq.distinct.map(id => s"trait ${id.tag}").mkString("\n")}
       |
       |""".stripMargin
  }
}
