package swaggerboot.codegen

import io.swagger.models.Swagger
import swaggerboot.swaggerops._

object Ids {
  def generatePackageObject(parentPackageName: String, swagger: Swagger): String = {
    val ids = swagger.ids

    s"""
       |package $parentPackageName
       |
       |package object ids {
       |  ${ids.values.toSeq.distinct.map(id => s"type ${id.name} = Id[${id.scalaType}, ids.${id.tag}]").mkString("\n  ")}
       |}
       |""".stripMargin
  }

  def generate(packageName: String, swagger: Swagger): String = {
    val ids = swagger.ids

    s"""
       |package $packageName.ids
       |
       |case class Id[V, T](value: V) extends AnyVal {
       |  override def toString = value.toString
       |  def convert[O](f: V => O): O = f(value)
       |  def as[T2]: Id[V, T2] = convert(Id[V, T2])
       |}
       |
       |${ids.values.toSeq.distinct.map(id => s"trait ${id.tag}").mkString("\n")}
       |
       |""".stripMargin
  }
}
