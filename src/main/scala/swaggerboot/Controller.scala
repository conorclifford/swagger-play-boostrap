package swaggerboot

case class Controller(name: String, methods: Seq[Method]) {
  lazy val scalaImpl =
    s"""
       |package controllers
       |
       |import play.api._
       |import play.api.mvc._
       |
       |import models._
       |import models.JsonOps._
       |
       |class $name extends Controller {
       |  ${methods.map(_.scalaImpl).mkString("")}
       |}
     """.stripMargin

  lazy val clientTrait =
    s"""trait ${name}Client {
        |  ${methods.map(_.clientSignature).mkString("\n  ")}
        |}
     """.stripMargin

  lazy val clientImpl =
    s"""object ${name}Client extends ${name}Client {
        |${Indenter.indent(methods.map(_.clientMethod).mkString("\n"))}
        |}
     """.stripMargin
}
