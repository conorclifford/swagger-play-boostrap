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
}
