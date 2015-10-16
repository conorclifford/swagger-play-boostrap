package swaggerboot.codegen.server

object HealthcheckController {
  def generate(): String = {
    s"""
       |package controllers
       |
       |import play.api.mvc.{Action, Controller}
       |
       |class HealthCheck extends Controller {
       |
       |  def ping = Action {
       |    Ok
       |  }
       |
       |}
     """.stripMargin
  }
}
