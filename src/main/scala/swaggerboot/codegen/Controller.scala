package swaggerboot.codegen

import swaggerboot.{Param, Method}

object Controller {
  def generate(controller: swaggerboot.Controller): String = {
    val name = controller.name
    val methods = controller.methods

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
       |  ${methods.map(scalaImpl).mkString("")}
       |}
     """.stripMargin
  }

  def scalaImpl(method: Method): String = {
    val name = method.name
    val params = method.params
    val headerParams = method.headerParams
    val producesCommentary = {
      if (method.produces.nonEmpty) {
        s"""
           |    /* Produces (one of):
           |      - ${method.produces.mkString("\n      - ")}
           |     */""".stripMargin
      } else {
        ""
      }
    }

    method.body match {
      case None =>
        s"""
            |  def $name(${params.map(paramSig).mkString(", ")}) = Action {
            |    ${if(headerParams.nonEmpty) headerParams.map(paramSig).mkString("// header-param: ", "\n    //", "\n") else ""}${producesCommentary}
            |    InternalServerError("not implemented") // FIXME needs implementation
            |  }
       """.stripMargin

      case Some(body) =>
        val bodyModelName = if (method.httpMethod == "PATCH") s"Patch${body.typeName}" else body.typeName
        s"""
           |  def $name(${params.map(paramSig).mkString(", ")}) = Action(parse.json) { request =>
           |    ${if(headerParams.nonEmpty) headerParams.map(paramSig).mkString("// header-param: ", "\n    //", "\n") else ""}${producesCommentary}
           |    def ${name}OnValid(body: $bodyModelName) = {
           |      InternalServerError("not implemented") // FIXME needs implementation
           |    }
           |
           |    request.body.validate[$bodyModelName].fold(
           |      valid = ${name}OnValid,
           |      invalid = (e => BadRequest(e.toString))
           |    )
           |  }
       """.stripMargin
    }
  }

  private def paramSig(param: Param) = {
    val required = param.required
    val paramName = param.name.toLowerCase
    val baseType = param.baseType
    val typeName = if (required) baseType else s"Option[$baseType]"

    (param.required, param.defaultValue) match {
      case (true, _) =>
        s"$paramName: $typeName"
      case (false, None) =>
        s"$paramName: $typeName = None"
      case (_, Some(_)) =>
        // The default value is injected through the routes entry, not here.
        s"$paramName: $baseType"
    }
  }
}
