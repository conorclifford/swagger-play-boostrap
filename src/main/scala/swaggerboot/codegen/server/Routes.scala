package swaggerboot.codegen.server

import swaggerboot.SwaggerCodeGenerator.Config
import swaggerboot.Id
import swaggerboot.{Method, Param, RoutedController}

object Routes {

  def generate(controllers: Seq[RoutedController])(implicit config: Config, ids: Map[String, Id]): String = {
    val generatedRoutes = controllers.sortBy(_.path).map { routed =>
      val routesFileEntries: Seq[String] = {
        val controller = routed.controller
        controller.methods.map { method =>
          routeFileEntry(method, routed.path, controller.name)
        }
      }
      routesFileEntries.mkString("\n")
    }.mkString("\n\n")

    val healthcheck = if (config.withHealthCheck) {
      s"GET   /health   controllers.HealthCheck.ping() "
    } else {
      ""
    }

    s"""
       |$generatedRoutes
       |
       |$healthcheck
       |
       |GET     /assets/*file   controllers.Assets.at(path="/public", file)
       |GET     /ui  controllers.Default.redirect(to="/assets/lib/swagger-ui/index.html?/url=/assets/swagger.json")
     """.stripMargin
  }

  private def routeFileEntry(method: Method, path: String, controllerName: String)(implicit ids: Map[String, Id]) = {
    s"${method.httpMethod} \t$path \tcontrollers.$controllerName.${method.name}(${method.params.map(playRouteParam).mkString(", ")})"
  }

  def paramType(param: Param)(implicit ids: Map[String, Id]) = {
    ids.get(param.name).map { id =>
      s"ids.${id.name}"
    }.getOrElse {
      param.baseType
    }
  }

  private def playRouteParam(param: Param)(implicit ids: Map[String, Id]) = {
    def defaultValueToString(defval: String): String = param.baseType match {
      case "String" => s""""$defval""""
      case _ => defval
    }

    param.defaultValue.fold {
      s"${paramName(param)}: ${typeName(param)}"
    } { defval =>
      s"${paramName(param)}: ${paramType(param)} ?= ${defaultValueToString(defval)}"
    }
  }

  def typeName(param: Param)(implicit ids: Map[String, Id]) = {
    if (param.required) paramType(param) else s"Option[${paramType(param)}]"
  }

  def paramName(param: Param) = param.name.toLowerCase
}
