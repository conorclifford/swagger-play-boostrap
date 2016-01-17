package swaggerboot.codegen.server

import swaggerboot.SwaggerCodeGenerator.Config
import swaggerboot.Id
import swaggerboot.codegen.Enums
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
    s"${method.httpMethod} \t$path \tcontrollers.$controllerName.${method.name}(${method.params.map(playRouteParam(controllerName, _)).mkString(", ")})"
  }

  def paramType(controllerName: String, param: Param)(implicit ids: Map[String, Id]) = {
    (ids.get(param.name), param.modeledEnum) match {
      case (Some(id), _)    => s"ids.${id.name}"
      case (_, Some(menum)) => Enums.fqn(controllerName, param.name)
      case (_, None)        => param.baseType
    }
  }

  private def playRouteParam(controllerName: String, param: Param)(implicit ids: Map[String, Id]) = {
    def defaultValueToString(defval: String): String = (param.baseType, param.modeledEnum) match {
      case ("String", None)        => s""""$defval""""
      case ("String", Some(menum)) => Enums.fqValue(controllerName, param.name, defval)
      case _                       => defval
    }

    param.defaultValue.fold {
      s"${paramName(param)}: ${typeName(controllerName, param)}"
    } { defval =>
      s"${paramName(param)}: ${paramType(controllerName, param)} ?= ${defaultValueToString(defval)}"
    }
  }

  def typeName(controllerName: String, param: Param)(implicit ids: Map[String, Id]) = {
    val ptype = paramType(controllerName, param)
    if (param.required) ptype else s"Option[$ptype]"
  }

  def paramName(param: Param) = param.name.toLowerCase
}
