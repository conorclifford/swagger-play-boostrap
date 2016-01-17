package swaggerboot.codegen.server

import swaggerboot.SwaggerCodeGenerator.Config
import swaggerboot._
import swaggerboot.codegen._
import swaggerboot.Id

object Controller {
  def generate(controller: Controller)(implicit config: Config, ids: Map[String, Id]): String = {
    val name = controller.name
    val methods = controller.methods

    val methodToDelegate = methods.map { method =>
      method -> method.delegate.getOrElse(swaggerboot.Delegates.getDefault(controller, method))
    }.toMap

    val injectableParamList = if (config.generateDelegates) {
      s"@Inject() (${delegateInjectionList(controller)})"
    } else {
      ""
    }

    s"""
       |//
       |// This is generated code.
       |//
       |package controllers
       |
       |import play.api._
       |import play.api.mvc._
       |
       |import com.google.inject.Inject
       |
       |import api.models._
       |import api.ids
       |import api.models.JsonOps._
       |
       |class ${toScalaName(name)} $injectableParamList extends Controller {
       |  ${methods.map(method => scalaImpl(name, method, methodToDelegate(method))).mkString("")}
       |}
     """.stripMargin
  }

  private def valName(className: String): String = toScalaName(className.head.toLower +: className.tail)

  private def delegateInjectionList(controller: Controller)(implicit config: Config): String = {
    if (config.generateDelegates) {
      val delegates = swaggerboot.Delegates.extract(Seq(controller))
      delegates.map { delegate =>
        s"${valName(delegate.className)}: _root_.${ControllerDelegateTraits.PackageName}.${delegate.className}"
      }.mkString(", ")
    } else {
      ""
    }
  }

  private def scalaImpl(controllerName: String, method: Method, delegate: MethodDelegate)(implicit config: Config, ids: Map[String, Id]): String = {
    val name = method.name
    val params = method.params
    val headerParams = method.headerParams

    // FIXME use content-type specific parsing!!
    val parsing = method.body.fold("")(_ => "(parse.tolerantJson)")

    val dispatchLogic = if (config.generateDelegates) {
      s"${valName(delegate.className)}.${toScalaName(name)}(${paramNameList(params)})"
    } else {
      s"??? // FIXME"
    }

    s"""
        |  def ${toScalaName(name)}(${paramSigs(controllerName, params)}) = Action.async$parsing { implicit request =>
        |    ${if(headerParams.nonEmpty) headerParams.map(paramSig(controllerName, _)).mkString("// header: ", "\n    // header: ", "\n") else ""}
        |    $dispatchLogic
        |  }
   """.stripMargin
  }

  def paramNameList(params: Seq[Param]): String = params.map { param =>
    // FIXME should only lowercase the first char?
    toScalaName(param.name.toLowerCase)
  }.mkString(", ")

  def paramSigs(controllerName: String, params: Seq[Param])(implicit ids: Map[String, Id]): String = params.map(paramSig(controllerName, _)).mkString(", ")

  def paramType(controllerName: String, param: Param)(implicit ids: Map[String, Id]) = {
    (ids.get(param.name), param.modeledEnum) match {
      case (Some(id), _)    => s"ids.${id.name}"
      case (_, Some(menum)) => Enums.fqn(controllerName, param.name)
      case (_, None)        => param.baseType
    }
  }

  private def paramSig(controllerName: String, param: Param)(implicit ids: Map[String, Id]) = {
    val required = param.required
    val paramName = param.paramType match {
      case HeaderParam => param.name // Keep header params exactly as per API - only used in comments for now anyway.
      case _ => toScalaName(param.name.toLowerCase) // FIXME - should only lowercase the first char?
    }

    val baseType = paramType(controllerName, param)
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
