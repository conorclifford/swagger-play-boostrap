package swaggerboot.codegen

import swaggerboot._

object Controller {
  def generate(controller: Controller): String = {
    val name = controller.name
    val methods = controller.methods

    val methodToDelegate = methods.map { method =>
      method -> method.delegate.getOrElse(swaggerboot.Delegates.getDefault(controller, method))
    }.toMap

    s"""
       |package controllers
       |
       |import play.api._
       |import play.api.mvc._
       |
       |import com.google.inject.Inject
       |
       |import models._
       |import models.JsonOps._
       |
       |class ${toScalaName(name)} @Inject() (${delegateInjectionList(controller)}) extends Controller {
       |  ${methods.map(method => scalaImpl(method, methodToDelegate(method))).mkString("")}
       |}
     """.stripMargin
  }

  private def valName(className: String): String = toScalaName(className.head.toLower +: className.tail)

  private def delegateInjectionList(controller: Controller): String = {
    val delegates = swaggerboot.Delegates.extract(Seq(controller))
    delegates.map { delegate =>
      s"${valName(delegate.className)}: _root_.${ControllerDelegateTraits.PackageName}.${delegate.className}"
    }.mkString(", ")
  }

  private def scalaImpl(method: Method, delegate: MethodDelegate): String = {
    val name = method.name
    val params = method.params
    val headerParams = method.headerParams

    // FIXME use content-type specific parsing!!
    val parsing = method.body.fold("")(_ => "(parse.tolerantJson)")

    s"""
        |  def ${toScalaName(name)}(${paramSigs(params)}) = Action.async$parsing { implicit request =>
        |    ${if(headerParams.nonEmpty) headerParams.map(paramSig).mkString("// header: ", "\n    // header: ", "\n") else ""}
        |    ${valName(delegate.className)}.${toScalaName(name)}(${paramNameList(params)})
        |  }
   """.stripMargin
  }

  def paramNameList(params: Seq[Param
    ]): String = params.map { param =>
    // FIXME should only lowercase the first char?
    toScalaName(param.name.toLowerCase)
  }.mkString(", ")

  def paramSigs(params: Seq[Param]): String = params.map(paramSig).mkString(", ")

  private def paramSig(param: Param) = {
    val required = param.required
    val paramName = param.paramType match {
      case HeaderParam => param.name // Keep header params exactly as per API - only used in comments for now anyway.
      case _ => toScalaName(param.name.toLowerCase) // FIXME - should only lowercase the first char?
    }

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
