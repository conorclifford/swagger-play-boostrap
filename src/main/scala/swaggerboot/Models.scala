package swaggerboot

import swaggerboot.codegen.server.ControllerDelegateTraits

sealed trait ParamType
case object PathParam extends ParamType
case object QueryParam extends ParamType
case object HeaderParam extends ParamType

case class Param(name: String, baseType: String, required: Boolean, paramType: ParamType, defaultValue: Option[String])

case class Body(typeName: String)

case class ModeledEnum(values: Seq[String])

case class SwaggerType(baseType: String, format: Option[String])

case class ModelAttribute(name: String, swaggerType: SwaggerType, scalaType: String, required: Boolean, referencedName: Option[String], modeledEnum: Option[ModeledEnum])

case class ModelDefinition(name: String, attributes: Seq[ModelAttribute], supportPatch: Boolean, cyclicReferences: Option[Set[String]] = None)

case class ReturnType(name: String, isArray: Boolean)
case class ReturnValue(rcode: Int, description: Option[String], returnType: Option[ReturnType])

case class MethodDelegate(packageName: String, className: String, functionName: String, method: Method)

case class Method(routePath: String,
                  httpMethod: String,
                  name: String,
                  params: Seq[Param],
                  headerParams: Seq[Param],
                  produces: Seq[String],
                  consumes: Seq[String],
                  returnValues: Set[ReturnValue],
                  operationId: Option[String],
                  body: Option[Body]) {
  val bodyType = body.map(_.typeName)
  val isGet = httpMethod == "GET"
  val acceptableContentTypes = (if (isGet) produces else consumes) match {
    case Nil => Seq("application/json")
    case x => x
  }

  val delegate = operationId.map { oid =>
    val bits = oid.split("\\.")
    val fname = bits.last
    val cname = bits.dropRight(1).last
    val pname = bits.dropRight(2).mkString(".")
    require(pname != ControllerDelegateTraits.PackageName, "Illegal packagename for delegates")
    MethodDelegate(pname, cname, fname, this)
  }
}

case class Controller(name: String, methods: Seq[Method])

case class ControllerDelegate(packageName: String, className: String, methods: Seq[MethodDelegate])

case class RoutedController(path: String, controller: Controller)

case class ParseError(msg: String, replacement: String = "play.api.libs.json.JsValue", required: Boolean = false)

