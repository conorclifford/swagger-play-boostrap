package swaggerboot

sealed trait ParamType
case object PathParam extends ParamType
case object QueryParam extends ParamType
case object HeaderParam extends ParamType

case class Param(name: String, baseType: String, required: Boolean, paramType: ParamType, defaultValue: Option[String])

case class Body(typeName: String)

case class ModelAttribute(name: String, scalaType: String, required: Boolean, referencedName: Option[String])

case class ModelDefinition(name: String, attributes: Seq[ModelAttribute], supportPatch: Boolean, cyclicReferences: Option[Set[String]] = None)

case class ReturnType(name: String, isArray: Boolean)
case class ReturnValue(rcode: Int, description: Option[String], returnType: Option[ReturnType])

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
}

case class Controller(name: String, methods: Seq[Method])

case class RoutedController(path: String, controller: Controller)

case class ParseError(msg: String, replacement: String = "play.api.libs.json.JsValue", required: Boolean = false)

