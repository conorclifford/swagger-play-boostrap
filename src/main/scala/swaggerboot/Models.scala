package swaggerboot

sealed trait ParamType
case object PathParam extends ParamType
case object QueryParam extends ParamType
case object HeaderParam extends ParamType

case class Param(name: String, baseType: String, required: Boolean, paramType: ParamType, defaultValue: Option[String])

case class Body(typeName: String)

case class ModelAttribute(name: String, scalaType: String, required: Boolean, referencedName: Option[String])

case class ModelDefinition(name: String, attributes: Seq[ModelAttribute], supportPatch: Boolean, cyclicReferences: Option[Set[String]] = None)

case class Controller(name: String, methods: Seq[Method])

case class RoutedController(path: String, controller: Controller)

case class ParseError(msg: String, replacement: String = "play.api.libs.json.JsValue", required: Boolean = false)

