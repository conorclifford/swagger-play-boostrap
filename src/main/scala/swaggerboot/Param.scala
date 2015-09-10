package swaggerboot

sealed trait ParamType
case object PathParam extends ParamType
case object QueryParam extends ParamType
case object HeaderParam extends ParamType

case class Param(name: String, baseType: String, required: Boolean, paramType: ParamType, defaultValue: Option[String]) {
  val typeName = if (required) baseType else s"Option[$baseType]"
  val paramName = name.toLowerCase

  override def toString() = defaultValue.fold(s"$paramName: $typeName") { defval => s"$paramName: $baseType = ${defaultValueToString(defval)}" }

  private def defaultValueToString(defval: String): String = baseType match {
    case "String" => s""""$defval""""
    case _ => defval
  }
}
