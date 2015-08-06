package swaggerboot

sealed trait ParamType
case object PathParam extends ParamType
case object QueryParam extends ParamType
case object HeaderParam extends ParamType

case class Param(name: String, baseType: String, required: Boolean, paramType: ParamType) {
  val typeName = if (required) baseType else s"Option[$baseType]"
  val paramName = name.toLowerCase
  override def toString() = s"$paramName: $typeName"
}
