package swaggerboot

case class Param(name: String, baseType: String, required: Boolean) {
  val typeName = if (required) baseType else s"Option[$baseType]"
  override def toString() = s"$name: $typeName"
}
