package swaggerboot

case class ModelAttribute(name: String, scalaType: String, required: Boolean, referencedName: Option[String])
