package swaggerboot

case class ModelDefinition(name: String, attributes: Seq[ModelAttribute], supportPatch: Boolean, cyclicReferences: Option[Set[String]] = None)
