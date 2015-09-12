package swaggerboot

case class ReturnType(name: String, isArray: Boolean)
case class ReturnValue(rcode: Int, description: Option[String], returnType: Option[ReturnType])

object Method {
  def apply(routePath: String,
            httpMethod: String,
            name: String,
            params: Seq[Param],
            headerParams: Seq[Param],
            produces: Seq[String],
            consumes: Seq[String],
            returnValues: Set[ReturnValue],
            body: Option[Body]): Method = {
    body.fold(BodyLessMethod(routePath, httpMethod, name, params, headerParams, produces, consumes, returnValues): Method) {
      bod => MethodExpectingBody(routePath, httpMethod, name, params, headerParams, produces, consumes, returnValues, bod)
    }
  }
}

sealed trait Method {
  def routePath: String
  def httpMethod: String
  def name: String
  def params: Seq[Param]
  def headerParams: Seq[Param]
  def bodyType: Option[String]
  def produces: Seq[String]
  def consumes: Seq[String]
  def returnValues: Set[ReturnValue]

  final val isGet = httpMethod == "GET"
  final val acceptableContentTypes = (if (isGet) produces else consumes) match {
    case Nil => Seq("application/json")
    case x => x
  }
}

case class BodyLessMethod(override val routePath: String,
                          override val httpMethod: String,
                          override val name: String,
                          override val params: Seq[Param],
                          override val headerParams: Seq[Param],
                          override val produces: Seq[String],
                          override val consumes: Seq[String],
                          override val returnValues: Set[ReturnValue]) extends Method {
  override val bodyType = None
}

case class MethodExpectingBody(override val routePath: String,
                               override val httpMethod: String,
                               override val name: String,
                               override val params: Seq[Param],
                               override val headerParams: Seq[Param],
                               override val produces: Seq[String],
                               override val consumes: Seq[String],
                               override val returnValues: Set[ReturnValue],
                               body: Body) extends Method {
  override val bodyType = Some(body.typeName)
}

