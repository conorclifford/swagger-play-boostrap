package swaggerboot


object Method {
  def apply(httpMethod: String, name: String, params: Seq[Param], body: Option[Body]): Method = {
    body.fold(BodyLessMethod(httpMethod, name, params): Method)(bod => MethodExpectingBody(httpMethod, name, params, bod))
  }
}

sealed trait Method {
  def httpMethod: String
  def name: String
  def params: Seq[Param]
  def scalaImpl: String
  def bodyType: Option[String]

  final def routeFileEntry(path: String, controllerName: String) = {
    s"$httpMethod \t$path \tcontrollers.$controllerName.$name(${params.mkString(", ")})"
  }
}

case class BodyLessMethod(override val httpMethod: String, override val name: String, override val params: Seq[Param]) extends Method {

  override val bodyType = None

  override lazy val scalaImpl =
    s"""
       |  def $name(${params.mkString(", ")}) = Action {
       |    InternalServerError("not implemented") // FIXME needs implementation
       |  }
     """.stripMargin
}

case class MethodExpectingBody(override val httpMethod: String, override val name: String, override val params: Seq[Param], body: Body) extends Method {

  override val bodyType = Some(body.typeName)

  private val bodyModelName = if (httpMethod == "PATCH") s"Patch${body.typeName}" else body.typeName

  override lazy val scalaImpl =
    s"""
       |  def $name(${params.mkString(", ")}) = Action(parse.json) { request =>
       |    def ${name}OnValid(body: $bodyModelName) = {
       |      InternalServerError("not implemented") // FIXME needs implementation
       |    }
       |
       |    request.body.validate[$bodyModelName].fold(
       |      valid = ${name}OnValid,
       |      invalid = (e => BadRequest(e.toString))
       |    )
       |  }
   """.stripMargin
}

