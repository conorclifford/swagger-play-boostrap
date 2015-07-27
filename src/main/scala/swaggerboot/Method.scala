package swaggerboot


object Method {
  def apply(httpMethod: String, name: String, params: Seq[Param], headerParams: Seq[Param], produces: Seq[String], body: Option[Body]): Method = {
    body.fold(BodyLessMethod(httpMethod, name, params, headerParams, produces): Method) {
      bod => MethodExpectingBody(httpMethod, name, params, headerParams, produces, bod)
    }
  }
}

sealed trait Method {
  def httpMethod: String
  def name: String
  def params: Seq[Param]
  def headerParams: Seq[Param]
  def scalaImpl: String
  def bodyType: Option[String]
  def produces: Seq[String]

  protected def producesCommentary(): String = {
    if (produces.nonEmpty) {
      s"""
         |    /* Produces (one of):
         |      - ${produces.mkString("\n      - ")}
         |     */""".stripMargin
    } else {
      ""
    }
  }

  final def routeFileEntry(path: String, controllerName: String) = {
    s"$httpMethod \t$path \tcontrollers.$controllerName.$name(${params.mkString(", ")})"
  }
}

case class BodyLessMethod(override val httpMethod: String,
                          override val name: String,
                          override val params: Seq[Param],
                          override val headerParams: Seq[Param],
                          override val produces: Seq[String]) extends Method {

  override val bodyType = None

  override lazy val scalaImpl =
    s"""
       |  def $name(${params.mkString(", ")}) = Action {
       |    ${if(headerParams.nonEmpty) headerParams.mkString("// header-param: ", "\n    //", "\n") else ""}${producesCommentary()}
       |    InternalServerError("not implemented") // FIXME needs implementation
       |  }
     """.stripMargin
}

case class MethodExpectingBody(override val httpMethod: String,
                               override val name: String,
                               override val params: Seq[Param],
                               override val headerParams: Seq[Param],
                               override val produces: Seq[String],
                               body: Body) extends Method {

  override val bodyType = Some(body.typeName)

  private val bodyModelName = if (httpMethod == "PATCH") s"Patch${body.typeName}" else body.typeName

  override lazy val scalaImpl =
    s"""
       |  def $name(${params.mkString(", ")}) = Action(parse.json) { request =>
       |    ${if(headerParams.nonEmpty) headerParams.mkString("// header-param: ", "\n    //", "\n") else ""}${producesCommentary()}
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

