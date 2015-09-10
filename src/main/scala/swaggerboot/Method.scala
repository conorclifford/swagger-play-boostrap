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
            returnValues: Set[ReturnValue],
            body: Option[Body]): Method = {
    body.fold(BodyLessMethod(routePath, httpMethod, name, params, headerParams, produces, returnValues): Method) {
      bod => MethodExpectingBody(routePath, httpMethod, name, params, headerParams, produces, returnValues, bod)
    }
  }
}

sealed trait Method {
  def routePath: String
  def httpMethod: String
  def name: String
  def params: Seq[Param]
  def headerParams: Seq[Param]
  def scalaImpl: String
  def bodyType: Option[String]
  def produces: Seq[String]
  def returnValues: Set[ReturnValue]

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

  def clientSignature(): String = {
    val returnTypeMap: Map[Int, ReturnType] = returnValues.flatMap {
      case ReturnValue(rcode, _, rtype) => rtype.map(rcode -> _)
    }.toMap

    val ftype = {
      returnTypeMap.filterKeys(_ > 399).map { case (_, ReturnType(name, _)) => name } match {
        case Nil => "Nothing"
        case names => names.mkString(""" \/ """)
      }
    }

    val stype = {
      returnTypeMap.filterKeys(_ < 300).map { case (_, ReturnType(name, _)) => name } match {
        case Nil => "Nothing"
        case names => names.mkString(""" \/ """)
      }
    }

    val bodyParam = bodyType.map { body =>
      if (httpMethod == "PATCH") {
        s"Patch$body"
      } else {
        body
      }
    }.map { bt => s"body: $bt" }

    val mandatoryParams = params.filter(_.required).map(p => Some(p.toString)) :+ bodyParam
    val optionalParams = params.filterNot(_.required).map(p => Some(s"$p = None"))
    val parameters = (mandatoryParams ++ optionalParams ++ headerParams.map(p => Some(p.toString))).flatten :+ s"""contentType: String = "${produces.headOption.getOrElse("application/json")}""""

    s"""def $name(${parameters.mkString(", ")})(implicit ec: ExecutionContext): Future[Result.Error[$ftype] \\/ Result.Success[$stype]]"""
  }

  def clientMethod(): String = {
    val acceptableContentTypes = produces match {
      case Nil => Seq("application/json")
      case _ => produces
    }

    def toHeader(param: Param) = if (param.required) {
      s"""Some("${param.name}" -> ${param.paramName})"""
    } else {
      s"""${param.paramName}.map("${param.name}" -> _)"""
    }

    def queryParamsSnippet(): String = {
      if (params.exists(_.paramType == QueryParam)) {
        s"""
           |  val queryParams = Seq(
           |    ${
          params.filter(_.paramType == QueryParam).map {
            case param @ Param(name, _, required, _, _) if required => s"""Some("$name" -> ${param.paramName})"""
            case param @ Param(name, _, required, _, _) if !required => s"""${param.paramName}.map("$name" -> _)"""
          }.mkString(",\n    ")}
           |  ).flatten
           |"""
      } else {
        ""
      }
    }

    val path = params.filter(_.paramType == PathParam).foldLeft(routePath) {
      case (rp, param) =>
        rp.replace(s":${param.name}", s"""$${play.utils.UriEncoding.encodePathSegment(${param.paramName}, "UTF-8")}""")
    }

    val withQueryString = if (params.exists(_.paramType == QueryParam)) {
      s""".withQueryString(queryParams:_*)"""
    } else {
      ""
    }

    val isGet = httpMethod == "GET"
    val withBody = bodyType.filter(_ => isGet).fold("")(_ => ".withBody(Json.toJson(body))")
    val callArgs = bodyType.filterNot(_ => isGet).fold("")(_ => "Json.toJson(body)")

    def codify(v: Option[String]) = v match {
      case None => "None"
      case Some(x) => s"""Some("$x")"""
    }


    s"""$clientSignature = {
       |  require(Seq(${acceptableContentTypes.mkString("\"", "\", \"", "\"")}) contains contentType, "Unacceptable contentType specified")
       |
       |  val headers = Seq(
       |    Some("Content-Type" -> s"$$contentType; charset=UTF-8")${if (headerParams.nonEmpty) ",\n    " else ""}${headerParams.map(toHeader).mkString("", ",\n    ", "\n  ")}).flatten
       |  $queryParamsSnippet
       |  wsClient.url(s"$$baseUrl$path")$withQueryString.withHeaders(headers:_*)$withBody.${httpMethod.toLowerCase}($callArgs).map {
       |${
            returnValues.toSeq.sortBy(_.rcode).map {
              case ReturnValue(rcode, message, None) if rcode < 400 =>
                s"""    case resp if resp.status == $rcode =>
                   |      \\/-(Result.Success(resp.status, message = ${codify(message)}))
                   |""".stripMargin
              case ReturnValue(rcode, message, Some(ReturnType(retName, _))) if rcode < 400 =>
                s"""    case resp if resp.status == $rcode =>
                   |      resp.json.validate[$retName].fold(
                   |        invalid = errors => -\\/(Result.Error(resp.status, Some("Invalid body for '$retName': " + errors.mkString(", ")))),
                   |        valid = b => \\/-(Result.Success(resp.status, message = ${codify(message)}, Some(b)))
                   |      )
                   |""".stripMargin
              case ReturnValue(rcode, message, None) if rcode > 399 =>
                s"""    case resp if resp.status == $rcode =>
                   |      -\\/(Result.Error(resp.status, message = ${codify(message)}))
                   |""".stripMargin
              case ReturnValue(rcode, message, Some(ReturnType(retName, _))) if rcode > 399 =>
                s"""    case resp if resp.status == $rcode =>
                   |      -\\/(resp.json.validate[$retName].fold(
                   |        invalid = errors => Result.Error(resp.status, Some("Invalid body for error body '$retName': " + errors.mkString(", "))),
                   |        valid = b => Result.Error(resp.status, message = ${codify(message)}, body = Some(b))
                   |      ))
                   |""".stripMargin
             }.mkString("")
            }    case resp =>
       |      -\\/(Result.Error(resp.status, message = Some("Unexpected response code")))
       |  }
       |}
     """.stripMargin
  }
}

case class BodyLessMethod(override val routePath: String,
                          override val httpMethod: String,
                          override val name: String,
                          override val params: Seq[Param],
                          override val headerParams: Seq[Param],
                          override val produces: Seq[String],
                          override val returnValues: Set[ReturnValue]) extends Method {

  override val bodyType = None

  override lazy val scalaImpl =
    s"""
       |  def $name(${params.mkString(", ")}) = Action {
       |    ${if(headerParams.nonEmpty) headerParams.mkString("// header-param: ", "\n    //", "\n") else ""}${producesCommentary()}
       |    InternalServerError("not implemented") // FIXME needs implementation
       |  }
     """.stripMargin
}

case class MethodExpectingBody(override val routePath: String,
                               override val httpMethod: String,
                               override val name: String,
                               override val params: Seq[Param],
                               override val headerParams: Seq[Param],
                               override val produces: Seq[String],
                               override val returnValues: Set[ReturnValue],
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

