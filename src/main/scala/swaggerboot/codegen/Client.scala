package swaggerboot.codegen

import swaggerboot._
import swaggerboot.Id

object Client {
  def generate(packageName: String, controllers: Seq[Controller], generatePlay23Code: Boolean)(implicit ids: Map[String, Id]): String = {
    def toIdentifier(name: String) = name.head.toLower +: name.tail

    s"""//
        |// This is generated code. Do not edit this file.
        |//
        |package $packageName
        |
        |import play.api.libs.json.Json
        |import play.api.libs.ws.WSClient
        |
        |import scala.concurrent.{ExecutionContext, Future}
        |import scalaz._
        |import Scalaz._
        |
        |import models._
        |import JsonOps._
        |
        |object Result {
        |  case class Success[B](responseCode: Int, message: Option[String], body: Option[B] = None, rawResponse: play.api.libs.ws.WSResponse)
        |  case class Error[B](responseCode: Int, message: Option[String] = None, body: Option[B] = None, rawResponse: play.api.libs.ws.WSResponse)
        |}
        |
        |case class RequestTimeout(duration: scala.concurrent.duration.Duration)
        |
        |${controllers.map(clientTrait).mkString("\n")}
        |
        |trait Client {
        |${Indenter.indent(controllers.map(rc => s"def ${toIdentifier(rc.name)}: ${rc.name}Client").mkString("\n"))}
        |}
        |
        |class ClientImpl(baseUrl: String, wsClient: => WSClient = play.api.libs.ws.WS.client(play.api.Play.current)) extends Client {
        |  import JsonOps._
        |
        |${Indenter.indent(controllers.map(rc => s"override val ${toIdentifier(rc.name)}: ${rc.name}Client = ${rc.name}Client").mkString("\n"))}
        |
        |${Indenter.indent(controllers.map(clientImpl(_, generatePlay23Code)).mkString("\n"))}
        |}
       """.stripMargin
  }

  private def clientTrait(controller: Controller)(implicit ids: Map[String, Id]) =
    s"""trait ${controller.name}Client {
        |  ${controller.methods.map(clientSignature).mkString("\n  ")}
        |}
     """.stripMargin

  private def clientImpl(controller: Controller, generatePlay23Code: Boolean)(implicit ids: Map[String, Id]) =
    s"""object ${controller.name}Client extends ${controller.name}Client {
        |${Indenter.indent(controller.methods.map(clientMethod(_, generatePlay23Code)).mkString("\n"))}
        |}
     """.stripMargin

  private def clientSignature(method: Method)(implicit ids: Map[String, Id]): String = {
    val returnTypeMap: Map[Int, ReturnType] = method.returnValues.flatMap {
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

    val bodyParam = method.bodyType.map { body =>
      if (method.httpMethod == "PATCH") {
        s"Patch$body"
      } else {
        body
      }
    }.map { bt => s"body: $bt" }

    def paramSig(param: Param)(implicit ids: Map[String, Id]) = {
      def paramType(param: Param) = {
        ids.get(param.name).map { id =>
          s"ids.${id.name}"
        }.getOrElse {
          param.baseType
        }
      }

      val baseType = paramType(param)
      val typeName = if (param.required) baseType else s"Option[${baseType}]"
      s"${param.scalaName}: $typeName"
    }

    val contentTypeParam = if (includeContentType(method)) {
      Option(s"""contentType: String = "${method.acceptableContentTypes.headOption.getOrElse("application/json")}"""")
    } else {
      None
    }

    val mandatoryParams = method.params.filter(_.required).map(p => Some(paramSig(p))) :+ bodyParam
    val optionalParams = method.params.filterNot(_.required).map(p => Some(s"${paramSig(p)} = None"))
    val parameters =
      ( mandatoryParams ++
        optionalParams ++
        method.headerParams.map(p => Some(paramSig(p)))
      ).flatten ++ contentTypeParam.toSeq

    s"""def ${method.name}(${parameters.mkString(", ")})(implicit ec: ExecutionContext, requestTimeout: RequestTimeout): Future[Result.Error[$ftype] \\/ Result.Success[$stype]]"""
  }

  private def includeContentType(method: Method): Boolean = {
    method.httpMethod match {
      case "PUT" | "POST" | "PATCH" | "DELETE" if method.body.isEmpty => false
      case _ => true
    }
  }

  def clientMethod(method: Method, generatePlay23Code: Boolean)(implicit ids: Map[String, Id]): String = {

    def paramName(param: Param) = param.scalaName

    def encodedParam(param: Param) = {
      if (param.baseType startsWith "Seq[") s"""${paramName(param)}.mkString(",")"""
      else s"${paramName(param)}.toString"
    }

    def toHeader(param: Param) = {
      if (param.required) {
        s"""Some("${param.name}" -> ${paramName(param)}.toString)"""
      } else {
        s"""${paramName(param)}.map("${param.name}" -> _.toString)"""
      }
    }

    def queryParamsSnippet(): String = {
      if (method.params.exists(_.paramType == QueryParam)) {
        s"""
           |  val queryParams = Seq(
           |    ${
          method.params.filter(_.paramType == QueryParam).map {
            case param @ Param(name, _, required, _, _) if required => s"""Some("$name" -> ${paramName(param)}.toString)"""
            case param @ Param(name, _, required, _, _) if !required => s"""${paramName(param)}.map("$name" -> _.toString)"""
          }.mkString(",\n    ")}
           |  ).flatten
           |"""
      } else {
        ""
      }
    }

    val path = method.params.filter(_.paramType == PathParam).foldLeft(method.routePath) {
      case (rp, param) =>
        rp.replace(s":${param.name}", s"""$${play.utils.UriEncoding.encodePathSegment(${encodedParam(param)}, "UTF-8")}""")
    }

    val withQueryString = if (method.params.exists(_.paramType == QueryParam)) {
      s""".withQueryString(queryParams:_*)"""
    } else {
      ""
    }

    val withBody = method.bodyType.filter(_ => method.isGet).fold("")(_ => ".withBody(Json.toJson(body))")
    val callArgs = method.bodyType.filterNot(_ => method.isGet).fold("")(_ => "Json.toJson(body)")

    val executeMethod = method.httpMethod match {
      case "PUT" | "PATCH" if method.bodyType.isEmpty => s"""withMethod("${method.httpMethod}").execute()"""
      case _ => s"${method.httpMethod.toLowerCase}($callArgs)"
    }

    def codify(v: Option[String]) = v match {
      case None => "None"
      case Some(x) => s"""Some("$x")"""
    }

    val contentTypeCheck = if (includeContentType(method)) {
      s"""require(Seq(${method.acceptableContentTypes.mkString("\"", "\", \"", "\"")}) contains contentType, "Unacceptable contentType specified")"""
    } else {
      ""
    }

    val contentTypeHeader = if (includeContentType(method)) {
      s"""Some("Content-Type" -> s"$$contentType; charset=UTF-8")"""
    } else {
      ""
    }

    val withRequestTimeout = if (generatePlay23Code) {
      "withRequestTimeout(requestTimeout.duration.toMillis.toInt)"
    } else {
      "withRequestTimeout(requestTimeout.duration.toMillis)"
    }


    s"""${clientSignature(method)} = {
       |  $contentTypeCheck
       |  val headers = Seq(
       |    $contentTypeHeader${if (includeContentType(method) && method.headerParams.nonEmpty) ",\n    " else ""}${method.headerParams.map(toHeader).mkString("", ",\n    ", "\n  ")}).flatten
       |  $queryParamsSnippet
       |  wsClient.url(s"$$baseUrl$path")$withQueryString.withHeaders(headers:_*).$withRequestTimeout$withBody.$executeMethod.map {
       |${
            method.returnValues.toSeq.sortBy(_.rcode).map {
              case ReturnValue(rcode, message, None) if rcode < 400 =>
                s"""    case resp if resp.status == $rcode =>
                   |      \\/-(Result.Success(resp.status, message = ${codify(message)}, rawResponse = resp))
                   |""".stripMargin
              case ReturnValue(rcode, message, Some(ReturnType(retName, _))) if rcode < 400 =>
                s"""    case resp if resp.status == $rcode =>
                   |      resp.json.validate[$retName].fold(
                   |        invalid = errors => -\\/(Result.Error(resp.status, Some("Invalid body for '$retName': " + errors.mkString(", ")), rawResponse = resp)),
                   |        valid = b => \\/-(Result.Success(resp.status, message = ${codify(message)}, Some(b), rawResponse = resp))
                   |      )
                   |""".stripMargin
              case ReturnValue(rcode, message, None) if rcode > 399 =>
                s"""    case resp if resp.status == $rcode =>
                   |      -\\/(Result.Error(resp.status, message = ${codify(message)}, rawResponse = resp))
                   |""".stripMargin
              case ReturnValue(rcode, message, Some(ReturnType(retName, _))) if rcode > 399 =>
                s"""    case resp if resp.status == $rcode =>
                   |      -\\/(resp.json.validate[$retName].fold(
                   |        invalid = errors => Result.Error(resp.status, Some("Invalid body for error body '$retName': " + errors.mkString(", ")), rawResponse = resp),
                   |        valid = b => Result.Error(resp.status, message = ${codify(message)}, body = Some(b), rawResponse = resp)
                   |      ))
                   |""".stripMargin
             }.mkString("")
            }    case resp =>
       |      -\\/(Result.Error(resp.status, message = Some("Unexpected response code"), rawResponse = resp))
       |  }
       |}
     """.stripMargin
  }

}
