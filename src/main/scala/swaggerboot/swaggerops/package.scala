package swaggerboot

import io.swagger.models.auth.SecuritySchemeDefinition
import io.swagger.models.parameters._
import io.swagger.models.properties._
import io.swagger.models._
import scala.collection.JavaConverters._

import scalaz._
import Scalaz._

//
// FIXME **still needs A LOT OF cleanup**
//

package object swaggerops {

  implicit class SwaggerOps(val swagger: Swagger) extends AnyVal {
    def routedControllers(): Seq[RoutedController] = routedControllersWithErrors._1

    def routedControllersWithErrors(): (Seq[RoutedController], Iterable[ParseError]) = {
      val paths = Option(swagger.getPaths).map(_.asScala.toSeq).getOrElse(Nil)
      paths.map {
        case (pathStr, path) =>
          path.toController(swagger, basePath, pathStr)
      }.foldLeft((Seq.empty[RoutedController], Seq.empty[ParseError])) {
        case ((controllers, allErrors), (controller, errors)) =>
          (controllers :+ controller, allErrors ++ errors)
      }
    }

    def securityDefinitions(): Map[String, SecuritySchemeDefinition] = Option(swagger.getSecurityDefinitions).map(_.asScala.toMap).getOrElse(Map.empty)

    def definitionsWithErrors(): (Iterable[ModelDefinition], Iterable[ParseError]) = {
      val typesForPatching = routedControllers.flatMap(_.controller.methods.filter(_.httpMethod == "PATCH").flatMap(_.bodyType)).toSet

      val definitionsAndErrors: List[(ModelDefinition, Seq[ParseError])] = swagger.getDefinitions.asScala.toList.map {
        case (modelName, model) =>
          val attrsAndErrors = model.properties.map { case (propName, prop) =>
            val (scalaType, required, refname, error) = prop.scalaType(modelName, propName) match {
              case -\/(parseError) => (parseError.replacement, parseError.required, None, Some(parseError))
              case \/-((stype, required, refname)) => (stype, required, refname, None)
            }
            (ModelAttribute(propName, prop.swaggerType, scalaType, required, refname, prop.modeledEnum()), error)
          }

          // FIXME - change this to retain attribute order as per Swagger input...
          val (attrs, errors) = attrsAndErrors.foldLeft((Seq.empty[ModelAttribute], Seq.empty[ParseError])) {
            case ((attrs, errors), (attr, errOpt)) =>
              (attrs :+ attr, errOpt.fold(errors)(errors :+ _))
          }

          (ModelDefinition(modelName, attrs.toList, typesForPatching.contains(modelName)), errors)
      }

      val (realDefns, realErrors) = definitionsAndErrors.foldLeft((Seq.empty[ModelDefinition], Seq.empty[ParseError])) {
        case ((definitions, allErrors), (defn, errors)) =>
          (definitions :+ defn, allErrors ++ errors)
      }

      val (synthDefns, synthErrors) = Definitions.getSynthetics(swagger)

      (realDefns ++ synthDefns, realErrors ++ synthErrors)
    }

    def definitionsMap: Map[String, Model] = Option(swagger.getDefinitions).map(_.asScala).getOrElse(Map.empty).toMap

    def basePath: Option[String] = Option(swagger.getBasePath)
  }

  implicit class ModelOps(val model: Model) extends AnyVal {
    def properties() = Option(model.getProperties).map(_.asScala.toMap).getOrElse(Map.empty)
  }

  implicit class ObjectPropertyOps(val prop: ObjectProperty) extends AnyVal {
    def propertiesMap(): Map[String, Property] = Option(prop.getProperties).map(_.asScala.toMap).getOrElse(Map.empty)
  }

  implicit class PropertyOps(val property: Property) extends AnyVal {
    def modeledEnum(): Option[ModeledEnum] = property match {
      case strProp: StringProperty => Option(strProp.getEnum).map(_.asScala).map(ModeledEnum(_))
      case _ => None
    }

    def swaggerType(): SwaggerType = SwaggerType(property.getType, Option(property.getFormat))

    def scalaType(parentName: String, propName: String): ParseError \/ (String, Boolean, Option[String]) = {
      val rawType: ParseError \/ (String, Option[String]) = (property.getType, Option(property.getFormat)) match {
        case ("integer", Some("int32"))                             => ("Int", None).right
        case ("integer", Some("int64"))                             => ("Long", None).right
        case ("integer", None)                                      => ("Long", None).right
        case ("number", Some("float"))                              => ("Float", None).right
        case ("number", Some("double"))                             => ("Double", None).right
        case ("number", None)                                       => ("Double", None).right
        case ("string", Some("byte"))                               => ParseError(s"Unsupported property type 'string, format=byte'").left
        case ("string", Some("binary"))                             => ParseError(s"Unsupported property type 'string, format=binary'").left
        case ("string", Some("date"))                               => ("org.joda.time.DateTime", None).right
        case ("string", Some("date-time"))                          => ("org.joda.time.DateTime", None).right
        case ("string", _)                                          => ("String", None).right
        case ("boolean", _)                                         => ("Boolean", None).right
        case ("ref", _)                                             => getType(property.asInstanceOf[RefProperty])
        case ("array", _)                                           => {
          getType(parentName, propName, property.asInstanceOf[ArrayProperty]).fold(
            l = error => error.copy(replacement = s"Seq[${error.replacement}]", required = property.getRequired).left,
            r = v => (s"Seq[${v._1}]", v._2).right
          )
        }
        case ("object", _) if property.isInstanceOf[ObjectProperty] => getType(Definitions.syntheticModelName(parentName, propName), property.asInstanceOf[ObjectProperty])
        case ("object", _) if property.isInstanceOf[MapProperty]    => getType(parentName, propName, property.asInstanceOf[MapProperty]) // FIXME are these parent/prop names correct here?
        case x =>
          ParseError(s"Unsupported property type '$x'").left
      }
      rawType.map { case (n, rn) => (n, property.getRequired, rn) }
    }

    private def getType(parentName: String, propName: String, arrayProp: ArrayProperty): ParseError \/ (String, Option[String]) = {
      arrayProp.getItems.scalaType(parentName, propName).map(st => (st._1, st._3))
    }

    private def getType(refProp: RefProperty): ParseError \/ (String, Option[String]) = {
      (refProp.getSimpleRef, Some(refProp.getSimpleRef)).right
    }

    private def getType(parentName: String, propName: String, mapProp: MapProperty): ParseError \/ (String, Option[String]) = {
      mapProp.getAdditionalProperties.scalaType(parentName, propName).leftMap { case pe =>
        ParseError(msg = pe.msg, replacement = "Map[String, play.api.libs.json.JsValue]", required = pe.required)
      }.map { case (valtype, _, reftype) =>
        (s"Map[String, $valtype]", reftype)
      }
    }

    private def getType(name: String, objProp: ObjectProperty): ParseError \/ (String, Option[String]) = {
      if (objProp.getProperties.isEmpty()) {
        ("play.api.libs.json.JsValue", None).right
      } else {
        // Simply refer to the synthentic name here - note giving reference to the synthethic here for ordering above
        (name, Some(name)).right
      }
    }
  }

  implicit class PathOps(val path: Path) extends AnyVal {
    type NamedOperation = (String, Operation)

    def GET(): Option[NamedOperation] = Option(path.getGet()).map("GET" -> _)
    def PUT(): Option[NamedOperation] = Option(path.getPut()).map("PUT" -> _)
    def PATCH(): Option[NamedOperation] = Option(path.getPatch()).map("PATCH" -> _)
    def POST(): Option[NamedOperation] = Option(path.getPost()).map("POST" -> _)
    def DELETE(): Option[NamedOperation] = Option(path.getDelete()).map("DELETE" -> _)

    def operations(): Seq[NamedOperation] = Seq(GET, PUT, PATCH, POST, DELETE).flatten

    def parameters(): Seq[Parameter] = Option(path.getParameters).map(_.asScala).getOrElse(Nil)

    def toController(swagger: Swagger, basePath: Option[String], pathStr: String): (RoutedController, Seq[ParseError]) = {
      val controllerName = makeControllerName(pathStr)
      val singleInstance = isSingleInstance(pathStr)
      val scalaPath = basePath.fold(pathStr)(bp => s"$bp$pathStr".replace("//", "/")).replace("{", ":").replace("}", "")

      val (methods, errors) = path.operations.map { case (opName, op: Operation) =>
        val hasBodyArg = Option(op.getParameters).map(_.asScala).getOrElse(Nil).exists(_.isInstanceOf[BodyParameter])

        val hasSuccessResponseBody = op.responses().filterKeys(_.toInt < 300).find { case (_, resp) => Option(resp.getSchema).nonEmpty }.nonEmpty

        val methodName = opName match {
          case "GET" if singleInstance          => "get"
          case "GET" if hasBodyArg              => "get"
          case "GET" if !hasSuccessResponseBody => "get"
          case "GET"                            => "list"
          case "PUT" if singleInstance          => "put"
          case "PUT"                            => "putUnknown"
          case x                                => x.toLowerCase
        }

        // Path parameters apply to all Operations (can be overridden (by name), but cannot be removed)
        val allApplicableParameters = parameters.filterNot {
          pp =>
            op.parameters.exists(_.getName == pp.getName)
        } ++ op.parameters

        val (params, paramErrors) = allApplicableParameters.filter(p => Seq("query", "path").contains(p.getIn)).map { param =>
          val (typeName, error) = param.typeName match {
            case -\/(parseError) => (parseError.replacement, Some(parseError))
            case \/-(tname) => (tname, None: Option[ParseError])
          }
          val paramType = param.getIn match {
            case "query" => QueryParam
            case "path" => PathParam
          }

          (Param(param.getName, typeName, param.getRequired, paramType, param.defaultValue), error)
        }.foldLeft((Seq.empty[Param], Seq.empty[ParseError])) {
          case ((params, errors), (param, errOpt)) =>
            (params :+ param, errOpt.fold(errors)(errors :+ _))
        }

        // This assumes a single body parameter only.
        val (bodyOpt: Option[Body], bodyError: Option[ParseError]) = allApplicableParameters.find(_.getIn == "body").map { param =>
          val (typeName, error) = param.typeName match {
            case -\/(parseError) => (parseError.replacement, Some(parseError))
            case \/-(tname) => (tname, None)
          }
          (Body(typeName), error)
        } match {
          case Some((body, errorOpt)) => (Some(body), errorOpt)
          case None => (None, None)
        }

        val (headerParams, headerErrors) = allApplicableParameters.filter(_.getIn == "header").map { param =>
          val (typeName, error) = param.typeName match {
            case -\/(parseError) => (parseError.replacement, Some(parseError))
            case \/-(tname) => (tname, None: Option[ParseError])
          }
          (Param(param.getName, typeName, param.getRequired, HeaderParam, param.defaultValue), error)
        }.foldLeft((Seq.empty[Param], Seq.empty[ParseError])) {
          case ((params, errors), (param, errOpt)) =>
            (params :+ param, errOpt.fold(errors)(errors :+ _))
        }

        // Produces list can override swagger spec. wide (including wiping, with local empty list)...
        val produces = (Option(op.getProduces) orElse Option(swagger.getProduces)).map(_.asScala).getOrElse(Nil)
        val consumes = (Option(op.getConsumes) orElse Option(swagger.getConsumes)).map(_.asScala).getOrElse(Nil)

        val returnValues =
          op.responses().toSeq.map { case (rcode, resp) =>

            val optReturnType = Option(resp.getSchema).map {
              // FIXME get better "parentName" and definitely for "propName" for this?
              _.scalaType("Response", s"$rcode").fold(_.replacement, _._1)
            }.map {
              stype => ReturnType(stype, stype.startsWith("Seq["))
            }

            ReturnValue(
              rcode = rcode.toInt,
              description = Option(resp.getDescription),
              returnType = optReturnType
            )
          }.toSet

        (
          Method(
            routePath = scalaPath,
            httpMethod = opName,
            name = methodName,
            params = params,
            headerParams = headerParams,
            produces = produces,
            consumes = consumes,
            returnValues = returnValues,
            operationId = op.opId,
            body = bodyOpt),
          paramErrors ++ headerErrors ++ bodyError.toList
        )
      }.foldLeft((Seq.empty[Method], Seq.empty[ParseError])) {
        case ((methods, allErrors), (method, errors)) =>
          (methods :+ method, allErrors ++ errors)
      }

      (RoutedController(scalaPath, Controller(controllerName, methods)), errors)
    }

    private def parseResources(input: String) : Seq[String] = {
      input.split("/").flatMap {
        case bit if bit.nonEmpty && !bit.startsWith("{") => Some(bit)
        case _ => None
      }
    }
    private def makeControllerName(pathName: String)  = {
      val parsed = parseResources(pathName).map(resourceToClassName)
      parsed.init.map(singularize).mkString("") + parsed.last
    }

    private def singularize(str: String) = if (str.endsWith("s")) str.dropRight(1) else str

    private def isSingleInstance(path: String): Boolean = path.endsWith("}") || path.endsWith("}/")

    private def resourceToClassName(name: String) = name.split("-").map(w => w.take(1).toUpperCase + w.drop(1)).mkString("")
  }

  implicit class OperationOps(val op: Operation) extends AnyVal {
    def parameters(): Seq[Parameter] = Option(op.getParameters).map(_.asScala).getOrElse(Nil)
    def produces(): Seq[String] = Option(op.getProduces).map(_.asScala).getOrElse(Nil)
    def security(): Seq[Map[String, Seq[String]]] = Option(op.getSecurity).map(
      _.asScala.toSeq.map(
        _.asScala.toMap.mapValues(
          _.asScala.toSeq
        )
      )
    ).getOrElse(Nil)

    def opId(): Option[String] = Option(op.getOperationId)

    def responses(): Map[String, Response] = Option(op.getResponses).map(_.asScala.toMap).getOrElse(Map.empty)
  }

  implicit class ParameterOps(val param: Parameter) extends AnyVal {
    def typeName(): ParseError \/ String = param match {
      case p: QueryParameter => scalaType(p.getType, Option(p.getFormat))
      case p: PathParameter => scalaType(p.getType, Option(p.getFormat))
      case p: BodyParameter => getReference(p)
      case p: HeaderParameter => scalaType(p.getType, Option(p.getFormat))
      case _ =>
        ParseError(s"Unsupported swaggerboot.Param type ${param.getClass.getName}").left
    }

    def defaultValue(): Option[String] = param match {
      case qparam: QueryParameter =>
        Option(qparam.getDefaultValue)
      case _ =>
        None
    }

    private def getReference(p: BodyParameter): ParseError \/ String = {
      def processModel(mod: Model): Option[String] = mod match {
        case rmod: RefModel => Option(rmod.getSimpleRef)
        case amod: ArrayModel => amod.getItems match {
          case ref: RefProperty => Option(ref.getSimpleRef).map(m => s"Seq[$m]")
          case prop: AbstractProperty => prop.scalaType("BodyParam", "Body").toOption.map(st => s"Seq[${st._1}]")
        }
        case _ => None
      }

      Option(p.getSchema).flatMap(processModel) \/> ParseError("Missing Schema/Ref for a Body Parameter")
    }

    private def scalaType(swaggerType: String, format: Option[String]): ParseError \/ String = (swaggerType, format) match {
      case ("string", Some("date"))     => "org.joda.time.DateTime".right
      case ("string", Some("datetime")) => "org.joda.time.DateTime".right
      case ("string", Some("csv"))      => "Seq[String]".right
      case ("string", _)                => "String".right
      case ("integer", Some("int32"))   => "Int".right
      case ("integer", Some("int64"))   => "Long".right
      case ("integer", _)               => "Long".right
      case ("number", Some("float"))    => "Float".right
      case ("number", Some("double"))   => "Double".right
      case ("number", _)                => "Double".right
      case ("boolean", _)               => "Boolean".right
      case x =>
        ParseError("Unsupported parameter type '$x'", replacement = s"FIXME[$x]").left
    }
  }
}
