package swaggerboot

import io.swagger.models.parameters.{AbstractSerializableParameter, Parameter, BodyParameter}

import scala.collection.JavaConverters._
import io.swagger.models.{Operation, Path, Swagger}

import scalaz.{\/-, -\/}

case class SwaggerRep(title: String,
                      controllers: Seq[Controller],
                      routedControllers: Seq[RoutedController],
                      containsCsvParams: Boolean,
                      modelDefinitions: Seq[ModelDefinition],
                      ids: Map[String, Id])

object SwaggerProcessor {
  import swaggerops._

  def process(swagger: Swagger, verbose: Boolean = true): SwaggerRep = {

    val (controllers, routedControllers) = extractControllers(swagger, verbose)
    val modelDefinitions = extractModelDefinitions(swagger, routedControllers, verbose)
    val containsCsvParams = routedControllers.exists(_.controller.methods.exists(_.params.exists(_.baseType == "Seq[String]")))
    val ids = extractAllIds(modelDefinitions, controllers)
    val title = extractTitle(swagger)

    SwaggerRep(title, controllers, routedControllers, containsCsvParams, modelDefinitions.toSeq, ids)
  }

  private def logParseError(error: ParseError, verbose: Boolean) = {
    if (verbose) {
      System.err.println(s"WARN - ${error.msg} - Replaced actual generated code with '${error.replacement}'")
    }
  }

  private def extractTitle(swagger: Swagger): String = {
    (for {
      info <- Option(swagger.getInfo)
      title <- Option(info.getTitle)
    } yield title).getOrElse("No Info Title Provided")
  }

  private def extractControllers(swagger: Swagger, verbose: Boolean): (Seq[Controller], Seq[RoutedController]) = {
    def extractRoutedController(swagger: Swagger, path: Path, pathStr: String): RoutedController = {
      val controllerName = makeControllerName(pathStr)
      val singleInstance = isSingleInstance(pathStr)
      val scalaPath = swagger.basePath().fold(pathStr)(bp => s"$bp$pathStr".replace("//", "/")).replace("{", ":").replace("}", "")

      val methods = path.operations.map { case (opName, op: Operation) =>
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
        val allApplicableParameters = path.parameters.filterNot {
          pp =>
            op.parameters.exists(_.getName == pp.getName)
        } ++ op.parameters

        def getTypeName(param: Parameter): String = param.typeName match {
          case -\/(parseError) =>
            logParseError(parseError, verbose)
            parseError.replacement
          case \/-(tname) =>
            tname
        }

        def getEnumValues(param: Parameter): Option[ModeledEnum] = param match {
          case p: AbstractSerializableParameter[_] => Option(p.getEnum).map(e => ModeledEnum(e.asScala.toSeq))
          case _                                   => None
        }

        val params = allApplicableParameters.filter(p => Seq("query", "path").contains(p.getIn)).map { param =>
          val paramType = param.getIn match {
            case "query" => QueryParam
            case "path" => PathParam
          }
          Param(param.getName, getTypeName(param), param.getRequired, paramType, param.defaultValue, getEnumValues(param))
        }

        // This assumes a single body parameter only.
        val bodyOpt = allApplicableParameters.find(_.getIn == "body").map { param => Body(getTypeName(param)) }

        val headerParams = allApplicableParameters.filter(_.getIn == "header").map { param =>
          Param(param.getName, getTypeName(param), param.getRequired, HeaderParam, param.defaultValue, getEnumValues(param))
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
            body = bodyOpt)
      }
      RoutedController(scalaPath, Controller(controllerName, methods))
    }

    val paths = Option(swagger.getPaths).map(_.asScala.toSeq).getOrElse(Nil)

    val routedControllers = paths.map { case (pathStr, path) =>
      extractRoutedController(swagger, path, pathStr)
    }

    val controllers = routedControllers.groupBy(_.controller.name).map { case (name, rcontrollers) =>
      Controller(name, rcontrollers.flatMap(_.controller.methods))
    }.toSeq

    (controllers, routedControllers)
  }

  private def extractModelDefinitions(swagger: Swagger, routedControllers: Seq[RoutedController], verbose: Boolean): Seq[ModelDefinition] = {
    val unsortedModelDefinitions = {
      val typesForPatching = routedControllers.flatMap(_.controller.methods.filter(_.httpMethod == "PATCH").flatMap(_.bodyType)).toSet

      val realDefns: Seq[ModelDefinition] = swagger.definitionsMap.toList.map {
        case (modelName, model) =>
          // FIXME - change this to retain attribute order as per Swagger input...
          val attrs = model.properties.map { case (propName, prop) =>
            val (scalaType, required, refname) = prop.scalaType(modelName, propName) match {
              case -\/(parseError) =>
                logParseError(parseError, verbose)
                (parseError.replacement, parseError.required, None)
              case \/-(x) => x
            }
            ModelAttribute(propName, prop.swaggerType, scalaType, required, refname, prop.modeledEnum())
          }.toSeq

          ModelDefinition(modelName, attrs, typesForPatching.contains(modelName))
      }

      val synthDefns = Definitions.getSynthetics(swagger)(logParseError(_, verbose))

      realDefns ++ synthDefns
    }

    val (sorted, cyclicDefs) = Definitions.order(unsortedModelDefinitions.toSeq) { defnNameWithCycle =>
      if (verbose) {
        println(s"WARN - $defnNameWithCycle will be generated as 'def' in JsonOps (due to detected potential circular reference)")
      }
    }
    val cyclicRefsByName = cyclicDefs.map(d => d.name -> d.referencedNames).toMap
    sorted.map(_.md).map { md =>
      md.copy(cyclicReferences = cyclicRefsByName.get(md.name))
    }
  }

  private def extractAllIds(definitions: Seq[ModelDefinition], controllers: Seq[Controller]): Map[String, Id] = {
    def nameOf(defn: ModelDefinition, attr: ModelAttribute) = attr.name match {
      case _ if attr.name == "id" => s"${defn.name}_id"
      case _                      => attr.name
    }

    val fromDefinitions = for {
      definition <- definitions
      attribute  <- definition.attributes
      name        = if (attribute.name == "id") s"${definition.name}_id" else attribute.name
    } yield Id(nameOf(definition, attribute), attribute.scalaType).map(name -> _)

    val fromControllers = for {
      controller <- controllers
      method     <- controller.methods
      parameter  <- method.params ++ method.headerParams
    } yield Id(parameter.name, parameter.baseType).map(parameter.name -> _)

    (fromDefinitions ++ fromControllers).flatten.toMap
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

  private def singularize(str: String) = {
    if (str.endsWith("es")) str.dropRight(2)
    else if (str.endsWith("s")) str.dropRight(1)
    else str
  }

  private def isSingleInstance(path: String): Boolean = path.endsWith("}") || path.endsWith("}/")

  private def resourceToClassName(name: String) = name.split("-").map(w => w.take(1).toUpperCase + w.drop(1)).mkString("")

}
