package swaggerboot

import io.swagger.models.auth.SecuritySchemeDefinition
import io.swagger.models.parameters._
import io.swagger.models.properties.{ObjectProperty, ArrayProperty, RefProperty, Property}
import io.swagger.models._
import scala.annotation.tailrec
import scala.collection.JavaConverters._

import scalaz._
import Scalaz._

case class ParseError(msg: String, replacement: String = "play.api.libs.json.JsValue", required: Boolean = false)

//
// FIXME code in this file is overly complex and bit of a mess
// FIXME - as it tries to check for errors (based on internal limitations, etc)
// FIXME while also still allowing code generations to proceed (with simply "FIXME" values where the errors were
// FIXME detected.
// FIXME this error detection/replacement is also done in such a way as to allow a single-pass over API spec to
// FIXME extract the errors once, rather than logging errors as they pass, to have each error only logged once.
//
// FIXME **needs A LOT OF cleanup**
//

package object swaggerops {
  private case class SortingDefinition(md: ModelDefinition, references: Seq[String]) {
    val name = md.name
  }

  implicit class SwaggerOps(val swagger: Swagger) extends AnyVal {
    def routedControllers(): Iterable[RoutedController] = routedControllersWithErrors._1

    def routedControllersWithErrors(): (Iterable[RoutedController], Iterable[ParseError]) = {
      Option(swagger.getPaths).map(_.asScala).getOrElse(Nil).map {
        case (pathStr, path) =>
          path.toController(swagger, basePath, pathStr)
      }.foldLeft((Seq.empty[RoutedController], Seq.empty[ParseError])) {
        case ((controllers, allErrors), (controller, errors)) =>
          (controllers :+ controller, allErrors ++ errors)
      }
    }

    def securityDefinitions(): Map[String, SecuritySchemeDefinition] = Option(swagger.getSecurityDefinitions).map(_.asScala.toMap).getOrElse(Map.empty)

    private def orderDefinitions(unsorted: Seq[ModelDefinition], withCircularWarnings: Boolean = false): (Seq[SortingDefinition], Seq[(String, Set[String])]) = {

      // calculate the referenced definitions for each definition...
      val defsWithRefNames: Seq[SortingDefinition] = unsorted.map { d =>
        SortingDefinition(d, d.attributes.flatMap(_.referencedName))
      }

      type Bump = (String, Set[String])

      @tailrec
      def recur(remaining: Seq[SortingDefinition], acc: Seq[SortingDefinition], cycleAcc: Seq[Bump], bumps: Seq[Bump]): (Seq[SortingDefinition], Seq[Bump]) = {

        def inAcc(name: String, accumulator: Seq[SortingDefinition] = acc) = accumulator.exists(_.name == name)

        // Cleanse bumps
        val cleansedBumps = bumps.foldLeft(Seq.empty[Bump]) {
          case (newBumps, (bumpedName, bumpedRefs)) if bumpedRefs.forall(inAcc(_)) =>
            newBumps
          case (newBumps, (bumpedName, bumpedRefs)) =>
            newBumps :+(bumpedName, bumpedRefs.filterNot(inAcc(_)))
        }

        def isBumped(name: String) = bumps.exists(_._1 == name)
        def allBumped(refs: Set[String]) = refs.forall(isBumped)
        def refsAllBumped(name: String) = bumps.exists(b => b._1 == name && allBumped(b._2))
        def isCircular(bump: Bump) = allBumped(bump._2) && bump._2.forall(refsAllBumped)

        val circularRefs: Set[String] = cleansedBumps.filter(isCircular).map(_._1).toSet

        val newCycleAcc = cycleAcc ++ cleansedBumps.filter(isCircular)

        if (withCircularWarnings) {
          circularRefs.foreach { r => println(s"WARN - $r involved in circular reference - adding to output in arbitrary position - may require some manual reordering in JsonOps") }
        }
        val newBumps = bumps.filterNot(b => circularRefs.contains(b._1))
        val newAcc = acc ++ remaining.filter(r => circularRefs.contains(r.name))
        val newRemaining = remaining.filterNot(r => circularRefs.contains(r.name))

        // Process remaining for head/tail
        newRemaining match {
          case Nil =>
            (newAcc, newCycleAcc)
          case head +: tail if head.references.forall(r => inAcc(r, newAcc) || head.name == r) =>
            recur(tail, newAcc :+ head, newCycleAcc, newBumps)
          case head +: tail =>
            recur(tail :+ head, newAcc, newCycleAcc, newBumps :+ (head.name, head.references.toSet))
        }
      }

      recur(defsWithRefNames, Nil, Nil, Nil)
    }

    /**
     * A synthethic model is one that is built without an actual "Definition", but rather, in response to "object" properties, with
     * corresponding embedded "properties"...
     * It gets named based on its "parent" which may itself be synthetic, but no matter.
     * These need to get merged into the real "definitions" before ordering - these are needed for all code generation...
     *
     * this gathers all parse errors as we go, and build up definitions using "replacements"
     */
    private def synthethicDefinitions(): (Seq[ModelDefinition], Seq[ParseError]) = {
      def makeDefinition(defName: String, prop: ObjectProperty): (ModelDefinition, Seq[ParseError]) = {
        val attrsAndErrors = Option(prop.getProperties).map(_.asScala).getOrElse(Nil).map { case (propName, prop) =>
          val (scalaType, required, refname, error) = prop.scalaType(defName, propName) match {
            case -\/(parseError) => (parseError.replacement, parseError.required, None, Some(parseError))
            case \/-((stype, required, refname)) => (stype, required, refname, None)
          }
          (ModelAttribute(propName, scalaType, required, refname), error)
        }

        // FIXME - change this to retain attribute order as per Swagger input...
        val (attrs, errors) = attrsAndErrors.foldLeft((Seq.empty[ModelAttribute], Seq.empty[ParseError])) {
          case ((attrs, errors), (attr, errOpt)) =>
            (attrs :+ attr, errOpt.fold(errors)(errors :+ _))
        }

        (ModelDefinition(defName, attrs.toList, false), errors)
      }

      def getSynthethics(parentName: String, namedProperties: Seq[(String, Property)], defAcc: Seq[ModelDefinition] = Nil, errAcc: Seq[ParseError] = Nil): (Seq[ModelDefinition], Seq[ParseError]) = {
        namedProperties match {
          case Nil => (defAcc, errAcc)
          case head +: tail =>
            val (newDefs, newErrs) = head match {
              case (propName, oprop: ObjectProperty) =>
                val (defs, errs) = getSynthethics(
                  synthethicModelName(parentName, propName),
                  Option(oprop.getProperties).map(_.asScala).getOrElse(Nil).toSeq)
                val (newDef, newErrors) = makeDefinition(synthethicModelName(parentName, propName), oprop)
                (defs :+ newDef, errs ++ newErrors)
              case (propName, aprop: ArrayProperty) if aprop.getItems.isInstanceOf[ObjectProperty] =>
                val oprop = aprop.getItems.asInstanceOf[ObjectProperty]
                val (defs, errs) = getSynthethics(
                  synthethicModelName(parentName, propName),
                  Option(oprop.getProperties).map(_.asScala).getOrElse(Nil).toSeq)
                val (newDef, newErrors) = makeDefinition(synthethicModelName(parentName, propName), oprop)
                (defs :+ newDef, errs ++ newErrors)
              case _ =>
                (Nil, Nil)
            }
            getSynthethics(parentName, tail, defAcc ++ newDefs, errAcc ++ newErrs)
        }
      }

      Option(swagger.getDefinitions).map(_.asScala).getOrElse(Map.empty).toList.foldLeft((Seq.empty[ModelDefinition], Seq.empty[ParseError])) {
        case ((defsAcc, errAcc), (name, model)) =>
          val (defs, errs) = getSynthethics(name, model.properties.toSeq)
          (defsAcc ++ defs, errAcc ++ errs)
      }
    }

    def definitions(withCyclicWarnings: Boolean = false): Seq[ModelDefinition] = {
      val (sorted, cyclicDefs) = orderDefinitions(definitionsWithErrors._1.toSeq, withCyclicWarnings)
      val cyclicRefsByName = cyclicDefs.toMap
      sorted.map(_.md).map { md =>
        md.copy(cyclicReferences = cyclicRefsByName.get(md.name))
      }
    }

    def definitionsWithErrors(): (Iterable[ModelDefinition], Iterable[ParseError]) = {
      val typesForPatching = routedControllers.flatMap(_.methods.filter(_.httpMethod == "PATCH").flatMap(_.bodyType)).toSet

      val definitionsAndErrors: List[(ModelDefinition, Seq[ParseError])] = swagger.getDefinitions.asScala.toList.map {
        case (modelName, model) =>
          val attrsAndErrors = model.properties.map { case (propName, prop) =>
            val (scalaType, required, refname, error) = prop.scalaType(modelName, propName) match {
              case -\/(parseError) => (parseError.replacement, parseError.required, None, Some(parseError))
              case \/-((stype, required, refname)) => (stype, required, refname, None)
            }
            (ModelAttribute(propName, scalaType, required, refname), error)
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

      val (synthDefns, synthErrors) = synthethicDefinitions()

      (realDefns ++ synthDefns, realErrors ++ synthErrors)
    }

    def basePath: Option[String] = Option(swagger.getBasePath)

    def controllers(): Iterable[Controller] = {
      swagger.routedControllers.groupBy(_.name).map {
        case (name, rcontrollers) =>
          Controller(name, rcontrollers.flatMap(_.methods).toSeq)
      }
    }

    def routesFile(): String = routedControllers.toSeq.sortBy(_.path).map(_.routesFileEntries.mkString("\n")).mkString("\n\n")

    def modelsFile(packageName: String): String =
      s"""package $packageName
         |
         |${definitions().map(_.scalaClassImpl).mkString("\n")}
         """.stripMargin

    def jsonFile(packageName: String): String = {
      s"""package $packageName
         |
         |import play.api.libs.json._
         |import play.api.libs.functional.syntax._
         |
         |object JsonOps {
         |  ${definitions(true).map(_.playJsonImpl).mkString("  \n")}
          |}
       """.stripMargin
    }

    def clientFile(packageName: String): String = {

      def toIdentifier(name: String) = name.head.toLower +: name.tail

      s"""package $packageName
         |
         |import play.api.libs.json.Json
         |import play.api.libs.ws.WSClient
         |
         |import scala.concurrent.{ExecutionContext, Future}
         |import scalaz._
         |import Scalaz._
         |
         |object Result {
         |  case class Success[B](responseCode: Int, message: Option[String], body: Option[B] = None)
         |  case class Error[B](responseCode: Int, message: Option[String] = None, body: Option[B] = None)
         |}
         |
         |${controllers.map(_.clientTrait).mkString("\n")}
         |
         |class Client(baseUrl: String, wsClient: => WSClient = play.api.libs.ws.WS.client(play.api.Play.current)) {
         |  import JsonOps._
         |  import play.api.Play.current
         |
         |${Indenter.indent(controllers.map(rc => s"val ${toIdentifier(rc.name)}: ${rc.name}Client = ${rc.name}Client").mkString("\n"))}
         |
         |${Indenter.indent(controllers.map(_.clientImpl).mkString("\n"))}
         |}
       """.stripMargin
    }

  }

  implicit class ModelOps(val model: Model) extends AnyVal {
    def properties() = Option(model.getProperties).map(_.asScala.toMap).getOrElse(Map.empty)
  }

  implicit class PropertyOps(val property: Property) extends AnyVal {
    def scalaType(parentName: String, propName: String): ParseError \/ (String, Boolean, Option[String]) = {
      val rawType: ParseError \/ (String, Option[String]) = property.getType match {
        case "string" => ("String", None).right
        case "integer" => ("Int", None).right
        case "boolean" => ("Boolean", None).right
        case "ref" => getType(property.asInstanceOf[RefProperty])
        case "array" => {
          getType(parentName, propName, property.asInstanceOf[ArrayProperty]).fold(
            l = error => error.copy(replacement = s"Seq[${error.replacement}]", required = property.getRequired).left,
            r = v => (s"Seq[${v._1}]", v._2).right
          )
        }
        case "object" => getType(synthethicModelName(parentName, propName), property.asInstanceOf[ObjectProperty])
        case x =>
          ParseError(s"Unsupported property type '$x'").left
      }

      rawType.map { case (n, rn) => (n, property.getRequired, rn) }
    }

    private def getType(parentName: String, propName: String, arrayProp: ArrayProperty): ParseError \/ (String, Option[String]) = {
      arrayProp.getItems.scalaType(parentName, propName).map(st => (st._1, st._3))
    }
    private def getType(refProp: RefProperty): ParseError \/ (String, Option[String]) = (refProp.getSimpleRef, Some(refProp.getSimpleRef)).right
    private def getType(name: String, objProp: ObjectProperty): ParseError \/ (String, Option[String]) = {
      // Simply refer to the synthentic name here - note giving reference to the synthethic here for ordering above
      (name, Some(name)).right
    }
  }

  private def synthethicModelName(parentName: String, propertyName: String): String = {
    def camelOf(name: String): String = name.split("_").map { s => s.head.toString.toUpperCase ++ s.tail}.mkString("")
    s"${camelOf(parentName)}${camelOf(propertyName)}"
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

        val methodName = opName match {
          case "GET" if singleInstance  => "get"
          case "GET" if hasBodyArg      => "get"
          case "GET"                    => "list"
          case "PUT" if singleInstance  => "put"
          case "PUT"                    => "putUnknown"
          case x                        => x.toLowerCase
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
          (Param(param.getName, typeName, param.getRequired, paramType), error)
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
          (Param(param.getName, typeName, param.getRequired, HeaderParam), error)
        }.foldLeft((Seq.empty[Param], Seq.empty[ParseError])) {
          case ((params, errors), (param, errOpt)) =>
            (params :+ param, errOpt.fold(errors)(errors :+ _))
        }

        // Produces list can override swagger spec. wide (including wiping, with local empty list)...
        val produces = (Option(op.getProduces) orElse Option(swagger.getProduces)).map(_.asScala).getOrElse(Nil)

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
            returnValues = returnValues,
            body = bodyOpt),
          paramErrors ++ headerErrors ++ bodyError.toList
        )
      }.foldLeft((Seq.empty[Method], Seq.empty[ParseError])) {
        case ((methods, allErrors), (method, errors)) =>
          (methods :+ method, allErrors ++ errors)
      }

      (RoutedController(scalaPath, controllerName, methods), errors)
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

    def responses(): Map[String, Response] = Option(op.getResponses).map(_.asScala.toMap).getOrElse(Map.empty)
  }

  implicit class ParameterOps(val param: Parameter) extends AnyVal {
    def typeName(): ParseError \/ String = param match {
      case p: QueryParameter => scalaType(p.getType)
      case p: PathParameter => scalaType(p.getType)
      case p: BodyParameter => getReference(p)
      case p: HeaderParameter => scalaType(p.getType)
      case _ =>
        ParseError(s"Unsupported swaggerboot.Param type ${param.getClass.getName}").left
    }

    private def getReference(p: BodyParameter): ParseError \/ String = {
      def processModel(mod: Model): Option[String] = mod match {
        case rmod: RefModel => Option(rmod.getSimpleRef)
        case amod: ArrayModel => amod.getItems match {
          case ref: RefProperty => Option(ref.getSimpleRef).map(m => s"Seq[$m]")
          case _ => None
        }
        case _ => None
      }

      Option(p.getSchema).flatMap(processModel) \/> ParseError("Missing Schema/Ref for a Body Parameter")
    }

    private def scalaType(swaggerType: String): ParseError \/ String = swaggerType match {
      case "string" => "String".right
      case "integer" => "Int".right
      case "boolean" => "Boolean".right
      case x =>
        ParseError("Unsupported parameter type '$x'", replacement = s"FIXME[$x]").left
    }
  }
}
