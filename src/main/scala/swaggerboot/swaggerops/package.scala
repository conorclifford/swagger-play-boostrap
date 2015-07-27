package swaggerboot

import io.swagger.models.auth.SecuritySchemeDefinition
import io.swagger.models.parameters._
import io.swagger.models.properties.{ObjectProperty, ArrayProperty, RefProperty, Property}
import io.swagger.models.{Operation, Path, Model, Swagger}
import scala.collection
import scala.collection.JavaConverters._
import scala.collection.generic.Subtractable
import scala.collection.mutable

import scalaz._

case class ParseError(msg: String, replacement: String = "FIXME", required: Boolean = false)

//
// FIXME code in this file is overly complex and bit of a mess
// FIXME - as it tries to check for errors (based on internal limitations, etc)
// FIXME while also still allowing code generations to proceed (with simply "FIXME" values where the errors were
// FIXME detected.
// FIXME this error detection/replacement is also done in such a way as to allow a single-pass over API spec to
// FIXME extract the errors once, rather than logging errors as they pass, to have each error only logged once.
// FIXME **needs cleanup**
//

package object swaggerops {
  implicit class SwaggerOps(val swagger: Swagger) extends AnyVal {
    def routedControllers(): Iterable[RoutedController] = routedControllersWithErrors._1

    def routedControllersWithErrors(): (Iterable[RoutedController], Iterable[ParseError]) = {
      swagger.getPaths.asScala.map {
        case (pathStr, path) =>
          path.toController(swagger, basePath, pathStr)
      }.foldLeft((Seq.empty[RoutedController], Seq.empty[ParseError])) {
        case ((controllers, allErrors), (controller, errors)) =>
          (controllers :+ controller, allErrors ++ errors)
      }
    }

    def securityDefinitions(): Map[String, SecuritySchemeDefinition] = Option(swagger.getSecurityDefinitions).map(_.asScala.toMap).getOrElse(Map.empty)

    def definitions(): Iterable[ModelDefinition] = definitionsWithErrors._1

    def definitionsWithErrors(): (Iterable[ModelDefinition], Iterable[ParseError]) = {
      val typesForPatching = routedControllers.flatMap(_.methods.filter(_.httpMethod == "PATCH").flatMap(_.bodyType)).toSet

      val definitionsAndErrors: List[(ModelDefinition, Seq[ParseError])] = swagger.getDefinitions.asScala.toList.map {
        case (name, model) =>
          val attrsAndErrors = model.properties.map { case (propName, prop) =>
            val (scalaType, required, error: Option[ParseError]) = prop.scalaType() match {
              case -\/(parseError) => (parseError.replacement, parseError.required, Some(parseError))
              case \/-((stype: String, required: Boolean)) => (stype, required, None)
            }
            (ModelAttribute(propName, scalaType, required), error)
          }

          // FIXME - change this to retain attribute order as per Swagger input...
          val (attrs, errors) = attrsAndErrors.foldLeft((Seq.empty[ModelAttribute], Seq.empty[ParseError])) {
            case ((attrs, errors), (attr, errOpt)) =>
              (attrs :+ attr, errOpt.fold(errors)(errors :+ _))
          }

          (ModelDefinition(name, attrs.toList, typesForPatching.contains(name)), errors)
      }

      definitionsAndErrors.foldLeft((Seq.empty[ModelDefinition], Seq.empty[ParseError])) {
        case ((definitions, allErrors), (defn, errors)) =>
          (definitions :+ defn, allErrors ++ errors)
      }
    }

    def basePath: Option[String] = Option(swagger.getBasePath)

    def controllers(): Iterable[Controller] = {
      swagger.routedControllers.groupBy(_.name).map {
        case (name, rcontrollers) =>
          Controller(name, rcontrollers.flatMap(_.methods).toSeq)
      }
    }

    def routesFile(): String = routedControllers.toSeq.sortBy(_.path).map(_.routesFileEntries.mkString("\n")).mkString("\n\n")

    def modelsFile(): String =
      s"""package models
         |
         |${definitions.map(_.scalaClassImpl).mkString("\n")}
         """.stripMargin

    def jsonFile(): String =
      s"""package models
         |
         |import play.api.libs.json._
         |import play.api.libs.functional.syntax._
         |
         |object JsonOps {
         |  ${definitions.map(_.playJsonImpl).mkString("  \n")}
          |}
       """.stripMargin
  }

  implicit class ModelOps(val model: Model) extends AnyVal {
    def properties() = Option(model.getProperties).map(_.asScala.toMap).getOrElse(Map.empty)
  }

  implicit class PropertyOps(val property: Property) extends AnyVal {
    def scalaType(force: Boolean = false): ParseError \/ (String, Boolean) = {
      val rawType: ParseError \/ String = property.getType match {
        case "string" => \/-("String")
        case "integer" => \/-("Int")
        case "boolean" => \/-("Boolean")
        case "ref" => \/-(getType(property.asInstanceOf[RefProperty]))
        case "array" => {
          getType(property.asInstanceOf[ArrayProperty]).fold(
            l = error => -\/(error.copy(replacement=s"Seq[${error.replacement}]", required = property.getRequired)),
            r = v => \/-(s"Seq[$v]")
          )
        }
        case "object" => getType(property.asInstanceOf[ObjectProperty])
        case x =>
          -\/(ParseError(s"Unsupported property type '$x'"))
      }

      rawType.map(_ -> property.getRequired)
    }

    private def getType(arrayProp: ArrayProperty): ParseError \/ String = arrayProp.getItems.scalaType(true).map(_._1)
    private def getType(refProp: RefProperty): String = refProp.getSimpleRef
    private def getType(objProp: ObjectProperty): ParseError \/ String = {
      -\/(ParseError("currently incapable of handing embedded properties in an object", required = objProp.getRequired))
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
      val (methods, errors) = path.operations.map { case (opName, op: Operation) =>
        val methodName = opName match {
          case "GET" if singleInstance  => "get"
          case "GET"                    => "list"
          case "PUT" if singleInstance  => "upsert"
          case "PUT"                    => "upsertUnknown"
          case x                        => x.toLowerCase
        }

        // Path parameters apply to all Operations (can be overriden (by name), but cannot be removed)
        val allApplicableParameters = parameters.filterNot {
          pp =>
            op.parameters.exists(_.getName == pp.getName)
        } ++ op.parameters

        val (params, paramErrors) = allApplicableParameters.filter(p => Seq("query", "path").contains(p.getIn)).map { param =>
          val (typeName, error) = param.typeName match {
            case -\/(parseError) => (parseError.replacement, Some(parseError))
            case \/-(tname) => (tname, None: Option[ParseError])
          }
          (Param(param.getName, typeName, param.getRequired), error)
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
          (Param(param.getName, typeName, param.getRequired), error)
        }.foldLeft((Seq.empty[Param], Seq.empty[ParseError])) {
          case ((params, errors), (param, errOpt)) =>
            (params :+ param, errOpt.fold(errors)(errors :+ _))
        }

        // Produces list can override swagger spec. wide (including wiping, with local empty list)...
        val produces = (Option(op.getProduces) orElse Option(swagger.getProduces)).map(_.asScala).getOrElse(Nil)

        (
          Method(
            httpMethod = opName,
            name = methodName,
            params = params,
            headerParams = headerParams,
            produces = produces,
            body = bodyOpt),
          paramErrors ++ headerErrors ++ bodyError.toList
        )
      }.foldLeft((Seq.empty[Method], Seq.empty[ParseError])) {
        case ((methods, allErrors), (method, errors)) =>
          (methods :+ method, allErrors ++ errors)
      }

      val scalaPath = basePath.fold(pathStr)(bp => s"$bp$pathStr".replace("//", "/")).replace("{", ":").replace("}", "")
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

    private def isSingleInstance(path: String): Boolean = path.endsWith("}")

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
  }

  implicit class ParameterOps(val param: Parameter) extends AnyVal {
    def typeName(): ParseError \/ String = param match {
      case p: QueryParameter => scalaType(p.getType)
      case p: PathParameter => scalaType(p.getType)
      case p: BodyParameter => getReference(p)
      case p: HeaderParameter => scalaType(p.getType)
      case _ =>
        -\/(ParseError(s"Unsupported swaggerboot.Param type ${param.getClass.getName}"))
    }

    private def getReference(p: BodyParameter): ParseError \/ String = {
      val refOpt = for {
        schema <- Option(p.getSchema)
        ref <- Option(schema.getReference)
      } yield { ref }

      refOpt match {
        case None => -\/(ParseError("Missing Schema/Ref for a Body Parameter"))
        case Some(ref) => \/-(ref)
      }
    }

    private def scalaType(swaggerType: String): ParseError \/ String = swaggerType match {
      case "string" => \/-("String")
      case "integer" => \/-("Int")
      case "boolean" => \/-("Boolean")
      case x =>
        -\/(ParseError("Unsupported parameter type '$x'", replacement = s"FIXME[$x]"))
    }
  }
}
