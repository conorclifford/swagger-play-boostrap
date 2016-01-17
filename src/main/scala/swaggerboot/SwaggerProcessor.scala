package swaggerboot

import io.swagger.models.Swagger

case class SwaggerRep(title: String,
                      controllers: Seq[Controller],
                      routedControllers: Seq[RoutedController],
                      containsCsvParams: Boolean,
                      modelDefinitions: Seq[ModelDefinition],
                      ids: Map[String, Id])

object SwaggerProcessor {
  import swaggerops._

  def process(swagger: Swagger, verbose: Boolean = true): SwaggerRep = {

    val (routedControllers, routedControllerErrors) = swagger.routedControllersWithErrors()
    val (unsortedModelDefinitions, modelDefintionErrors) = swagger.definitionsWithErrors()

    val modelDefinitions = {
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

    val controllers = routedControllers.groupBy(_.controller.name).map { case (name, rcontrollers) =>
      Controller(name, rcontrollers.flatMap(_.controller.methods))
    }.toSeq

    val containsCsvParams = routedControllers.exists(_.controller.methods.exists(_.params.exists(_.baseType == "Seq[String]")))

    if (verbose) {
      (routedControllerErrors ++ modelDefintionErrors).foreach {
        case ParseError(msg, replacement, _) =>
          System.err.println(s"WARN - $msg - Replaced actual generated code with '$replacement'")
      }
    }

    val ids = extractAllIds(modelDefinitions, controllers)

    val title = (for {
      info <- Option(swagger.getInfo)
      title <- Option(info.getTitle)
    } yield title).getOrElse("No Info Title Provided")

    SwaggerRep(title, controllers, routedControllers, containsCsvParams, modelDefinitions.toSeq, ids)
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

}
