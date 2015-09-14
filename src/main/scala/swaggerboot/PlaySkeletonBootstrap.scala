package swaggerboot

import java.io.{File, PrintWriter}

import io.swagger.models.Swagger
import io.swagger.models.auth.OAuth2Definition
import io.swagger.parser.SwaggerParser

import swaggerops._

object PlaySkeletonBootstrap extends App {
  if (!(args.length == 2 || (args.length == 3 && args(2) == "-client"))) {
    System.err.println(
    """
      |Usage: PlaySkeletonBootstrap <swagger-spec> <output-directory> [-client]
      |    - <swagger-spec> - correctly formed Swagger API Specification - this tool has been only tested with YAML files.
      |    - <output-directory> must not exist (autocreated by this tool)
      |    - [-client] specifies that the client code should be generated (server code is generated by default)
    """.stripMargin)
    System.exit(1)
  }

  val swaggerSpec = args(0)
  val outputDir = new File(args(1))
  val genServerCode = args.length == 2

  if (!new File(swaggerSpec).isFile) {
    System.err.println(s"ERROR - swagger specification file '$swaggerSpec' is not a file...")
    System.exit(1)
  }

  if (outputDir.exists) {
    System.err.println(s"ERROR - output directory already exists - overwrite is not yet supported")
    System.exit(1)
  }

  val swagger: Swagger = new SwaggerParser().read(swaggerSpec)

  private def logCyclicDefinitionWarning(defnName: String): Unit = {
    println(s"WARN - $defnName will be generated as 'def' in JsonOps (due to detected potential circular reference)")
  }

  // FIXME
//  swagger.securityDefinitions.foreach {
//    case (name, oauth2: OAuth2Definition) =>
//      println(s"Security defn: OAUTH2: name = $name, flow = ${oauth2.getFlow}, scopes = ${oauth2.getScopes}")
//    case (name, obj) =>
//      println(s"Security defn: ${obj.getClass.getName}, name = $name")
//  }

  // Get the errors first, and log to stderr.
  (swagger.routedControllersWithErrors()._2 ++ swagger.definitionsWithErrors()._2).foreach {
    case ParseError(msg, replacement, _) =>
      System.err.println(s"WARN - $msg - Replaced actual generated code with '$replacement'")
  }

  if (genServerCode) {
    val confDir = new File(outputDir, "conf")
    val appDir = new File(outputDir, "app")
    val controllersDir = new File(appDir, "controllers")
    val delegateTraitsDir = new File(controllersDir, "delegates")
    val modelsDir = new File(appDir, "models")

    Seq(confDir, controllersDir, delegateTraitsDir, modelsDir).map { dir =>
      (dir, dir.mkdirs())
    }.find(!_._2).foreach { case (dir, _) =>
      System.err.println(s"Failed to create directory '$dir'")
      System.exit(1)
    }

    writingToFile(new File(confDir, "routes")) {
      _.println(codegen.Routes.generate(swagger.routedControllers))
    }

    swagger.controllers.foreach { c =>
      writingToFile(new File(controllersDir, s"${c.name}.scala")) {
        _.println(codegen.Controller.generate(c))
      }
    }

    Delegates.extract(swagger.controllers).foreach { delegate =>
      writingToFile(new File(delegateTraitsDir, s"${delegate.className}.scala")) {
        _.println(codegen.ControllerDelegateTraits.generate(delegate))
      }
    }

    writingToFile(new File(modelsDir, "Models.scala")) {
      _.println(codegen.Models.generate("models", swagger.definitions()))
    }

    writingToFile(new File(modelsDir, "JsonOps.scala")) {
      _.println(codegen.JsonOps.generate("models", swagger.definitions(logCyclicDefinitionWarning)))
    }

  } else {
    val title = for {
      info <- Option(swagger.getInfo)
      title <- Option(info.getTitle)
    } yield title

    val clientPackageName = title.getOrElse("No Info Title Provided Package").replace(".", "").replace(" ", "_").toLowerCase
    val clientDir = new File(new File(outputDir, "clients"), clientPackageName)

    Seq(clientDir).map { dir =>
      (dir, dir.mkdirs())
    }.find(!_._2).foreach { case (dir, _) if !dir.isDirectory =>
      System.err.println(s"Failed to create directory '$dir'")
      System.exit(1)
    }

    writingToFile(new File(clientDir, "Models.scala")) {
      _.println(codegen.Models.generate(s"clients.$clientPackageName", swagger.definitions()))
    }

    writingToFile(new File(clientDir, "JsonOps.scala")) {
      _.println(codegen.JsonOps.generate(s"clients.$clientPackageName", swagger.definitions(logCyclicDefinitionWarning)))
    }

    writingToFile(new File(clientDir, "Client.scala")) {
      _.println(codegen.Client.generate(s"clients.$clientPackageName", swagger.controllers()))
    }
  }

  private def writingToFile(file: File)(fn: PrintWriter => Unit): Unit = {
    val writer = new PrintWriter(file)
    try {
      fn(writer)
    } finally {
      writer.close()
    }
  }
}

