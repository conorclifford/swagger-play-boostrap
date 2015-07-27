package swaggerboot

import java.io.{File, PrintWriter}

import io.swagger.models.Swagger
import io.swagger.models.auth.OAuth2Definition
import io.swagger.parser.SwaggerParser

import swaggerops._

object PlaySkeletonBootstrap extends App {
  if (args.length != 2) {
    System.err.println(
    """
      |Usage: PlaySkeletonBootstrap <swagger-spec> <output-directory>
      |    - <swagger-spec> - correctly formed Swagger API Specification - this tool has been only tested with YAML files.
      |    - <output-directory> must not exist (autocreated by this tool)
    """.stripMargin)
    System.exit(1)
  }

  val swaggerSpec = args(0)
  val outputDir = new File(args(1))

  if (!new File(swaggerSpec).isFile) {
    System.err.println("ERROR - swagger specification file '$swaggerSpec' is not a file...")
    System.exit(1)
  }

  if (outputDir.exists) {
    System.err.println("ERROR - output directory already exists - overwrite is not yet supported")
    System.exit(1)
  }

  val swagger: Swagger = new SwaggerParser().read(swaggerSpec)

  // FIXME
//  swagger.securityDefinitions.foreach {
//    case (name, oauth2: OAuth2Definition) =>
//      println(s"Security defn: OAUTH2: name = $name, flow = ${oauth2.getFlow}, scopes = ${oauth2.getScopes}")
//    case (name, obj) =>
//      println(s"Security defn: ${obj.getClass.getName}, name = $name")
//  }

  val confDir = new File(outputDir, "conf")
  val appDir = new File(outputDir, "app")
  val controllersDir = new File(appDir, "controllers")
  val modelsDir = new File(appDir, "models")

  Seq(confDir, controllersDir, modelsDir).map { dir =>
    (dir, dir.mkdirs())
  }.find(!_._2).foreach { case (dir, _) =>
    System.err.println(s"Failed to create directory '$dir'")
    System.exit(1)
  }

  // Get the errors first, and log to stderr.
  (swagger.routedControllersWithErrors()._2 ++ swagger.definitionsWithErrors()._2).foreach {
    case ParseError(msg, replacement, _) =>
      System.err.println(s"WARN - $msg - Replaced actual generated code with '$replacement'")
  }

  // Assuming the errors are mapped to simply "broken" generated code, go ahead and generate the code skeleton.

  writingToFile(new File(confDir, "routes")) {
    _.println(swagger.routesFile())
  }

  swagger.controllers.foreach { c =>
    writingToFile(new File(controllersDir, s"${c.name}.scala")) {
      _.println(c.scalaImpl)
    }
  }

  writingToFile(new File(modelsDir, "Models.scala")) {
    _.println(swagger.modelsFile())
  }

  writingToFile(new File(modelsDir, "JsonOps.scala")) {
    _.println(swagger.jsonFile())
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

