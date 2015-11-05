package swaggerboot

import java.io.{File, PrintWriter}

import io.swagger.models.Swagger
import io.swagger.models.auth.OAuth2Definition
import io.swagger.parser.SwaggerParser
import io.swagger.util.Json
import org.apache.commons.io.FileUtils
import org.yaml.snakeyaml.Yaml

import swaggerops._

import scala.io.Source

object SwaggerCodeGenerator extends App {
  val IllegalFile = new File("illegal")
  case class Config(swaggerSpec: File = IllegalFile,
                    outputDir: File = IllegalFile,
                    genServerCode: Boolean = true,
                    replace: Boolean = false,
                    generatePlay23Code: Boolean = false,
                    generateDelegates: Boolean = true,
                    stubFullPlayApp: Boolean = false,
                    withHealthCheck: Boolean = false)

  val parser = new scopt.OptionParser[Config]("SwaggerCodeGenerator"){
    opt[File]("api") action { case (x, c) => c.copy(swaggerSpec = x) } required() maxOccurs(1) text("Swagger API specification (JSON or YAML)")
    opt[File]('o', "output-directory") action { case (x, c) => c.copy(outputDir = x) } required() maxOccurs(1) text("output directory")
    opt[Unit]("client") action { case (_, c) => c.copy(genServerCode = false) } text("specify that the client code should be generated (server code is generated by default)")
    opt[Unit]("replace") action { case (_, c) => c.copy(replace = true) } text("specifies to replace existing sources - note currently ONLY AVAILABLE with -client")
    opt[Unit]("play23") action { case (_, c) => c.copy(generatePlay23Code = true) } text("requests the generated code to be generated for play2.3  - note, this is currently ONLY AVAILABLE with -client, and is completely experimental")
    opt[Unit]("nodelegates") action { case (_, c) => c.copy(generateDelegates = false) } text("request the generated server code not have delegates workflow prepared")
    opt[Unit]("fullplaystub") action { case (_, c) => c.copy(stubFullPlayApp = true) } text("generate a stub for a full play app, including SBT, configuration, etc.")
    opt[Unit]("healthcheck") action { case (_, c) => c.copy(withHealthCheck = true) } text("Include simple healthcheck support (server only)")
    checkConfig { c =>
      if (c.genServerCode && c.replace) failure("Cannot replace code in server mode")
      else if (c.genServerCode && c.generatePlay23Code) failure("cannot generate play23 code in server mode")
      else if (c.swaggerSpec == IllegalFile || !c.swaggerSpec.isFile || !c.swaggerSpec.canRead) failure("Swagger API must be specified as real/readable file")
      else if (c.outputDir == IllegalFile) failure("output directory must be specified")
      else if (!c.replace && c.outputDir.exists()) failure("specified output already exists")
      else if (!c.genServerCode && c.withHealthCheck) failure("Cannot request healthcheck generation for client code")
      else success
    }
  }

  val configopt: Option[Config] = parser.parse(args, Config())
  if (configopt.isEmpty) System.exit(1) // This is ugly :(
  implicit val config = configopt.get

  val swagger: Swagger = new SwaggerParser().read(config.swaggerSpec.getAbsolutePath)

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

  def createDirsWithCheck(dirs: File*) = {
    dirs.map { dir =>
      (dir, dir.mkdirs())
    }.find(!_._2).foreach { case (dir, _) =>
      System.err.println(s"Failed to create directory '$dir'")
      System.exit(1)
    }
  }

  val title = (for {
    info <- Option(swagger.getInfo)
    title <- Option(info.getTitle)
  } yield title).getOrElse("No Info Title Provided")

  if (config.genServerCode) {
    val confDir = new File(config.outputDir, "conf")
    val appDir = new File(config.outputDir, "app")
    val controllersDir = new File(appDir, "controllers")
    val delegateTraitsDir = new File(controllersDir, "delegates")
    val modelsDir = new File(appDir, "models")

    val projectDir = new File(config.outputDir, "project")
    val bindersDir = new File(appDir, "binders")
    val publicDir = new File(config.outputDir, "public")

    createDirsWithCheck(confDir, controllersDir, delegateTraitsDir, modelsDir)
    if (config.stubFullPlayApp) {
      createDirsWithCheck(projectDir, bindersDir, publicDir)
    }

    writingToFile(new File(confDir, "routes")) {
      _.println(codegen.server.Routes.generate(swagger.routedControllers))
    }

    swagger.controllers.foreach { c =>
      writingToFile(new File(controllersDir, s"${c.name}.scala")) {
        _.println(codegen.server.Controller.generate(c))
      }
    }
    if (config.withHealthCheck) {
      writingToFile(new File(controllersDir, "Healthcheck.scala")) {
        _.println(codegen.server.HealthcheckController.generate())
      }
    }

    if (config.generateDelegates) {
      Delegates.extract(swagger.controllers).foreach { delegate =>
        writingToFile(new File(delegateTraitsDir, s"${delegate.className}.scala")) {
          _.println(codegen.server.ControllerDelegateTraits.generate(delegate))
        }
      }
    }

    // Generate warnings on cycles
    swagger.definitions(logCyclicDefinitionWarning)

    writingToFile(new File(modelsDir, "Models.scala")) {
      _.println(codegen.Models.generate("models", swagger.definitions()))
    }

    writingToFile(new File(modelsDir, "JsonOps.scala")) {
      _.println(codegen.JsonOps.generate("models", swagger.definitions()))
    }

    writingToFile(new File(modelsDir, "Enums.scala")) {
      // Disallowing unknown values in enum in server mode..
      _.println(codegen.Enums.generate("models", swagger.definitions(), false))
    }

    if (config.stubFullPlayApp) {
      writingToFile(new File(confDir, "application.conf")) {
        _.println(codegen.server.ApplicationConf.generate())
      }
      writingToFile(new File(confDir, "logback.xml")) {
        _.println(codegen.server.LogbackXml.generate())
      }
      writingToFile(new File(projectDir, "build.properties")) {
        _.println(codegen.server.BuildProperties.generate())
      }
      writingToFile(new File(projectDir, "plugins.sbt")) {
        _.println(codegen.server.ProjectPluginsSbt.generate())
      }
      writingToFile(new File(config.outputDir, "build.sbt")) {
        _.println(codegen.server.BuildSbt.generate(title))
      }
      writingToFile(new File(bindersDir, "package.scala")) {
        _.println(codegen.server.Binders.generate(swagger.containsCsvParams))
      }

      writingToFile(new File(publicDir, "swagger.json")) {
        val specSource = Source.fromFile(config.swaggerSpec).mkString
        val json = if (config.swaggerSpec.getName.endsWith(".json")) {
          specSource
        } else {
          yamlToJson(specSource)
        }
        _.println(json)
      }
    }

  } else {

    val clientPackageName = title.replace(".", "").replace(" ", "_").toLowerCase
    val clientDir = new File(new File(config.outputDir, "clients"), clientPackageName)

    if (clientDir.isDirectory && config.replace) {
      FileUtils.deleteDirectory(clientDir)
    }

    createDirsWithCheck(clientDir)

    val clientModelPackageFqn = s"clients.$clientPackageName"

    // Generate warnings on cycles.
    swagger.definitions(logCyclicDefinitionWarning)

    writingToFile(new File(clientDir, "Models.scala")) {
      _.println(codegen.Models.generate(clientModelPackageFqn, swagger.definitions()))
    }

    writingToFile(new File(clientDir, "JsonOps.scala")) {
      _.println(codegen.JsonOps.generate(clientModelPackageFqn, swagger.definitions()))
    }

    writingToFile(new File(clientDir, "Client.scala")) {
      _.println(codegen.Client.generate(clientModelPackageFqn, swagger.controllers(), config.generatePlay23Code))
    }

    writingToFile(new File(clientDir, "Enums.scala")) {
      // Allow unknown values in enum in client mode..
      _.println(codegen.Enums.generate(clientModelPackageFqn, swagger.definitions(), true))
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

  private def yamlToJson(yamlSource: String): String = {
    import scala.collection.JavaConverters._
    val yaml = (new Yaml).load(yamlSource)
    Json.pretty(yaml)
  }
}

