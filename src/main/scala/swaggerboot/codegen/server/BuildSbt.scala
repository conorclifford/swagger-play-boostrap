package swaggerboot.codegen.server

object BuildSbt {
  def generate(name: String): String = {
    s"""
       |name := "$name"
       |
       |version := "1.0-SNAPSHOT"
       |
       |lazy val root = (project in file(".")).enablePlugins(PlayScala)
       |
       |scalaVersion := "2.11.7"
       |
       |scalacOptions ++= Seq(
       |  "-deprecation",     // Emit warning and location for usages of deprecated APIs.
       |  "-feature",         // Emit warning and location for usages of features that should be imported explicitly.
       |  "-unchecked",       // Enable additional warnings where generated code depends on assumptions.
       |  "-Xfatal-warnings", // Fail the compilation if there are any warnings.
       |  "-Xlint",           // Enable recommended additional warnings.
       |  "-Xcheckinit",
       |  "-Ywarn-dead-code"  // Warn when dead code is identified.
       |)
       |
       |libraryDependencies ++= Seq(
       |  jdbc,
       |  cache,
       |  ws,
       |  "org.scalaz" %% "scalaz-core" % "7.1.3",
       |  "org.webjars" % "swagger-ui" % "2.1.8-M1",
       |  "org.scalatest" %% "scalatest" % "2.2.4" % Test,
       |  "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % Test,
       |  "org.scalatestplus" %% "play" % "1.4.0-M3" % Test
       |)
       |
       |resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
       |
       |// Play provides two styles of routers, one expects its actions to be injected, the
       |// other, legacy style, accesses its actions statically.
       |routesGenerator := InjectedRoutesGenerator
       |
       |routesImport := Seq(
       |  "binders._",
       |  "api.models._",
       |  "api.ids"
       |)
     """.stripMargin
  }
}
