scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "io.swagger" % "swagger-parser" % "1.0.10",
  "org.scalaz" %% "scalaz-core" % "7.1.3",
  "commons-io" % "commons-io" % "2.4",
  "com.github.scopt" %% "scopt" % "3.3.0",
  "com.typesafe.play" %% "play-json" % "2.4.3",
  "org.yaml" % "snakeyaml" % "1.16"
)

mainClass in assembly := Some("swaggerboot.SwaggerCodeGenerator")
