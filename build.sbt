scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "io.swagger" % "swagger-parser" % "1.0.10",
  "org.scalaz" %% "scalaz-core" % "7.1.3"
)

mainClass in assembly := Some("swaggerboot.PlaySkeletonBootstrap")
