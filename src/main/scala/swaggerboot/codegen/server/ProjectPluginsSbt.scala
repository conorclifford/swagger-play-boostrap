package swaggerboot.codegen.server

object ProjectPluginsSbt {
  def generate(): String = {
    s"""
       |// The Play plugin
       |addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.3")
     """.stripMargin
  }
}
