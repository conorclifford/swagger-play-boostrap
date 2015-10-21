package swaggerboot.codegen.server

object ApplicationConf {
  def generate(): String = {
    s"""
       |# This is the main configuration file for the application.
       |# ~~~~~
       |
       |# Secret key
       |# ~~~~~
       |# The secret key is used to secure cryptographics functions.
       |#
       |# This must be changed for production, but we recommend not changing it in this file.
       |#
       |# See http://www.playframework.com/documentation/latest/ApplicationSecret for more details.
       |play.crypto.secret = "changeme"
       |
       |# The application languages
       |# ~~~~~
       |play.i18n.langs = [ "en" ]
     """.stripMargin
  }
}