package swaggerboot.codegen.server

object LogbackXml {
  def generate(): String = {
    s"""
      |<configuration>
      |
      |  <conversionRule conversionWord="coloredLevel" converterClass="play.api.Logger$$ColoredLevel" />
      |
      |  <appender name="STDOUT" class="ch.qos.logback.core.ConsoleAppender">
      |    <encoder>
      |      <pattern>%coloredLevel - %logger - %message%n%xException</pattern>
      |    </encoder>
      |  </appender>
      |
      |  <!--
      |    The logger name is typically the Java/Scala package name.
      |    This configures the log level to log at for a package and its children packages.
      |  -->
      |  <logger name="play" level="INFO" />
      |  <logger name="application" level="DEBUG" />
      |
      |  <root level="ERROR">
      |    <appender-ref ref="STDOUT" />
      |  </root>
      |
      |</configuration>
      """.stripMargin
  }
}
