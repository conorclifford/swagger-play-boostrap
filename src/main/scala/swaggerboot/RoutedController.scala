package swaggerboot


case class RoutedController(path: String, name: String, methods: Seq[Method]) {
  lazy val routesFileEntries: Seq[String] = {
    methods.map { method => method.routeFileEntry(path, name) }
  }
}
