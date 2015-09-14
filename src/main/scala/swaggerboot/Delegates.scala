package swaggerboot

object Delegates {
  def extract(controllers: Seq[Controller]): Seq[ControllerDelegate] = {
    val delegateMethods = for {
      controller <- controllers
      method <- controller.methods
    } yield {
      method.delegate.getOrElse(getDefault(controller, method))
    }

    delegateMethods.groupBy(m => (m.packageName, m.className)).toSeq.map {
      case ((packageName, className), methods) =>
        ControllerDelegate(packageName, className, methods)
    }
  }

  def getDefault(controller: Controller, method: Method): MethodDelegate = {
    require(method.delegate.isEmpty)
    MethodDelegate("delegates", controller.name, method.name, method)
  }
}
