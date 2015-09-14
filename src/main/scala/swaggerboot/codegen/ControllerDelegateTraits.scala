package swaggerboot.codegen

import swaggerboot.{MethodDelegate, ControllerDelegate}

object ControllerDelegateTraits {
  val PackageName = "controllers.delegates"

  def generate(delegate: ControllerDelegate): String = {
    s"""package $PackageName
       |
       |import com.google.inject.ImplementedBy
       |
       |@ImplementedBy(classOf[_root_.${delegate.packageName}.${delegate.className}])
       |trait ${delegate.className} {
       |  ${
            delegate.methods.map { method =>
              s"def ${toScalaName(method.functionName)}(${paramList(method)})(implicit request: play.api.mvc.RequestHeader): scala.concurrent.Future[play.api.mvc.Result]"
            }.mkString("\n  ")
          }
       |}
     """.stripMargin
  }

  private def paramList(delegate: MethodDelegate): String = Controller.paramSigs(delegate.method.params)
}
