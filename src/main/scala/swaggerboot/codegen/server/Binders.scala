package swaggerboot.codegen.server

object Binders {
  def generate(includeStringCsv: Boolean) = {
    val stringCsvPathBinding = if (includeStringCsv) {
      s"""
         |implicit def stringSeqPathBindable(implicit stringBinder: PathBindable[String]) = new PathBindable[Seq[String]] {
         |    // FIXME use a better CSV parser perhaps?
         |    override def bind(key: String, value: String): Either[String, Seq[String]] = stringBinder.bind(key, value).right.map(_.split(","))
         |    override def unbind(key: String, value: Seq[String]): String = stringBinder.unbind(key, value.mkString(","))
         |  }
       """.stripMargin
    } else {
      ""
    }

    val stringCsvQueryBinding = if (includeStringCsv) {
      s"""
         |implicit def stringSeqQueryStringBindable(implicit sb: QueryStringBindable[String]) = new QueryStringBindable[Seq[String]] {
         |    // FIXME use a better CSV parser perhaps?
         |    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, Seq[String]]] = sb.bind(key, params).map(_.right.map(_.split(",")))
         |    override def unbind(key: String, value: Seq[String]): String = sb.unbind(key, value.mkString(","))
         |  }
       """.stripMargin
    } else {
      ""
    }

    s"""//
       |// This is a generated file.
       |//
       |
       |import scala.util.control.Exception.nonFatalCatch
       |import play.api.mvc.{QueryStringBindable, PathBindable}
       |
       |package object binders {
       |  $stringCsvPathBinding
       |
       |  $stringCsvQueryBinding
       |}
     """.stripMargin
  }
}
