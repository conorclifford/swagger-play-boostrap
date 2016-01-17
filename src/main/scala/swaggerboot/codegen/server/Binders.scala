package swaggerboot.codegen.server

import swaggerboot.{Controller, ModelDefinition}
import swaggerboot.codegen.{lowerFirst, Enums}

object Binders {
  def generate(definitions: Seq[ModelDefinition], controllers: Seq[Controller], includeStringCsv: Boolean) = {
    val stringCsvPathBinding = if (includeStringCsv) {
      s"""
         |implicit def stringSeqPathBindable(implicit stringBinder: PathBindable[String]) = new PathBindable[Seq[String]] {
         |    // FIXME use a better CSV parser perhaps?
         |    override def bind(key: String, value: String): Either[String, Seq[String]] = stringBinder.bind(key, value).right.map(_.split(","))
         |    override def unbind(key: String, value: Seq[String]): String = stringBinder.unbind(key, value.mkString(","))
         |  }
         |""".stripMargin
    } else {
      ""
    }

    val stringCsvQueryBinding = if (includeStringCsv) {
      s"""implicit def stringSeqQueryStringBindable(implicit sb: QueryStringBindable[String]) = new QueryStringBindable[Seq[String]] {
         |    // FIXME use a better CSV parser perhaps?
         |    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, Seq[String]]] = sb.bind(key, params).map(_.right.map(_.split(",")))
         |    override def unbind(key: String, value: Seq[String]): String = sb.unbind(key, value.mkString(","))
         |  }
         |""".stripMargin
    } else {
      ""
    }

    val enumQueryStringBinderCreator =
      s"""private def enumQueryStringBinder[E <: NamedEnum](fn: String => EnumError \\/ E)(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[E] = new QueryStringBindable[E] {
         |    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, E]] = {
         |      stringBinder.bind(key, params).map {
         |        _.disjunction.flatMap(fn(_).leftMap(_.message)).toEither
         |      }
         |    }
         |
         |    override def unbind(key: String, value: E): String = {
         |      stringBinder.unbind(key, value.name)
         |    }
         |  }
         |""".stripMargin

    val definitionEnumQueryStringBindables = Enums.enumObjectNames(definitions, controllers).map { case wrappingName =>
      s"implicit def ${lowerFirst(wrappingName)}QueryStringBindable(implicit sb: QueryStringBindable[String]) = enumQueryStringBinder($wrappingName.apply)"
    }

    s"""//
       |// This is a generated file.
       |//
       |
       |import scala.util.control.Exception.nonFatalCatch
       |import play.api.mvc.{QueryStringBindable, PathBindable}
       |
       |import api.models._
       |import api.ids
       |
       |import scalaz._
       |import Scalaz._
       |
       |package object binders {
       |
       |  implicit def idPathBindable[V, T](implicit binder: PathBindable[V]) = new PathBindable[ids.Id[V, T]] {
       |    override def bind(key: String, value: String) = binder.bind(key, value).right.map(ids.Id[V, T](_))
       |    override def unbind(key: String, value: ids.Id[V, T]) = binder.unbind(key, value.value)
       |  }
       |
       |  implicit def idQueryStringBindable[V, T](implicit binder: QueryStringBindable[V]) = new QueryStringBindable[ids.Id[V, T]] {
       |    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, ids.Id[V, T]]] = {
       |      binder.bind(key, params).map(_.right.map(ids.Id[V, T]))
       |    }
       |    override def unbind(key: String, value: ids.Id[V, T]): String = binder.unbind(key, value.value)
       |  }
       |
       |  $stringCsvPathBinding
       |  $stringCsvQueryBinding
       |
       |  ${definitionEnumQueryStringBindables.mkString("\n  ")}
       |
       |  $enumQueryStringBinderCreator
       |}
     """.stripMargin
  }
}
