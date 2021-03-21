package dudka

import org.apache.commons.cli.GnuParser
import magnolia._
import org.apache.commons.cli.{Options => CliOptions}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}
import scala.annotation.{implicitNotFound, StaticAnnotation}
import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.util.Try

object cli {

  def apply[T: CliParser](args: Seq[String]): Either[String, T] =
    implicitly[CliParser[T]]
      .parse(args, None)
      .left
      .map(msg => s"Got the following errors while parsing [$args]: \n\t$msg")

  trait CliParser[I] {
    def parse(value: Seq[String], meta: Option[Meta]): Either[String, I]
    def canHasArgs: Boolean = true
  }

  final case class Meta(longName: String, description: String, default: Option[Any])

  class doc(doc: String) extends StaticAnnotation with Serializable {
    override def toString: String = doc
  }

  object name {

    class short(name: String) extends StaticAnnotation with Serializable {
      override def toString: String = name
    }

    class long(name: String) extends StaticAnnotation with Serializable {
      override def toString: String = name
    }
  }

  object CliParserDerivation {

    type Typeclass[I] = CliParser[I]
    private lazy val Parser = new GnuParser()

    implicit def gen[T]: CliParser[T] = macro Magnolia.gen[T]

    def combine[T](ctx: CaseClass[Typeclass, T]): CliParser[T] = (args, _) => {
      val (options, metadata) = ctx.parameters.foldLeft((new CliOptions, Map.empty[String, Meta])) {
        case ((cliOptions, metadata), param) =>
          val shortName   = getAnnotation[name.short](param.annotations).getOrElse(param.label)
          val longName    = getAnnotation[name.long](param.annotations).getOrElse(param.label)
          val description = getAnnotation[doc](param.annotations).getOrElse("")
          (
            cliOptions.addOption(shortName, longName, param.typeclass.canHasArgs, description),
            metadata + (shortName -> Meta(longName, description, param.default))
          )
      }

      Try(Parser.parse(options, args.toArray)).fold(
        t => Left(t.getMessage),
        commandLine =>
          ctx.constructEither { param =>
            val cliOptName = getAnnotation[name.short](param.annotations).getOrElse(param.label)
            param.typeclass.parse(
              if (commandLine.hasOption(cliOptName)) Seq(commandLine.getOptionValue(cliOptName))
              else Seq.empty,
              metadata.get(cliOptName)
            )
          }.left.map(_.mkString("\n\t"))
      )
    }

    private def getAnnotation[T <: StaticAnnotation: ClassTag](annotations: Seq[Any]): Option[String] =
      annotations.collectFirst { case a: T => a.toString }

    def dispatch[T: Dispatchable](sealedTrait: SealedTrait[Typeclass, T]): CliParser[T] = ???

    implicit val _string: CliParser[String] = right[String]((s, _) => Right(s))

    implicit val _int: CliParser[Int] =
      right[Int](
        (s, m) =>
          Try(s.toInt).toEither.left.map(t => s"Cannot parse cli for ${m.longName}[${m.description}]: $t")
      )

    implicit val _long: CliParser[Long] =
      right[Long](
        (s, m) =>
          Try(s.toLong).toEither.left.map(t => s"Cannot parse cli for ${m.longName}[${m.description}]: $t")
      )

    implicit val _double: CliParser[Double] =
      right[Double](
        (s, m) =>
          Try(s.toDouble).toEither.left.map(t => s"Cannot parse cli for ${m.longName}[${m.description}]: $t")
      )

    implicit val _bool: CliParser[Boolean] = new CliParser[Boolean] {
      override def parse(value: Seq[String], meta: Option[Meta]): Either[String, Boolean] =
        Right(value.nonEmpty)
      override def canHasArgs: Boolean = false
    }

    implicit val _localDate: CliParser[LocalDate] =
      right[LocalDate](
        (s, m) =>
          Try(LocalDate.parse(s, DateTimeFormatter.ISO_LOCAL_DATE)).toEither.left
            .map(t => s"Cannot parse cli for ${m.longName}[${m.description}]: $t")
      )

    implicit val _localDateTime: CliParser[LocalDateTime] =
      right[LocalDateTime](
        (s, m) =>
          Try(LocalDateTime.parse(s, DateTimeFormatter.ISO_LOCAL_DATE_TIME)).toEither.left
            .map(t => s"Cannot parse cli for ${m.longName}[${m.description}]: $t")
      )

    implicit def _option[A: CliParser]: CliParser[Option[A]] = { (value, meta) =>
      if (value.isEmpty) Right(None)
      else implicitly[CliParser[A]].parse(value, meta).right.map(Option(_))
    }

    private def right[I](f: (String, Meta) => Either[String, I]): CliParser[I] = { (value, meta) =>
      for {
        m       <- meta.toRight("Error while constructing Metadata")
        default = m.default.map(_.asInstanceOf[I])
        v <- value.headOption
              .map(
                f(_, m).left.flatMap(e => default.toRight(e)) // todo: remove default ???
              )
              .getOrElse(default.toRight(s"No cli param for ${m.longName}[${m.description}]"))
      } yield v
    }

    @implicitNotFound("Cannot derive CliParser for sealed trait")
    private sealed trait Dispatchable[T]
  }
}
