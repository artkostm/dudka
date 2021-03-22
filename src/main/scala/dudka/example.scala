package dudka

import java.time.{LocalDate, LocalDateTime}


object Example {


  import com.petrofac.data.landing.rest.protocol.codec.cli._
  import CliParserDerivation._

  case class TestArgs(
                       @doc("the very first cli param") first: String,
                       @name.long("seconnnd") second: String,
                       @name.short("t") third: String,
                       test: Boolean,
                       i: Int,
                       ld: LocalDate,
                       ldt: LocalDateTime,
                       lng: Long,
                       dbl: Double,
                       opt: Option[String],
                       @separator(",") lst: Seq[Int], // separator should be specified
                       dflt: Int = 45
                     )

  def main(args: Array[String]): Unit = {

    cli[TestArgs](args).fold(e => new RuntimeException(e), cliArgs => {
      println(cliArgs)
      // any code here
    })
  }
}
