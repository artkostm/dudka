# dudka

 An example wrapper for Apache Common CLI library to construct case classes out of command line arguments.

#### How to use it:

 1. Create a case class describing your cli params:
 
 ```scala
 import package com.petrofac.data.common.cli._
 case class TestArgs(
                       @doc("the very first cli param") first: String,
                       @name.long("seconnnd") second: String,
                       @name.short("t") third: String,
                       test: Boolean,
                       i: Int,
                       ld: LocalDate,
                       ldt: LocalDateTime,
                       lng: Long,
                       storageAccount: Secret,
                       dbl: Double,
                       opt: Option[String],
                       @separator(",") lst: Seq[Int], // separator should be specified
                       dflt: Int = 45
                     )
   ```
   
  2. In your main method, write the code below to convert cli arguments to an instance of case class from #1

```scala
  import com.petrofac.data.common.cli._
  import CliParserDerivation._
  val instanceOfCaseClass = parseCli[TestArgs](args).fold(e => throw new RuntimeException(e), identity)
  ...
  ```
