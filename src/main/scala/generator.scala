/*
 *  Application entry-point for VL4S
 *  RW Penney, November 2017
 */

package uk.rwpenney.vl4s.gen


object Generator {
  case class Config(
    schemaUrl: String = "v2.0.0.json",
    srcOutput: String = "vl4s/src/main/scala/experiment.scala"
  )
  val defaultConfig = Config()

  def main(args: Array[String]) {
    val optparse = new scopt.OptionParser[Config]("vl4s-generator") {
      opt[String]('s', "schema-url") .
        action( (x, c) => c.copy(schemaUrl = x) ) .
        text(s"URL of Vega-Lite schema (default=${defaultConfig.schemaUrl})")

      help("help").text("Print usage information")
      override def showUsageOnError = true
    }

    optparse.parse(args, Config()) match {
      case Some(config) => {
        println(config)
        val schema = SchemaParser(config.schemaUrl)
        val codegen = new CodeGen(new java.io.FileOutputStream(config.srcOutput))
        codegen(CodeGen.toCodeable(schema))
      }
      case None => System.exit(1)
    }
  }
}
