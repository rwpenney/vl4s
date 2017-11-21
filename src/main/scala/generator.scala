/*
 *  Application entry-point for VL4S
 *  RW Penney, November 2017
 */

//  Copyright (C) 2017, RW Penney
//  This file is part of VL4S.
//
//  VL4S is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  Foobar is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with VL4S.  If not, see <http://www.gnu.org/licenses/>.

package uk.rwpenney.vl4s.gen


object Generator {
  case class Config(
    schemaUrl: String = "https://raw.githubusercontent.com/vega/schema/" +
                        "master/vega-lite/v2.0.0.json",
    srcOutput: String = "vl4s/src/main/scala/auto-vega.scala"
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
        val src = scala.io.Source.fromURL(config.schemaUrl)
        val schema = SchemaParser(src)
        val codegen = new CodeGen(new java.io.FileOutputStream(config.srcOutput))
        codegen(schema)
      }
      case None => System.exit(1)
    }
  }
}
