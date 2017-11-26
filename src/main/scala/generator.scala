/*
 *  Application entry-point for VL4S
 *  RW Penney, November 2017
 */

//  Copyright (C) 2017, RW Penney
//  This file is part of VL4S.

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

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
