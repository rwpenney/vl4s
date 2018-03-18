/*
 *  Application entry-point for VL4S
 *  RW Penney, November 2017
 */

//  Copyright (C) 2017-2018, RW Penney
//  This file is part of VL4S.

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package uk.rwpenney.vl4s.gen


/** Handle for reading known version of VegaLite schema, e.g. from URL */
class VersionedSource(val src: scala.io.BufferedSource,
                      val version: String)

object VersionedSource {
  val upstreamBaseURL = "https://raw.githubusercontent.com/vega/" +
                          "schema/master/vega-lite"

  def fromURL(url: String) = {
    url match {
      case schemaRegexp(version) =>
        new VersionedSource(scala.io.Source.fromURL(url), version)
      case _ => throw new IllegalArgumentException(
                      s"""Cannot extract schema version from URL "${url}"""")
    }
  }

  def fromLocal(rootdir: String, version: String) = {
    val path = java.nio.file.Paths.get(rootdir).resolve(schemaFile(version))
    new VersionedSource(scala.io.Source.fromFile(path.toFile), version)
  }

  def fromUpstream(version: String) = {
    val url = s"${upstreamBaseURL}/${schemaFile(version)}"
    new VersionedSource(scala.io.Source.fromURL(url), version)
  }

  def schemaFile(version: String) = s"v${version}.json"
  val schemaRegexp = raw".*v([0-9][.0-9]*[0-9])\.json$$".r
}


object Generator {
  import uk.rwpenney.vl4s.build.VegaConfig

  case class Config(
    schemaUrl: String = "",
    schemaVersion: String = VegaConfig.defaultSchemaVersion,
    srcOutput: String = "vl4s/src/main/scala/" + VegaConfig.defaultAutogenSource
  )
  val defaultConfig = Config()

  def main(args: Array[String]) {
    val optparse = new scopt.OptionParser[Config]("vl4s-generator") {
      opt[String]('o', "output-src") .
        action( (x, c) => c.copy(srcOutput = x) ).
        text("Filename of output scala source-code" +
              s" (default=${defaultConfig.srcOutput})")
      opt[String]('s', "schema-url") .
        action( (x, c) => c.copy(schemaUrl = x) ) .
        text(s"URL of Vega-Lite schema (default=${defaultConfig.schemaUrl})")
      opt[String]('V', "schema-version") .
        action( (x, c) => c.copy(schemaVersion = x) ) .
        text("Release version of Vega-Lite schema to read from github.com" +
              s" (default=${defaultConfig.schemaVersion})")

      help("help").text("Print usage information")
      override def showUsageOnError = true
    }

    optparse.parse(args, Config()) match {
      case Some(config) => {
        val src = if (config.schemaUrl.nonEmpty) {
                    VersionedSource.fromURL(config.schemaUrl)
                  } else {
                    VersionedSource.fromUpstream(config.schemaVersion)
                  }
        val schema = SchemaParser(src)
        val codegen = new CodeGen(new java.io.FileOutputStream(config.srcOutput))
        codegen(schema)
      }
      case None => System.exit(1)
    }
  }
}
