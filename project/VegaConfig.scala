//  Build-time settings for VL4S
//  This will be copied via build.sbt into */src_managed/main/

package uk.rwpenney.vl4s.build

object VegaConfig {
  val defaultSchemaVersion =      "2.5.0"
  val defaultAutogenSource =      "auto-vega.scala"
  val vegaJSlibraryVersion =      "3.3.1"
  val vegaEmbedJSlibraryVersion = "3.14.0"
}
