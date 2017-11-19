# VL4S (Vega-Lite for Scala)

vl4s is a prototype tool for automatically generating
a [Scala](https://www.scala-lang.org) library
via which interactive plots can be generated conforming
to the [Vega-Lite](https://vega.github.io/vega-lite) specification.

vl4s is intended to allow the
JSON [schema](https://github.com/vega/schema/tree/master/vega-lite)
provided by Vega-Lite to be converted into a Scala API
which provides natural, and type-safe,
ways of describing plots in Scala and emitting Vega-Lite JSON documents
that can be embedded in web-pages.
This is an alternative approach to that of [Vegas](https://www.vegas-viz.org/),
and [Argus](https://github.com/aishfenton/Argus).

The build-system is managed by [SBT](http://www.scala-sbt.org/),
and consists of two projects:
* the "root" project, which must be compiled and run to generate
  the file "vl4s/src/main/scala/auto-vega.scala"
  from a local copy of the VegaLite schema (e.g. "v2.0.0.json")
* the "vl4s" project, which will build the VegaLite API itself,
  and eventually allow creation of VegaLite JSON documents.

(The tool is currently at a very early stage of development.)
