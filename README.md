# VL4S (Vega-Lite for Scala)

vl4s is a tool for automatically generating
a [Scala](https://www.scala-lang.org) library
via which interactive plots can be generated conforming
to the [Vega-Lite](https://vega.github.io/vega-lite) specification.

vl4s is intended to allow the
JSON [schema](https://github.com/vega/schema/tree/master/vega-lite)
provided by Vega-Lite to be converted into a Scala API
which provides natural, and type-safe,
ways of describing plots in Scala and emitting Vega-Lite JSON documents
that can be embedded in web-pages.
This is an alternative approach to that of [Vegas](https://www.vegas-viz.org/).

(The tool is currently at a very early stage of development.)
