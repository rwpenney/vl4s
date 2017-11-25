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
and [Argus](https://github.com/aishfenton/Argus), aiming to create
a DSL that is closer to the structure of the underlying JSON schema
(hence, hopefully, easier to document) and allowing export
of a [json4s](http://json4s.org) datastructure that could
be customized by the user.

The build-system is managed by [SBT](http://www.scala-sbt.org/),
and consists of two projects:
* the "root" project, which must be compiled and run to generate
  the file "vl4s/src/main/scala/auto-vega.scala"
  from a local copy of the VegaLite schema (e.g. "v2.0.0.json")
* the "vl4s" project, which will build the VegaLite API itself,
  and eventually allow creation of VegaLite JSON documents.

(The tool is currently at a very early stage of development.)


## Example

```scala
import org.json4s._
import org.json4s.native.JsonMethods.{ pretty, render }
import uk.rwpenney.vl4s._
import uk.rwpenney.vl4s.ShortcutImplicits._

val plot = SimpleSpec() .
  background("green") .
  data("file:/somewhere/interesting.csv") .
  encoding(EncodingWithFacet() .
    x(PositionFieldDef() .
      field("x_column") .
      axis(Axis() .
        title("Some title"))) .
    y(PositionFieldDef() .
      field("y_column")
      bin(BinParams() .
        maxbins(50) .
        nice(true))))

val json = plot.toJValue
println(pretty(render(json)))
```
will create:
```json
{
  "background":"green",
  "data":{
    "url":"file:/somewhere/interesting.csv"
  },
  "encoding":{
    "x":{
      "field":"x_column",
      "axis":{
        "title":"Some title"
      }
    },
    "y":{
      "field":"y_column",
      "bin":{
        "maxbins":50.0,
        "nice":true
      }
    }
  }
}

```
