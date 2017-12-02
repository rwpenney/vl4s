/*
 *  Simple demonstration of VL4S (Vega-Lite for Scala) library
 *  RW Penney, November 2017
 */

package uk.rwpenney.vl4s.demo


import org.json4s._
import org.json4s.native.JsonMethods.{ pretty, render }
import uk.rwpenney.vl4s._
import uk.rwpenney.vl4s.ExportImplicits._
import uk.rwpenney.vl4s.ShortcutImplicits._


object Demo {
  def main(args: Array[String]) {
    Seq(simple(), simple2()).map { spec =>
      val json = spec.toJValue

      println(pretty(render(json)))
      println("\n")
    }

    webpage()
  }

  def simple(): TopLevelSpec =
    SimpleSpec() .
      background("GhostWhite") .
      data(InlineData() .
        values(Seq(
          Map("x_column" -> 2, "y_column" -> 1.4, "other" -> -31),
          Map("x_column" -> 5, "y_column" -> 3.2, "other" -> -3),
          Map("x_column" -> 7, "y_column" -> 1.9, "other" -> -5.2) ))) .
      mark(Mark.line) .
      encoding(EncodingWithFacet() .
        x(PositionFieldDef() .
          field("x_column") .
          axis(Axis() .
            title("x-axis"))) .
        y(PositionFieldDef() .
          field("y_column") .
          axis(Axis() .
            title("y-axis")))
      )

  def simple2(): TopLevelSpec =
    SimpleSpec() .
      data("file:apt-package-sizes.tsv") .
      selection(
        Map("chosen" -> SingleSelection() .
                          encodings(Seq(SingleDefChannel.color)) .
                          vtype(SingleSelection_type.single))) .
      mark(Mark.bar) .
      encoding(EncodingWithFacet() .
        >> { enc => enc .
          x(enc.XYaxisDef .
            axis(Axis() .
              title("log(size)")) .
            field("logSize") .
            vtype(Type.quantitative) .
            bin(BinParams() .
              maxbins(20))) .
          y(enc.XYaxisDef .
            axis(Axis() .
              title("count")) .
            field("*") .
            vtype(Type.quantitative) .
            aggregate(AggregateOp.count)) .
          color(MarkPropValueDefWithCondition() .
            condition(Conditional_MarkPropFieldDef_() .
              field("simpleSection") .
              selection("chosen") .
              vtype(Type.nominal)) .
            value("grey"))
        }
      )

  def webpage() {
    val spec = simple2()

    println(s"""|<html>
                |<head><title>Simple VL4S demo</title>
                |  <style type="text/css">
                |    body { background-color: white; }
                |    div.vl4s_embed { height: 500px; width: 700px; }
                |  </style>
                |${spec.jsImports(indent="  ")}
                |</head>
                |<body>
                |<h1>A trivial VL4S demonstration</h1>
                |${spec.htmlDiv(ident="vl4s_embed")}
                |</body>
                |</html>""" . stripMargin)
  }
}
