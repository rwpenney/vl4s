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
      println(s"Spec: ${spec} =>\n")

      val json = spec.toJValue

      println(pretty(render(json)))
      println("\n")
    }

    webpage()
  }

  def simple(): TopLevelSpec =
    SimpleSpec() .
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

  def simple2(): TopLevelSpec =
    SimpleSpec() .
      data("file:apt-package-sizes.tsv") .
      //selection(SelectionDef()) .   // FIXME - added selection definition
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
                |    div.vl4s_embed { height: 400px; width: 600px; }
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
