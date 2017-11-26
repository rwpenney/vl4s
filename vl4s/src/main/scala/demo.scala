/*
 *  Simple demonstration of VL4S (Vega-Lite for Scala) library
 *  RW Penney, November 2017
 */

package uk.rwpenney.vl4s.demo


import org.json4s._
import org.json4s.native.JsonMethods.{ pretty, render }
import uk.rwpenney.vl4s._
import uk.rwpenney.vl4s.ShortcutImplicits._


object Demo {
  def main(args: Array[String]) {
    Seq(simple(), simple2()).map { spec =>
      println(s"Spec: ${spec} =>\n")

      val json = spec.toJValue

      println(pretty(render(json)))
      println("\n")
    }
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
      encoding(EncodingWithFacet() .
        x(PositionFieldDef() .
          field("logSize") .
          axis(Axis())) .
        y(PositionFieldDef() .
          field("*") .
          axis(Axis())) .
        color(MarkPropFieldDefWithCondition() .
          field("simpleSection") .
          vtype(Type.nominal)))
}
