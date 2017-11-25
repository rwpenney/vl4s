/*
 *  Simple demonstration of VL4S (Vega-Lite for Scala) library
 *  RW Penney, November 2017
 */

package uk.rwpenney.vl4s.demo

import uk.rwpenney.vl4s._
import uk.rwpenney.vl4s.ShortcutImplicits._


object Demo {
  def main(args: Array[String]) {
    val e0 = AggregateOp.average
    val e1 = ScaleType.bin_linear

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

    println(s"${plot}")
  }
}
