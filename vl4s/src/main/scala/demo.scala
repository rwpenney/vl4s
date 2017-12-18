/*
 *  Simple demonstration of VL4S (Vega-Lite for Scala) library
 *  RW Penney, November 2017
 */

//  Copyright (C) 2017, RW Penney
//  This file is part of VL4S.

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package uk.rwpenney.vl4s.demo


import org.json4s._
import org.json4s.native.JsonMethods.{ pretty, render }
import scala.annotation.tailrec
import uk.rwpenney.vl4s._
import uk.rwpenney.vl4s.ExportImplicits._
import uk.rwpenney.vl4s.ShortcutImplicits._


/** Dataset generator using a simple Gaussian mixture model */
object GaussMix {
  val randgen = new scala.util.Random

  case class Gaussian(label: String, mean: Double, stddev: Double)
  val clusters = Map(1.0 -> Gaussian("cat-A", 1.0, 0.2),
                     0.2 -> Gaussian("cat-B", 2.0, 0.3),
                     0.1 -> Gaussian("cat-C", 0.0, 0.1))

  def apply(npoints: Integer = 100): Seq[Map[String, Any]] = {
    val clusterSeq = clusters.scanLeft((0.0, Gaussian("", 0, 0))) {
      case ((tot, _), (wght, g)) => (tot + wght, g) } . drop(1)
    val totalWeight = clusters.keys.sum

    (0 until npoints).map { _ =>
      val wCluster = totalWeight * randgen.nextDouble

      val clust = clusterSeq.dropWhile {
        case (w, g) => (w < wCluster) } . head._2

      Map("label" ->  clust.label,
          "x" ->      (clust.mean + clust.stddev * randgen.nextGaussian))
    }
  }
}


trait SpecGenerator {
  def makeSpec: TopLevelSpec
}


object TrivialDemo extends SpecGenerator {
  def makeSpec: TopLevelSpec =
    SimpleSpec() .
      background("GhostWhite") .
      //data("myDataFile.csv") .
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
}


object WaveDemo extends SpecGenerator {
  def makeSpec: TopLevelSpec = {
    val xvals = (0.0 to 5.0 by 0.2).toSeq
    val curves = Map(
      "cosine" -> xvals.map { math.cos(_) },
      "sine" -> xvals.map { math.sin(_) },
      "J0" -> xvals.map { bessel0(_) } )
    val curveData = curves.map { case (id, vals) =>
      xvals.zip(vals).map { case (x, y) =>
        Map( "func" -> id, "x" -> x, "y" -> y) } } . flatten . toSeq

    SimpleSpec() .
      data(InlineData() .
        values(curveData)) .
      mark(MarkDef() .
        interpolate(Interpolate.basis)
        vtype(Mark.line)) .
      encoding(EncodingWithFacet() .
        x(PositionFieldDef() .
          field("x")) .
        y(PositionFieldDef() .
          field("y")) .
        color(MarkPropFieldDefWithCondition() .
          field("func") .
          vtype(Type.nominal)))
  }

  def bessel0(x: Double) = {
    @tailrec
    def addTerms(accum: Double, xpow: Double, n: Integer, factorial: Long): Double = {
      val term = xpow  / (factorial * factorial)
      if (term < 1e-16) {
        accum
      } else {
        val sgn = if ((n % 2) == 1) -1 else +1
        addTerms(accum + sgn * term,
                 0.25 * xpow * x * x, n + 1, (n + 1) * factorial)
      }
    }
    addTerms(0.0, 1.0, 0, 1)
  }
}


object GaussMixDemo extends SpecGenerator {
  def makeSpec: TopLevelSpec = {
    val inlineData = GaussMix(1000)

    SimpleSpec() .
      data(InlineData() .
        values(inlineData)) .
      selection(
        Map("chosen" -> SingleSelection() .
                          encodings(Seq(SingleDefChannel.color)) .
                          vtype(SingleSelection_type.single))) .
      mark(Mark.bar) .
      encoding(EncodingWithFacet() .
        >> { enc => enc .
          x(enc.XYaxisDef .
            axis(Axis() .
              title("location")) .
            field("x") .
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
              field("label") .
              selection("chosen") .
              vtype(Type.nominal)) .
            value("grey"))
        }
      )
  }
}


object Demo {
  object Mode extends Enumeration {
    type Mode = Value
    val Trivial, Waves, GaussMix = Value
  }
  implicit val modeRead: scopt.Read[Mode.Value] =
    scopt.Read.reads(Mode withName _)
  val generators: Map[Mode.Value, SpecGenerator] = Map(
    Mode.Trivial →  TrivialDemo,
    Mode.Waves →    WaveDemo,
    Mode.GaussMix → GaussMixDemo
  )

  object Format extends Enumeration {
    type Format = Value
    val Json, Webpage = Value
  }
  implicit val formatRead: scopt.Read[Format.Value] =
    scopt.Read.reads(Format withName _)

  case class Config(
    mode: Mode.Value = Mode.Trivial,
    outputFile: String = "",
    outputFormat: Format.Value = Format.Json
  )
  val defaultConfig = Config()

  def main(args: Array[String]) {
    val optparse = new scopt.OptionParser[Config]("vl4s demonstrations") {
      opt[Mode.Value]('m', "mode") .
        action( (x, c) => c.copy(mode = x) ).
        text(s"Choice of demonstration (${Mode.values.mkString("/")}," +
             s" default=${defaultConfig.mode})")
      opt[Format.Value]('f', "output-format") .
        action( (x, c) => c.copy(outputFormat = x) ).
        text(s"Output format (${Format.values.mkString("/")}," +
             s" default=${defaultConfig.outputFormat})")
      opt[String]('o', "output-file") .
        action( (x, c) => c.copy(outputFile = x) ) .
        text("Output file to generate (blank implies stdout," +
              s" default=${defaultConfig.outputFile})")

      help("help").text("Print usage information")
      override def showUsageOnError =true
    }

    optparse.parse(args, Config()) match {
      case Some(config) => {
        val spec = generators(config.mode).makeSpec
        val json = spec.toJValue

        val doc = config.outputFormat match {
          case Format.Json => pretty(render(json))
          case Format.Webpage => makeWebpage(spec)
        }

        config.outputFile match {
          case "" => println(doc)
          case filename => {
            val pw = new java.io.PrintWriter(filename)
            pw.println(doc)
            pw.close
          }
        }
      }

      case None => System.exit(1)
    }
  }

  def makeWebpage(spec: TopLevelSpec): String =
    s"""${spec.htmlPage(headerPrefix="<title>Simple VL4S demo</title>",
                        bodyPrefix="<h1>A trivial VL4S demonstration</h1>")}"""
}
