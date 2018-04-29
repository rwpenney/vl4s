/*
 *  Simple demonstration of VL4S (Vega-Lite for Scala) library
 *  RW Penney, November 2017
 */

//  Copyright (C) 2017-2018, RW Penney
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


trait SpecGenerator {
  def title: String
  def makeSpec: Vl4sTopLevelSpec
}


object TrivialDemo extends SpecGenerator {
  def title = "Trivial VL4S demo"

  def makeSpec: Vl4sTopLevelSpec =
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


object WaveDemo extends SpecGenerator with BesselCalc {
  def title = "VL4S oscillatory functions"

  def makeSpec: Vl4sTopLevelSpec = {
    val xvals = (0.0 to 8.0 by 0.2).toSeq
    val curves = Map(
      "cosine" -> xvals.map { math.cos(_) },
      "sine" -> xvals.map { math.sin(_) },
      "J0" -> xvals.map { J(0)(_) },
      "J4" -> xvals.map { J(4)(_) } )
    val curveData = curves.flatMap { case (id, vals) =>
      xvals.zip(vals).map { case (x, y) =>
        Map( "func" -> id, "x" -> x, "y" -> y) } } . toSeq

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
          vtype(BasicType.nominal)))
  }
}


object BesselDemo extends SpecGenerator with BesselCalc {
  def title = "VL4S Bessel functions"

  def makeSpec: Vl4sTopLevelSpec = {
    val xvals = (0.0 to 10.0 by 0.2).toSeq
    val orders = (0 until 4).toSeq
    val labels = orders.map { n => s"J${n}" }
    val curveData = xvals.map { x => {
      orders.zip(labels).map {
        case (n, id) => id -> J(n)(x) }.toMap + ( "x" -> x ) }
    }

    RepeatedSpec() .
      data(InlineData() .
        values(curveData)) .
      repeat(Repeat() .
        column(labels)) .
      spec(CompuSpec() .
        mark(Mark.line) .
        encoding(Encoding() .
          x(PositionFieldDef() .
            field("x")) .
          y(PositionFieldDef() .
            field(RepeatRef() .
              repeat(RepeatRef_repeat.column))))) .
      resolve(Resolve() .
        scale(ScaleResolveMap() .
          y(ResolveMode.shared) .
          y(ResolveMode.shared)))
  }
}


object GaussMixDemo extends SpecGenerator {
  def title = "VL4S Gaussian mixtures"

  def makeSpec: Vl4sTopLevelSpec = {
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
            vtype(BasicType.quantitative) .
            bin(BinParams() .
              maxbins(20))) .
          y(enc.XYaxisDef .
            axis(Axis() .
              title("count")) .
            field("*") .
            vtype(BasicType.quantitative) .
            aggregate(AggregateOp.count)) .
          color(MarkPropValueDefWithCondition() .
            condition(ConditionalSelection_MarkPropFieldDef_() .
              field("label") .
              selection("chosen") .
              vtype(BasicType.nominal)) .
            value("grey"))
        }
      )
  }
}


object Demo {
  object Mode extends Enumeration {
    type Mode = Value
    val Trivial, Waves, Bessel, GaussMix = Value
  }
  implicit val modeRead: scopt.Read[Mode.Value] =
    scopt.Read.reads(Mode withName _)
  val generators: Map[Mode.Value, SpecGenerator] = Map(
    Mode.Trivial →  TrivialDemo,
    Mode.Waves →    WaveDemo,
    Mode.Bessel →   BesselDemo,
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
        val generator = generators(config.mode)
        val spec = generator.makeSpec
        val json = spec.toJValue

        val doc = config.outputFormat match {
          case Format.Json => pretty(render(json))
          case Format.Webpage => makeWebpage(spec, title=generator.title)
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

  def makeWebpage(spec: Vl4sTopLevelSpec, title: String = ""): String = {
    implicit val htmlSettings = HtmlSettings(vegaExport=true)

    s"""${spec.htmlPage(headerPrefix=s"<title>${title}</title>",
                        bodyPrefix=s"<h1>${title}</h1>")}"""
  }
}
