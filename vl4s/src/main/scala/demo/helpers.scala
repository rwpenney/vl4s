/*
 *  Supporting routines for VL4S (Vega-Lite for Scala) demonstrations
 *  RW Penney, December 2017
 */

//  Copyright (C) 2017, RW Penney
//  This file is part of VL4S.

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package uk.rwpenney.vl4s.demo

import scala.annotation.tailrec


/** Methods for computing Bessel functions of the first kind */
trait BesselCalc {
  def J(n: Integer)(x: Double) = {
    addTerms(JnxArgs(order=n, x=x), 0.0,
             math.pow(0.5 * x, n.toDouble), 0,
             factorial(n))
  }

  protected case class JnxArgs(order: Integer, x: Double) {
    val xSqr = x * x }

  @tailrec
  protected final def addTerms(nx: JnxArgs, accum: Double,
                               xpow: Double, s: Integer,
                               denom: math.BigInt): Double = {
    val sgn = if ((s % 2) == 1) -1 else +1
    val term = sgn * xpow  / denom.toDouble
    if (math.abs(term) < 1e-16) {
      accum + term
    } else {
      addTerms(nx, accum + term,
               xpow * nx.xSqr, s + 1,
               4 * (s + 1) * (s + nx.order + 1) * denom)
    }
  }

  protected def factorial(n: Int): math.BigInt = {
    if (n > 1) {
      (math.BigInt(1) to n).product
    } else {
      1
    }
  }
}


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
