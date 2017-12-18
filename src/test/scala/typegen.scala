/*
 *  Scala code-generation unit-tests for VL4S
 *  RW Penney, December 2017
 */

//  Copyright (C) 2017, RW Penney
//  This file is part of VL4S.

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package uk.rwpenney.vl4s.gen.testing

import org.scalatest.FlatSpec

import uk.rwpenney.vl4s.gen._


object CodeHelpers {
  def fromBare(name: String) = CodeGen.toCodeable(VLbareType(name))
}


class TypeGen extends FlatSpec {
  "A bare type" should "have sensible names" in {
    val bare = VLbareType("BareType")
    val coded = CodeGen.toCodeable(bare)

    assert(coded.typename == "BareType")
    assert(coded.targetname == "BareType")
    assert(coded.toCode().trim == "")
  }

  it should "rename native types" in {
    val b_boolean = CodeHelpers.fromBare("boolean")
    assert(b_boolean.typename == "Boolean")
    assert(b_boolean.targetname == "Boolean")

    val b_null = CodeHelpers.fromBare("null")
    assert(b_null.typename == "Unit")
    assert(b_null.targetname == "Unit")

    val b_number = CodeHelpers.fromBare("number")
    assert(b_number.typename == "Double")
    assert(b_number.targetname == "Double")

    val b_string = CodeHelpers.fromBare("string")
    assert(b_string.typename == "String")
    assert(b_string.targetname == "String")
  }

  it should "handle invalid characters" in {
    val b0 = CodeHelpers.fromBare("Something<Peculiar>")
    assert(b0.typename == "Something_Peculiar_")
    assert(b0.targetname == "Something<Peculiar>")

    val b1 = CodeHelpers.fromBare("ContainerOf[Lemons]")
    assert(b1.typename == "ContainerOf_Lemons_")
    assert(b1.targetname == "ContainerOf[Lemons]")
  }


  "An array type" should "have sensible names" in {
    val arr = VLarrayOf("NumericArray", VLbareType("number"))
    val coded = CodeGen.toCodeable(arr)

    assert(coded.typename == "NumericArray")
    assert(coded.targetname == "Seq[Double]")
  }

  "A map type" should "have sensible names" in {
    val mp = VLmapOf("BoolMap", VLbareType("boolean"))
    val coded = CodeGen.toCodeable(mp)

    assert(coded.typename == "BoolMap")
    assert(coded.targetname == "Map[String, Boolean]")
  }

  "An enum type" should "mention all values" in {
    val randgen = new scala.util.Random()
    val evals = (1 to 50).map { _ => f"Enum-${randgen.nextInt(1 << 30)}%08x" }
    val enum = VLenumDefn("DummyEnum", evals)

    val coded = CodeGen.toCodeable(enum).toCode()
    evals.foreach { str =>
      assert(coded.contains(str))
      assert(coded.contains("final val " + str.replaceAll("-", "_")))
    }
  }
}
