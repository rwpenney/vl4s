/*
 *  Vega-Lite schema parsing for VL4S
 *  RW Penney, November 2017
 */

//  Copyright (C) 2017, RW Penney
//  This file is part of VL4S.
//
//  VL4S is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  Foobar is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with VL4S.  If not, see <http://www.gnu.org/licenses/>.

package uk.rwpenney.vl4s.gen

import org.json4s.{ JArray, JField, JObject, JString, JValue }
import org.json4s.native.JsonMethods.{ parse => J4Sparse }
import scala.annotation.tailrec


sealed abstract class VLtypeDefn(val name: String)

case class VLbareType(override val name: String) extends VLtypeDefn(name)

case class VLarrayOf(vltype: VLtypeDefn) extends VLtypeDefn("[array]")

case class VLenumDefn(override val name: String,
                      values: Seq[String]) extends VLtypeDefn(name)

case class VLanyOf(override val name: String,
                   options: Seq[VLtypeDefn]) extends VLtypeDefn(name)

case class VLproperty(name: String,
                      vltype: VLtypeDefn,
                      description: Option[String] = None)

case class VLopDefn(override val name: String,
                    properties: Seq[VLproperty]) extends VLtypeDefn(name)

case class VLobjRef(val alias: String,
                    target: VLtypeDefn) extends VLtypeDefn(alias)

/** Representation of all VegaLite types extracted from a JSON schema */
class VLschema(val types: Seq[VLtypeDefn]) {
  /** Recursively search for all object-references within the schema */
  def objRefs: Seq[VLobjRef] = {
    @tailrec
    def recurse(vltypes: Seq[VLtypeDefn],
                objs: Seq[VLobjRef]): Seq[VLobjRef] = {
      vltypes match {
        case (or: VLobjRef) :: tail => recurse(tail, objs :+ or)
        case (op: VLopDefn) :: tail => {
          val children = op.properties.map { _.vltype }
          recurse(children ++ tail, objs)
        }
        case _ +: tail => recurse(tail, objs)
        case _ => objs
      }
    }

    recurse(types, Nil)
  }
}


/** Ingest Vega-Lite JSON schema, converting to object tree */
object SchemaParser {
  def apply(filename: String): VLschema = {
    val json = J4Sparse(new java.io.File(filename))
    new VLschema(digestTree(json))
  }

  /** Extract Vega-Lite class definitions from json4s document tree */
  def digestTree(root: JValue): Seq[VLtypeDefn] = {
    val JObject(doctree) = root

    val defns = for {
      JField("definitions", JObject(deftree)) <- doctree
      JField(vlTypeName, JObject(vlTypeSpec)) <- deftree
    } yield parseTypeDefn(vlTypeName, vlTypeSpec.toMap)

    defns
  }

  /** Extract single Vega-Lite type definition */
  def parseTypeDefn(vlTypeName: String,
                    spec: Map[String, JValue]): VLtypeDefn = {
    def objToMap(obj: JValue): Map[String, JValue] = {
      val JObject(items) = obj
      items.map {
        case JField(field, value) => (field, value) } .toMap
    }

    spec match {
      case _ if spec.contains("anyOf") =>
        parseAnyOf(vlTypeName, spec)
      case _ if spec.contains("enum") =>
        parseEnumDefn(vlTypeName, spec)
      case _ if spec.contains("properties") || spec.isEmpty =>
        parseOpDefn(vlTypeName, spec)
      case _ if spec.contains("$ref") =>
        parseRef(vlTypeName, spec)
      case _ if spec.contains("type") => {
        spec("type") match {
          case JString("array") =>
            VLarrayOf(parseTypeDefn(vlTypeName + "_array",
                                    objToMap(spec("items"))))
          case JString("object") =>
            VLobjRef(vlTypeName,
                     parseRef(vlTypeName + "_ref",
                              objToMap(spec("additionalProperties"))))
          case JString(x) =>
            VLbareType(x)
          case JArray(tuple) => {
            println(s"WARNING: partial handling of ${vlTypeName} ~ ${tuple}")
            // FIXME - more here
            VLbareType("ref")
          }
          case _ => {
            println(s"ERROR: Unknown type for ${vlTypeName}")
            VLbareType("UNKNOWN")
          }
        }
      }
      case _ => {
        println(s"ERROR: Unable to cast ${vlTypeName}")
        VLbareType("UNKNOWN")
      }
    }
  }

  val refRegex = """#/definitions/(.*)""".r

  def parseRef(name: String, spec: Map[String, JValue]): VLbareType = {
    val JString(xref) = spec("$ref")

    xref match {
      case refRegex(vltype) => VLbareType(vltype)
      case _ => {
        println(s"ERROR: Unable to cross-reference from cast ${name}")
        VLbareType("UNKNOWN")
      }
    }
  }

  def parseEnumDefn(name: String,
                    spec: Map[String, JValue]): VLenumDefn = {
    val terms = for { JString(term) <- spec("enum") } yield term

    spec.get("type") match {
      case Some(JString(vltype)) => {
        if (vltype != "string") {
          println(s"WARNING: non-string enum for ${name}")
        }
      }
      case _ =>
    }

    VLenumDefn(name, terms)
  }

  def parseOpDefn(opname: String,
                  spec: Map[String, JValue]): VLopDefn = {
    val propdefs = spec.get("properties") match {
      case Some(JObject(props)) => for {
        JField(propname, JObject(propdef)) <- props
      } yield (propname, propdef.toMap)
      case _ => Nil
    }

    val props = propdefs.map {
      case (propname, fields) => parsePropDefn(opname, propname, fields)
    }

    // FIXME - do something with "additionalProperties" field

    VLopDefn(opname, properties=props)
  }

  def parsePropDefn(opname: String, propname: String,
                    spec: Map[String, JValue]): VLproperty = {
    VLproperty(propname,
               vltype = parseTypeDefn(opname + "_" + propname, spec),
               description = spec.get("description") match {
                 case Some(JString(x)) => Some(x)
                 case _ => None })
  }

  def parseAnyOf(name: String,
                 spec: Map[String, JValue]): VLanyOf = {
    VLanyOf(name, Nil)
    // FIXME - much more here
  }
}
