/*
 *  Vega-Lite schema parsing for VL4S
 *  RW Penney, November 2017
 */

//  Copyright (C) 2017, RW Penney
//  This file is part of VL4S.

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package uk.rwpenney.vl4s.gen

import org.json4s.{ JArray, JField, JObject, JString, JValue }
import org.json4s.native.JsonMethods.{ parse => J4Sparse }
import scala.annotation.tailrec


/** Representation of a generic VegaLite datatype */
sealed abstract class VLtypeDefn(val name: String)

trait VLtypeContainer {
  def expandContents: (Option[VLtypeDefn], Seq[VLtypeDefn])
}

/** Helper methods for manipulating VegaLite datatype descriptions */
object VLtypeDefn {

  /** Recursively expand children of a set of parent datatypes */
  def expandDependencies(roots: Seq[VLtypeDefn]): Seq[VLtypeDefn] = {
    @tailrec
    def recurse(vltypes: Seq[VLtypeDefn],
                locals: Seq[Option[VLtypeDefn]]): Seq[Option[VLtypeDefn]] = {
      vltypes match {
        case head :: tail => {
          val (loc, children) = head match {
            case ctr: VLtypeContainer =>  ctr.expandContents
            case _ =>                     (None, Nil)
          }
          recurse(children ++ tail, locals :+ loc)
        }
        case _ => locals
      }
    }

    recurse(roots, Nil) . flatten
  }
}

/** A simple VegaLite datatype, typically corresponding to a native type */
case class VLbareType(override val name: String) extends VLtypeDefn(name)

/** A VegaLite datatype representing a homogeneous sequence */
case class VLarrayOf(override val name: String, vltype: VLtypeDefn)
    extends VLtypeDefn(name) with VLtypeContainer {
  def expandContents() = (None, Seq(vltype))
}

/** A VegaLite homogeneous object datatype keyed by strings */
case class VLmapOf(override val name: String, vltype: VLtypeDefn)
    extends VLtypeDefn(name) with VLtypeContainer {
  def expandContents() = (None, Seq(vltype))
}

/** A VegaLite enumerated datatype, typically consisting of multiple strings */
case class VLenumDefn(override val name: String,
                      values: Seq[String]) extends VLtypeDefn(name)
    with VLtypeContainer {
  def expandContents() = (Some(this), Nil)
}

/** A VegaLite datatype expressing a collection of equivalent options */
case class VLanyOf(override val name: String,
                   options: Seq[VLtypeDefn])
    extends VLtypeDefn(name) with VLtypeContainer {
  def expandContents() = (Some(this), options)
}

/** A field within a VegaLite operator definition */
case class VLproperty(name: String,
                      vltype: VLtypeDefn,
                      description: Option[String] = None)

/** Representation of A VegaLite operator */
case class VLopDefn(override val name: String, properties: Seq[VLproperty])
    extends VLtypeDefn(name) with VLtypeContainer {
  def expandContents() = (Some(this), Nil)
}

/** Representation of a typename synonym within a set of VegaLite datatypes */
case class VLobjRef(val alias: String,
                    target: VLtypeDefn) extends VLtypeDefn(alias)


/** Representation of all VegaLite types extracted from a JSON schema */
class VLschema(val types: Seq[VLtypeDefn], val version: String) {

  /** Extract all top-level object references (likely to need typedefs) */
  def objRefs: Seq[VLtypeDefn] = {
    types.flatMap { x =>
      x match {
        case m: VLmapOf => Some(m)
        case o: VLobjRef => Some(o)
        case _ => None
      }
    }
  }
}


/** Ingest Vega-Lite JSON schema, converting to object tree */
object SchemaParser {
  def apply(vsrc: VersionedSource): VLschema = {
    val json = J4Sparse(vsrc.src.reader)
    new VLschema(digestTree(json), vsrc.version)
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

  def objToMap(obj: JValue): Map[String, JValue] = {
    val JObject(items) = obj
    items.map {
      case JField(field, value) => (field, value) } .toMap
  }

  /** Extract single Vega-Lite type definition */
  def parseTypeDefn(vlTypeName: String,
                    spec: Map[String, JValue]): VLtypeDefn = {
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
            VLarrayOf(vlTypeName,
                      parseTypeDefn(vlTypeName + "_elts",
                                    objToMap(spec("items"))))
          case JString("object") => {
            spec.get("additionalProperties") match {
              case Some(addprops) =>
                VLmapOf(vlTypeName,
                        parseTypeDefn(vlTypeName + "_vals",
                                      objToMap(addprops)))
              case None => VLbareType("object")
            }
          }
          case JString(x) =>
            VLobjRef(vlTypeName, VLbareType(x))
          case JArray(arr) => {
            val ao = VLanyOf(vlTypeName,
                             arr.flatMap {
                                case JString(sub) => Some(VLbareType(sub))
                                case _ => None })
            if (ao.options.length != arr.length) {
              println(s"ERROR: unhandled child types in plain anyOf ${ao.name}")
            }
            ao
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

  def parseRef(name: String, spec: Map[String, JValue]): VLtypeDefn = {
    val JString(xref) = spec("$ref")

    xref match {
      case refRegex(vltype) => VLobjRef(name, VLbareType(vltype))
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
    val JArray(root) = spec("anyOf")
    val options = for { JObject(obj) <- root } yield JObject(obj)
    VLanyOf(name,
            options.zipWithIndex.map {
              case (obj, idx) => {
                val subname = s"${name}_${idx}"
                parseTypeDefn(subname, objToMap(obj))
              } } )
  }
}
