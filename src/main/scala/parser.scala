/*
 *  Vega-Lite schema parsing for VL4S
 *  RW Penney, November 2017
 */

package uk.rwpenney.vl4s.gen

import org.json4s.{ JArray, JField, JObject, JString, JValue }
import org.json4s.native.JsonMethods.{ parse => J4Sparse }


abstract class VLtypeDefn(name: String)

case class VLbareType(name: String) extends VLtypeDefn(name)

case class VLarrayOf(vltype: VLtypeDefn) extends VLtypeDefn("[array]")

case class VLenumDefn(name: String,
                      values: Seq[String]) extends VLtypeDefn(name)

case class VLanyOf(name: String,
                   options: Seq[VLtypeDefn]) extends VLtypeDefn(name)

case class VLproperty(name: String, vltype: VLtypeDefn,
                      description: Option[String] = None)

case class VLopDefn(name: String,
                    properties: Seq[VLproperty]) extends VLtypeDefn(name)

class VLschema(val types: Seq[VLtypeDefn])


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
    spec match {
      case _ if spec.contains("anyOf") =>
        VLbareType("any")   // FIXME - more here
      case _ if spec.contains("enum") =>
        parseEnumDefn(vlTypeName, spec)
      case _ if spec.contains("properties") || spec.isEmpty =>
        parseOpDefn(vlTypeName, spec)
      case _ if spec.contains("$ref") =>
        VLbareType("ref")   // FIXME - more here
      case _ if spec.contains("type") => {
        spec("type") match {
          case JString("array") => {
            val JObject(items) = spec("items")
            VLarrayOf(parseTypeDefn(vlTypeName + "_array",
                                    items.map {
                                      case JField(f, v) => (f, v) }.toMap))
          }
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
}
