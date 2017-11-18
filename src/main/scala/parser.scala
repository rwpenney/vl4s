/*
 *  Vega-Lite schema parsing for VL4S
 *  RW Penney, November 2017
 */

package uk.rwpenney.vl4s.gen

import org.json4s.{ JArray, JField, JObject, JString, JValue }
import org.json4s.native.JsonMethods.{ parse => J4Sparse }


abstract class VLtypeDefn(name: String)

case class VLenumDefn(name: String,
                      values: Seq[String]) extends VLtypeDefn(name)

case class VLproperty(name: String, vltype: String = "string",
                      description: Option[String] = None)
// FIXME - more flexible vltype model needed

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
    } yield parseDefn(vlTypeName, vlTypeSpec.toMap)

    defns
  }

  /** Extract single Vega-Lite type definition */
  def parseDefn(vlTypeName: String,
                vlTypeSpec: Map[String, JValue]): VLtypeDefn = {
    if (vlTypeSpec.contains("enum")) {
      parseEnumDefn(vlTypeName, vlTypeSpec)
    } else {
      parseOpDefn(vlTypeName, vlTypeSpec)
    }
  }

  def parseEnumDefn(name: String,
                    spec: Map[String, JValue]): VLenumDefn = {
    val terms = for { JString(term) <- spec("enum") } yield term
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
      case (name, fields) => parsePropDefn(name, fields)
    }

    // FIXME - much more here

    VLopDefn(opname, properties=props)
  }

  def parsePropDefn(name: String, spec: Map[String, JValue]): VLproperty = {
    VLproperty(name,
               vltype = spec.get("type") match {
                 case Some(JString(x)) => x
                 // FIXME - match richer types
                 case _ => "string" },
               description = spec.get("description") match {
                 case Some(JString(x)) => Some(x)
                 case _ => None })
  }
}
