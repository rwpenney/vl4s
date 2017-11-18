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

case class VLopDefn(name: String) extends VLtypeDefn(name)


class VLschema(val types: Seq[VLtypeDefn]) {} // FIXME - much more here


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
    println(s"DEFNS: ${defns}")

    defns
  }

  /** Extract single Vega-Lite class definition */
  def parseDefn(vlTypeName: String,
                vlTypeSpec: Map[String, JValue]): VLtypeDefn = {
    if (vlTypeSpec.contains("enum")) {
      val terms = for { JString(term) <- vlTypeSpec("enum") } yield term
      val et = vlTypeSpec.getOrElse("type", "string")
      VLenumDefn(vlTypeName, terms)
    } else {
      VLopDefn(vlTypeName)
    }
  }
}
