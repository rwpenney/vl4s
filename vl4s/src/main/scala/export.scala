/*
 *  VL4S mechanisms for exporting JSON
 *  RW Penney, November 2017
 */

//  Copyright (C) 2017, RW Penney
//  This file is part of VL4S.

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package uk.rwpenney.vl4s

import org.json4s.{ JArray, JBool, JDouble, JField, JInt,
                    JObject, JString, JValue }


trait JsonExporter {
  def toJValue: JValue

  def exportTerm(term: Any): JValue = {
    term match {
      case exp: JsonExporter => exp.toJValue
      case b: Boolean =>        JBool(b)
      case i: Int =>            JInt(i)
      case d: Double =>         JDouble(d)
      case s: String =>         JString(s)
      case s: Seq[Any] =>       JArray(s.map{ exportTerm(_) }.toList)
      case _ =>                 JString(term.toString)
    }
  }

  def exportMap(props: Map[String, Any]): JValue = {
    JObject(props.toList.map {
      case (field, setting) => JField(field, exportTerm(setting)) })
  }
}
