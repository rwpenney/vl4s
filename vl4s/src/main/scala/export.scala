/*
 *  VL4S mechanisms for exporting JSON
 *  RW Penney, November 2017
 */

package uk.rwpenney.vl4s

import org.json4s.{ JBool, JDouble, JField, JInt, JObject, JString, JValue }


trait JsonExporter {
  def toJValue: JValue

  def dumpMap(props: Map[String, Any]): JValue = {
    JObject(props.toList.map { case (field, setting) =>
      val jvalue = setting match {
        case exp: JsonExporter => exp.toJValue
        case b: Boolean =>        JBool(b)
        case i: Int =>            JInt(i)
        case d: Double =>         JDouble(d)
        case s: String =>         JString(s)
        case _ =>                 JString(setting.toString)
      }
      JField(field, jvalue) })
  }
}
