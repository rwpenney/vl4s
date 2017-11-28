/*
 *  Shorthand wrappers for auto-generated VL4S classes
 */

package uk.rwpenney.vl4s

import scala.language.implicitConversions


object ShortcutImplicits {
  /** Allow specification of data source from plain string */
  implicit def dataFromRawUrl(rawUrl: String): Data = {
    new Data(UrlData() . url(rawUrl))
  }

  implicit def markFieldCondFromMap(conditions: Map[String, String]):
      MarkPropFieldDefWithCondition_condition = {
    new MarkPropFieldDefWithCondition_condition(toConditionalSeq(conditions))
  }

  implicit def markValCondFromMap(conditions: Map[String, String]):
      MarkPropValueDefWithCondition_condition = {
    new MarkPropValueDefWithCondition_condition(toConditionalSeq(conditions))
  }

  def toConditionalSeq(conditions: Map[String, String]):
      Seq[Conditional_ValueDef_] = {
    conditions.toSeq.map {
      case (sel, value) => Conditional_ValueDef_() .
                            selection(sel) . value(value)
    }
  }
}
