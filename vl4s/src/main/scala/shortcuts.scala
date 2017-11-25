/*
 *  Shorthand wrappers for auto-generated VL4S classes
 */

package uk.rwpenney.vl4s

import scala.language.implicitConversions


object ShortcutImplicits {
  /** Allow specification of data source from plain string */
  implicit def fromRawUrl(rawUrl: String): Data = {
    new Data(UrlData() . url(rawUrl))
  }
}
