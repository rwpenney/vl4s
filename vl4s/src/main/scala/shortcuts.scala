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
}


/** Type shorthands for use within axis encodings (e.g. EncodingWithFacet) */
trait EncodingShorthands {
  def XYaxisDef = PositionFieldDef()
}
