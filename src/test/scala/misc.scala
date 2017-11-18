/*
 *  Miscellaneous unit-tests for VL4S
 *  RW Penney, November 2017
 */

package uk.rwpenney.vl4s.gen.testing

import org.scalatest.FlatSpec


class Pending extends FlatSpec {
  "integers" should "behave unlike floats" in {
    assert(1 + 1 == 2)
    assert(1 + 1 != 0.9 + 2.3 - 1.2)
  }
}
