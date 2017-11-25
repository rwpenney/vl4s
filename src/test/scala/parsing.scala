/*
 *  JSON schema-parsing unit-tests for VL4S
 *  RW Penney, November 2017
 */

package uk.rwpenney.vl4s.gen.testing

import org.json4s.{ JArray, JField, JObject, JString, JValue }
import org.json4s.native.JsonMethods.{ parse => J4Sparse }
import scala.reflect.ClassTag
import org.scalatest.FlatSpec

import uk.rwpenney.vl4s.gen._


object Helpers {
  def parseJson(doc: String): Seq[VLtypeDefn] = {
    val root = J4Sparse(doc)
    SchemaParser.digestTree(root)
  }
}


/** Unit-tests for SchemaParser */
class Parser extends FlatSpec {
  "The SchemaParser" should "handle simple bare types" in {
    val typedefs = Helpers.parseJson("""{
        "definitions": {
          "Type0": { "type": "number" } }
      }""")

    val Seq(VLobjRef(defn, VLbareType(t0))) = typedefs

    assert(t0 == "number")
  }

  it should "extract all basic types" in {
    val typedefs = Helpers.parseJson("""{
        "definitions": {
          "Bare": { "type" : "string" },
          "Array": { "type": "array", "items": { "type": "number" } },
          "Enum": { "enum": [ "e0", "e1", "e2", "e3"] },
          "AnyOf": { "anyOf": [ { "$ref": "#/definitions/SomeAny" },
                                { "type": "boolean" } ] },
          "Tuple": { "type": [ "boolean", "number", "string" ] },
          "Reference": { "$ref": "#/definitions/SomeRef" },
          "Operator": {
            "properties": {
              "prop0": {  "description": "Property-zero",
                          "items": { "$ref": "#/definitions/SomeArray" },
                          "type": "array" },
              "prop1": {  "description": "Property-one",
                          "type": "object" } } }
        }
      }""")

    def typeCheck[T: ClassTag](vltype: VLtypeDefn)(fn: T => Unit) {
      vltype match {
        case x: T => fn(x)
        case _ => throw new IllegalArgumentException
      }
    }

    assert(typedefs.length == 7)

    typeCheck[VLobjRef](typedefs(0)) { ref =>
      typeCheck[VLbareType](ref.target) {
        bare => assert(bare.name == "string")
      }
    }

    typeCheck[VLarrayOf](typedefs(1)) { arr =>
      typeCheck[VLobjRef](arr.vltype) { ref =>
        typeCheck[VLbareType](ref.target) {
          bare => assert(bare.name == "number")
        }
      }
    }

    typeCheck[VLenumDefn](typedefs(2)) { enum =>
      assert(enum.values.length == 4)
      assert(enum.values == Seq("e0", "e1", "e2", "e3"))
    }

    typeCheck[VLanyOf](typedefs(3)) { ao =>
      assert(ao.options.length == 2)

      typeCheck[VLobjRef](ao.options(0)) { ref =>
        assert(ref.target.name == "SomeAny")
      }

      typeCheck[VLobjRef](ao.options(1)) { ref =>
        typeCheck[VLbareType](ref.target) {
          bare => assert(bare.name == "boolean")
        }
      }
    }

    typeCheck[VLanyOf](typedefs(4)) { ao =>
      assert(ao.options.length == 3)

      typeCheck[VLbareType](ao.options(0)) {
        bare => assert(bare.name == "boolean")
      }

      typeCheck[VLbareType](ao.options(1)) {
        bare => assert(bare.name == "number")
      }

      typeCheck[VLbareType](ao.options(2)) {
        bare => assert(bare.name == "string")
      }
    }

    typeCheck[VLobjRef](typedefs(5)) { ref =>
      assert(ref.target.name == "SomeRef")
    }

    typeCheck[VLopDefn](typedefs(6)) { obj =>
      assert(obj.properties.length == 2)

      assert(obj.properties(0).name == "prop0")
      typeCheck[VLarrayOf](obj.properties(0).vltype) { arr =>
        typeCheck[VLobjRef](arr.vltype) { ref =>
          assert(ref.target.name == "SomeArray")
        }
      }

      assert(obj.properties(1).name == "prop1")
      assert(obj.properties(1).vltype.name == "object")
    }
  }
}
