package uk.rwpenney

/**
 *  vl4s is a Scala library for generating plotting instructions
 *  to be rendered by the Vega-Lite javascript library.
 *
 *  == Background ===
 *  <a href="https://vega.github.io/vega-lite">Vega-Lite</a>
 *  operates by converting a JSON document which describes
 *  one or more graphs (e.g. line plots, bar-chart, scatter-plot),
 *  and renders that within a browser using the Vega javascript library.
 *  The allowed structure and content of this JSON document
 *  is constrained by the Vega-Lite schema.
 *  The complexity of this schema means that it is difficult
 *  to guarantee that any JSON document will operate correctly
 *  when passed to Vega-Lite, especially if the JSON
 *  is generated dynamically by a program, or contains
 *  rich features such as multiple layers or selections.
 *
 *  <a href="https://github.com/rwpenney/vl4s">vl4s</a>
 *  is intended to make it easier for Scala applications
 *  to generate Vega-Lite plot specifications that conform
 *  correctly to the Vega-Lite schema.
 *  Datastructures within vl4s provide type-safe constraints
 *  which provide compile-time validation that a plot specification
 *  will match the schema.
 *
 *  == Relationship between API and schema ==
 *  vl4s is based on an automatic translation process that
 *  converts the official Vega-Lite
 *  <a href="https://github.com/vega/schema/tree/master/vega-lite">schema</a>
 *  directly into Scala source-code.
 *  (The version of the upstream schema is preserved within
 *  the [[MetaData]] object.)
 *  This translation is intended to preserve as much as possible
 *  of the naming and interrelationships between datatypes
 *  within the schema, and those in the vl4s package.
 *  For example, the properties in the "Axis" class in the schema
 *  are converted into methods such as [[Axis.domain]](),
 *  [[Axis.labelOverlap]]() in the [[Axis]] class,
 *  whose call signatures are directly derived from the schema.
 *
 *  The translation between the schema and the vl4s API
 *  operates on the following principles:
 *  <ul>
 *    <li>Native JSON types are converted to corresponding Scala native types,
 *        e.g. "boolean" -> Boolean, "null" -> Unit,
 *        "number" -> Double, "string" -> String.</li>
 *    <li>All Vega-Lite type-definitions and parameters
 *        are mapped to Scala types of the same name,
 *        except where these are not valid Scala identifiers</li>
 *    <li>Vega-Lite identifiers that contain characters such as '&lt';, '&gt';
 *        have these converted to '_',
 *        for example "TopLevel<FacetedUnitSpec>"
 *        becomes [[Toplevel_FacetedUnitSpec_]].</li>
 *    <li>Properties of each Vega-Lite class become methods
 *        within the corresponding vl4s class.
 *        For example, the "mark" property of the "TopLevel<FacetedUnitSpec>"
 *        becomes the [[Toplevel_FacetedUnitSpec_.mark]]() method,
 *        which expects to be passed an [[AnyMark]] object.</li>
 *    <li>Properties that conflict with Scala reserved words
 *        have fixed replacements, such as "type" becomes "vtype",
 *        and "wait" becomes "vwait".
 *  </ul>
 *  Scaladoc documentation for all classes directly derived
 *  from the Vega-Lite schema is extracted from the schema itself.
 *
 *  Some additional hand-written classes are also provided
 *  to assist with generating html web-pages from a [[Vl4sTopLevelSpec]],
 *  such as the [[ExportImplicits.HtmlExporter]].
 *
 *  == Key classes ==
 *  [[SimpleSpec]]
 *  [[InlineData]]
 *  [[Mark]]
 *  [[EncodingWithFacet]]
 *  [[PositionFieldDef]]
 */
package object vl4s extends Vl4sSpecAliases {
  val CompuSpec = CompositeUnitSpecAlias

  /** Helpers for displaying plots in Apache Zeppelin notebooks */
  object ZeppelinImplicits {
    import vl4s.ExportImplicits.HtmlExporter
    var htmlSettings = vl4s.HtmlSettings()
    implicit class Renderer(spec: Vl4sTopLevelSpec) {
      def render(): Unit = {
        print(s"%html ${spec.htmlIframe()(htmlSettings)}")
      }
    }
  }
}
