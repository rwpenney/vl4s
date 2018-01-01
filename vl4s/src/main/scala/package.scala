package uk.rwpenney

package object vl4s {
  val CompuSpec = CompositeUnitSpecAlias

  val FacetedUnitSpec = TopLevel_FacetedUnitSpec_
  val RepeatedSpec =    TopLevel_RepeatSpec_
  val SimpleSpec =      TopLevel_FacetedUnitSpec_


  /** Helpers for displaying plots in Apache Zeppelin notebooks */
  object ZeppelinImplicits {
    import vl4s.ExportImplicits.HtmlExporter
    var htmlSettings = vl4s.HtmlSettings()
    implicit class Renderer(spec: TopLevelSpec) {
      def render(): Unit = {
        print(s"%html ${spec.htmlIframe()(htmlSettings)}")
      }
    }
  }
}
