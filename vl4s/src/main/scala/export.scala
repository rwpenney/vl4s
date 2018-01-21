/*
 *  VL4S mechanisms for exporting JSON
 *  RW Penney, November 2017
 */

//  Copyright (C) 2017-2018, RW Penney
//  This file is part of VL4S.

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package uk.rwpenney.vl4s

import org.json4s.{ JArray, JBool, JDouble, JField, JInt,
                    JObject, JString, JValue }


/** Converter from native types to json4s wrappers */
trait JsonExporter {
  def toJValue: JValue

  def exportTerm(term: Any): JValue = {
    term match {
      case exp: JsonExporter => exp.toJValue
      case b: Boolean =>        JBool(b)
      case i: Int =>            JInt(i)
      case d: Double =>         JDouble(d)
      case m: Map[String @unchecked, Any @unchecked] =>
                    JObject(m.toList.map { kv => (kv._1, exportTerm(kv._2)) })
      case s: String =>         JString(s)
      case s: Seq[Any] =>       JArray(s.map{ exportTerm(_) }.toList)
      case _ =>                 JString(term.toString)
    }
    // FIXME - find tighter type test for Map[String,Any]
  }

  def exportMap(props: Map[String, Any]): JValue = {
    JObject(props.toList.map {
      case (field, setting) => JField(field, exportTerm(setting)) })
  }
}


/** Configuration options for HTML output */
case class HtmlSettings(
  cdnUrlPrefix: String = "https://cdnjs.cloudflare.com/ajax/libs",
  prettyJson: Boolean = false,
  vegaEditor: Boolean = false,
  vegaExport: Boolean = false,
  vegaRenderer: String = "svg",
  vegaSource: Boolean = false
)


object ExportImplicits {
  implicit class HtmlExporter(spec: TopLevelSpec) {
    import org.json4s.native.JsonMethods.{ compact, pretty, render }

    def jsLibraries(implicit config: HtmlSettings): Seq[String] = {
      val pfx = config.cdnUrlPrefix
      Seq(s"${pfx}/vega/3.0.9/vega.js",
          s"${pfx}/vega-lite/${MetaData.schemaVersion}/vega-lite.js",
          s"${pfx}/vega-embed/3.0.0-rc7/vega-embed.js")
    }

    def jsImports(indent: String = "")
                 (implicit config: HtmlSettings): String = {
      jsLibraries(config).map {
        url => s"""${indent}<script src="${url}"></script>""" } .
      mkString("\n")
    }

    def htmlPage(headerPrefix: String = "", bodyPrefix: String = "")
                (implicit config: HtmlSettings): String = {
      val ident = makeIdent
      s"""|<html>
          |<head>${headerPrefix}
          |  <meta http-equiv="content-type"
          |    content="text/html; charset=utf-8"/>
          |  <style type="text/css">
          |    body { background-color: white; }
          |  </style>
          |${jsImports(indent="  ")(config)}
          |</head>
          |<body>${bodyPrefix}
          |${htmlDiv(ident=ident)(config)}
          |</body>
          |</html>""" . stripMargin
    }

    def htmlDiv(ident: String, jsvarname: String = "vegaSpec")
               (implicit config: HtmlSettings): String = {
      val layout = if (config.prettyJson) pretty _ else compact _
      val jsonSpec = layout(render(spec.toJValue))

      val vOptions = Seq(
              """"mode": "vega-lite"""",
              s""""renderer": "${config.vegaRenderer}"""" ).mkString(", ")
      val vActions = Seq(s""""editor": ${config.vegaEditor}""",
                         s""""export": ${config.vegaExport}""",
                         s""""source": ${config.vegaSource}""").mkString(", ")

      s"""|<div id="${ident}" style="width: 100%;"></div>
          |<script type="text/javascript">
          |var ${jsvarname}_opts = { ${vOptions}, "actions": { ${vActions} } }
          |var ${jsvarname} = ${jsonSpec}
          |vegaEmbed("#${ident}", ${jsvarname}, ${jsvarname}_opts);
          |</script>""" . stripMargin
    }

    def htmlIframe()(implicit config: HtmlSettings): String = {
      val ident = makeIdent + "-iframe"
      s"""|<iframe id="${ident}"
          |  sandbox="allow-scripts allow-same-origin"
          |  style="border: none; width: 100%;"
          |  srcdoc="${scala.xml.Utility.escape(htmlPage()(config))}">
          |</iframe>
          |<script>
          |  (function() {
          |    function vl4sFitFrame(elt, attempt) {
          |      var targetHeight = elt.contentWindow.document.body.scrollHeight || '320';
          |      elt.style.height = (targetHeight + 16) + 'px';
          |      if (attempt <= 5) {
          |        setTimeout(function() { vl4sFitFrame(elt, attempt+1) },
          |                   500 * (0.5 + attempt));
          |      }
          |    }
          |    vl4sFitFrame(document.getElementById('${ident}'), 0);
          |  })();
          |</script>""" . stripMargin
    }

    def makeIdent: String = "vl4s-" + java.util.UUID.randomUUID.toString
  }
}
