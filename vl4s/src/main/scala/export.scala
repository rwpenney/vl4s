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


object ExportImplicits {
  implicit class HtmlExporter(spec: TopLevelSpec) {
    import org.json4s.native.JsonMethods.{ compact, pretty, render }

    val cdnUrlPrefix = "https://cdnjs.cloudflare.com/ajax/libs"

    val jsLibraries = Seq(
      s"${cdnUrlPrefix}/vega/3.0.7/vega.js",
      s"${cdnUrlPrefix}/vega-lite/${MetaData.schemaVersion}/vega-lite.js",
      s"${cdnUrlPrefix}/vega-embed/3.0.0-rc7/vega-embed.js")

    def jsImports(indent: String = ""): String = {
      jsLibraries.map {
        url => s"""${indent}<script src="${url}"></script>""" } .
      mkString("\n")
    }

    def htmlPage(headerPrefix: String = "",
                 bodyPrefix: String = ""): String = {
      val ident = makeIdent
      s"""|<html>
          |<head>${headerPrefix}
          |  <meta http-equiv="content-type"
          |    content="text/html; charset=utf-8"/>
          |  <style type="text/css">
          |    body { background-color: white; }
          |  </style>
          |${jsImports(indent="  ")}
          |</head>
          |<body>${bodyPrefix}
          |${htmlDiv(ident=ident, prettyJson=true)}
          |</body>
          |</html>""" . stripMargin
    }

    def htmlDiv(ident: String, jsvarname: String = "vegaSpec",
                prettyJson: Boolean = false): String = {
      val layout = if (prettyJson) pretty _ else compact _
      val jsonSpec = layout(render(spec.toJValue))

      s"""|<div id="${ident}" style="width: 100%;"></div>
          |<script type="text/javascript">
          |var ${jsvarname}_opts = { "mode": "vega-lite", "renderer": "svg",
          |  "actions": { "editor": false, "export": true, "source": false } }
          |var ${jsvarname} = ${jsonSpec}
          |vegaEmbed("#${ident}", ${jsvarname}, ${jsvarname}_opts);
          |</script>""" . stripMargin
    }

    def htmlIframe(): String = {
      val ident = makeIdent + "-iframe"
      s"""|<iframe id="${ident}"
          |  sandbox="allow-scripts allow-same-origin"
          |  style="border: none; width: 100%;"
          |  srcdoc="${scala.xml.Utility.escape(htmlPage())}">
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
