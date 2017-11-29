/*
 *  Schema to source-code converter for VL4S
 *  RW Penney, November 2017
 */

//  Copyright (C) 2017, RW Penney
//  This file is part of VL4S.

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package uk.rwpenney.vl4s.gen

import java.io.{ OutputStream, PrintWriter }
import scala.annotation.tailrec


/** Mechanism for converting a VegaLite type into Scala source-code */
trait TypeCoder {
  /** The Scala typename */
  def typename: String

  /** The Scala typename of any underlying type to which this refers */
  def targetname: String = typename

  /** Generate source-code for this type */
  def toCode(recursive: Boolean = true): String
}


/** Helper methods for converting container types into Scala source-code */
trait ParentCoder {
  def makeHelperClasses(roots: Seq[VLtypeDefn],
                        recursive: Boolean = true): Option[String] = {
    val locals = if (recursive) VLtypeDefn.expandDependencies(roots)
                 else roots

    if (locals.nonEmpty) {
      Some(locals.map { CodeGen.toCodeable(_) } .
            flatMap { c =>
              val codetext = c.toCode()
              if (codetext.nonEmpty) Some(codetext) else None } .
            mkString("\n"))
    } else {
      None
    }
  }
}


class EmptyCoder extends TypeCoder {
  def typename = "[EMPTY]"
  def toCode(recursive: Boolean = true) = ""
}


class BareCoder(defn: VLbareType) extends TypeCoder {
  def typename = CodeGen.cleanClassName(
                    CodeGen.mapBareTypes.getOrElse(defn.name, defn.name))
  def toCode(recursive: Boolean = true) = ""
}


class ArrayCoder(defn: VLarrayOf) extends TypeCoder with ParentCoder {
  val itemtype = CodeGen.toCodeable(defn.vltype).targetname

  def typename = CodeGen.cleanClassName(defn.name)
  override def targetname = s"Seq[${itemtype}]"
  def toCode(recursive: Boolean = true) =
    makeHelperClasses(Seq(defn.vltype)).getOrElse("")
}


class EnumCoder(defn: VLenumDefn) extends TypeCoder {
  def typename = defn.name

  def toCode(recursive: Boolean = true): String = {
    val terms = defn.values.map { term =>
      s"""|  final val ${cleanName(term)} = new ${typename}(term = "${term}")"""
    } . mkString("\n")

    s"""|sealed class ${typename}(val term: String) extends JsonExporter {
        |  def toJValue = JString(term)
        |}
        |object ${typename} {
        ${terms}
        |}
        |""" . stripMargin
  }

  def cleanName(orig: String): String =
    orig.replaceAll("-", "_")
}


class AnyOfCoder(defn: VLanyOf) extends TypeCoder with ParentCoder {
  def typename = CodeGen.cleanClassName(defn.name)

  def toCode(recursive: Boolean = true): String = {
    val options = defn.options.map { opt => {
      val cdbl = CodeGen.toCodeable(opt)
        s"""|  implicit def from${cdbl.typename}(_arg: ${cdbl.targetname}) =
            |    new ${typename}(choice = _arg)""" . stripMargin
      }
    } . mkString("\n")
    // FIXME - replace dummy member

    Seq(if (recursive) makeHelperClasses(defn.options) else None,
        Some(s"""|class ${typename}(val choice: Any) extends JsonExporter {
                 |  def toJValue = exportTerm(choice) }""" . stripMargin),
        Some(s"object ${typename} {"),
        Some(options),
        Some("}")) . flatten . mkString("", "\n", "\n\n")
  }
}


class TupleCoder(defn: VLtupleDefn) extends TypeCoder with ParentCoder {
  val elementnames = defn.elements.map {
    elt => CodeGen.toCodeable(elt).targetname }

  def typename = CodeGen.cleanClassName(defn.name)
  override def targetname = elementnames.mkString("(", ", ", ")")

  def toCode(recursive: Boolean = true): String = ""
}


/** Code-generator for VegaLite operators */
class OperatorCoder(defn: VLopDefn) extends TypeCoder with ParentCoder {
  def typename = CodeGen.cleanClassName(defn.name)

  val fieldTypes = defn.properties.map { prop =>
    ( prop.name, CodeGen.toCodeable(prop.vltype).targetname )
  } . toMap
  val methodNames = defn.properties.map { prop =>
    ( prop.name, CodeGen.mapReserved.getOrElse(prop.name, prop.name) )
  } . toMap

  def toCode(recursive: Boolean = true): String = {
    val markers = makeMarkers()
    val propsetters = defn.properties.map { prop =>
      makePropMethod(prop) } .mkString("\n")

    Seq(makeHelperClasses(defn.properties.map { _.vltype }, recursive=false),
        Some(s"case class ${typename}" +
              "(_properties: Map[String, Any] = Map.empty)" +
              s"\n    extends JsonExporter ${markers}{"),
        Some(s"""|  def toJValue: JValue = exportMap(_properties)
                 |  def >>(fn: ${typename} => ${typename}) = fn(this)"""
              . stripMargin),
        if (propsetters.nonEmpty) Some(propsetters) else None,
        Some("}")) . flatten . mkString("", "\n", "\n\n")
  }

  /** Prepare "with" annotations for class definition */
  def makeMarkers(): String = {
    val traitOptions = CodeGen.markerInterfaces ++ CodeGen.shorthandInterfaces
    traitOptions.flatMap { case (regexp, marker) =>
      regexp.findFirstIn(defn.name) match {
        case Some(_) => Some(s"with ${marker} ")
        case None =>    None
      }
    } . mkString("")
  }

  /** Create setter method definition for single property */
  def makePropMethod(prop: VLproperty): String = {
    val method = methodNames(prop.name)
    val argtype = fieldTypes(prop.name)
    val doc = prop.description match {
      case Some(desc) => Some(s"  /** ${desc} */")
      case None => None
    }

    val code = s"""|  def ${method}(__arg: ${argtype}): ${typename} =
                   |    this.copy(_properties = this._properties +
                   |               ("${prop.name}" → __arg))""" . stripMargin

    Seq(doc, Some(code)) . flatten .
      mkString("", "\n", "\n")
  }
}


class ObjRefCoder(defn: VLobjRef) extends TypeCoder {
  def typename = CodeGen.cleanClassName(defn.alias)
  val targetcoder = CodeGen.toCodeable(defn.target)
  override def targetname = CodeGen.cleanClassName(targetcoder.targetname)

  def toCode(recursive: Boolean = true) = ""

  def toAliasCode(recursive: Boolean = true) = {
    s"""object ${typename} {
    |  type ${typename} = ${targetname}
    |}
    |import ${typename}._
    |""" . stripMargin
  }
}


class CodeGen(val stream: OutputStream) {
  def apply(schema: VLschema) {
    val pw = new PrintWriter(stream)

    val warning = "/* AUTOGENERATED by VL4S - do not edit by hand */\n"

    pw.print(warning)
    pw.print("""
             |package uk.rwpenney.vl4s
             |
             |import org.json4s.{ JString, JValue }
             |import scala.language.implicitConversions
             |
             |""" . stripMargin)

    pw.print(CodeGen.markerInterfaces.map {
                case (re, mrk) => s"trait ${mrk} extends JsonExporter" } .
             mkString("", "\n", "\n\n"))
    // FIXME - allow for marker traits not extending JsonExporter

    CodeGen.makeTypeRefs(schema) match {
      case Some(typerefs) => pw.print(typerefs)
      case None =>
    }

    schema.types.foreach { vltype =>
      vltype match {
        case or: VLobjRef =>  // top-level references handled by makeTypeRefs()
        case _ => {
          val codeable = CodeGen.toCodeable(vltype)
          val codetext = codeable.toCode(recursive=true)

          if (codetext.nonEmpty) {
            pw.print(s"\n${codetext}")
          }
        }
      }
    }

    pw.print("\n" + warning)
    pw.close()
  }
}


object CodeGen {
  /** Convert VegaLite type descriptor into source-code generator object */
  def toCodeable(vltype: VLtypeDefn): TypeCoder = {
    vltype match {
      case bare: VLbareType =>  new BareCoder(bare)
      case arr: VLarrayOf =>    new ArrayCoder(arr)
      case enum: VLenumDefn =>  new EnumCoder(enum)
      case ao: VLanyOf =>       new AnyOfCoder(ao)
      case tpl: VLtupleDefn =>  new TupleCoder(tpl)
      case op: VLopDefn =>      new OperatorCoder(op)
      case or: VLobjRef =>      new ObjRefCoder(or)
      case _ =>                 new EmptyCoder
    }
  }

  /** Generate code for type aliases */
  def makeTypeRefs(schema: VLschema): Option[String] = {
    val objrefs = schema.objRefs

    if (objrefs.nonEmpty) {
      Some((Seq("object TypeRefs {") ++
            objrefs.map { ref =>
              val codeable = toCodeable(ref)
              s"  type ${codeable.typename} = ${codeable.targetname}" } ++
            Seq("}", "import TypeRefs._")) . mkString("", "\n", "\n"))
    } else {
      None
    }
  }

  /** Conversion for VL properties which clash with Scala reserved words */
  val mapReserved = Map(
    "type" -> "vtype"
  )

  /** Conversion between VL raw types and Scala native types */
  val mapBareTypes = Map(
    "boolean" ->  "Boolean",
    "null" ->     "Unit",
    "number" ->   "Double",
    "object" ->   "Any",
    "string" ->   "String"
  )

  /** Convert VegaLite typename into valid Scala identifier */
  def cleanClassName(orig: String): String =
    orig.map {
      case '<' => '_'
      case '>' => '_'
      case c =>   c
    }

  /** Marker traits to apply to VegaLite operators based on classname regexps */
  val markerInterfaces = Map(
    raw"^TopLevel".r -> "TopLevelSpec"
  )

  /** Mixins to apply to VegaLite operators based on classname regexps */
  val shorthandInterfaces = Map(
    raw"^Encoding[A-Za-z]*".r -> "EncodingShorthands"
  )
}
