/*
 *  Schema to source-code converter for VL4S
 *  RW Penney, November 2017
 */

//  Copyright (C) 2017-2018, RW Penney
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
  def toCode(schema: VLschema = VLschema.empty,
             recursive: Boolean = true): String
}


/** Helper methods for converting container types into Scala source-code */
trait ParentCoder {
  def makeHelperClasses(roots: Seq[VLtypeDefn],
                        schema: VLschema = VLschema.empty,
                        recursive: Boolean = true): Option[String] = {
    val locals = if (recursive) VLtypeDefn.expandDependencies(roots)
                 else roots

    if (locals.nonEmpty) {
      Some(locals.map { CodeGen.toCodeable(_) } .
            flatMap { c =>
              val codetext = c.toCode(schema)
              if (codetext.nonEmpty) Some(codetext) else None } .
            mkString("\n"))
    } else {
      None
    }
  }
}


class EmptyCoder extends TypeCoder {
  def typename = "[EMPTY]"
  def toCode(vs: VLschema = VLschema.empty, recursive: Boolean = true) = ""
}


class NullCoder(defn: VLnullType) extends TypeCoder {
  override def typename = "Null"
  def toCode(vs: VLschema = VLschema.empty, recursive: Boolean = true) = ""
}


class BareCoder(defn: VLbareType) extends TypeCoder {
  override def targetname = CodeGen.mapBareTypes.getOrElse(defn.name, defn.name)
  def typename = CodeGen.cleanClassName(targetname)
  def toCode(vs: VLschema = VLschema.empty, recursive: Boolean = true) = ""
}


class ArrayCoder(defn: VLarrayOf) extends TypeCoder with ParentCoder {
  val itemtype = CodeGen.toCodeable(defn.vltype).targetname

  def typename = CodeGen.cleanClassName(defn.name)
  override def targetname = s"Seq[${itemtype}]"
  def toCode(schema: VLschema = VLschema.empty, recursive: Boolean = true) =
    makeHelperClasses(Seq(defn.vltype), schema).getOrElse("")
}


class MapCoder(defn: VLmapOf) extends TypeCoder with ParentCoder {
  val itemtype = CodeGen.toCodeable(defn.vltype).targetname

  def typename = CodeGen.cleanClassName(defn.name)
  override def targetname = s"Map[String, ${itemtype}]"
  def toCode(schema: VLschema = VLschema.empty, recursive: Boolean = true) =
    makeHelperClasses(Seq(defn.vltype), schema).getOrElse("")
}


class EnumCoder(defn: VLenumDefn) extends TypeCoder {
  def typename = defn.name

  def toCode(vs: VLschema = VLschema.empty,
             recursive: Boolean = true): String = {
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

  def cleanName(orig: String): String = {
    val cleaner = orig.replaceAll("-", "_")
    CodeGen.mapReserved.getOrElse(cleaner, cleaner)
  }
}


class AnyOfCoder(defn: VLanyOf) extends TypeCoder with ParentCoder {
  def typename = CodeGen.cleanClassName(defn.name)

  def toCode(schema: VLschema = VLschema.empty,
             recursive: Boolean = true): String = {
    val options = defn.options.map { opt => {
        val cdbl = CodeGen.toCodeable(opt)

        // Generate implicit conversions from both our own AnyOf cases,
        // and the first generation of children that are themselves AnyOfs:
        ( s"  implicit def from${cdbl.typename}"
            + s"(_arg: ${cdbl.targetname}) =\n"
            + s"    new ${typename}(choice = _arg)" ) +:
        ( refsAnyOf(opt, schema) match {
            case Some(ref) => ref.options.map { child =>
                val childcdbl = CodeGen.toCodeable(child)
                s"  implicit   def from${childcdbl.targetname}" +
                  s"(_arg: ${childcdbl.targetname}) =\n" +
                  s"    new ${typename}" +
                  s"(choice = new ${cdbl.targetname}(_arg))" }
            case None => Nil } )
      }
    } . flatten . mkString("\n")

    Seq(if (recursive) makeHelperClasses(defn.options, schema) else None,
        Some(s"""|class ${typename}(val choice: Any) extends JsonExporter {
                 |  def toJValue = exportTerm(choice) }""" . stripMargin),
        Some(s"object ${typename} {"),
        Some(options),
        Some("}")) . flatten . mkString("", "\n", "\n\n")
  }

  /** Check if one of our options refers to someting that is itself an AnyOf */
  def refsAnyOf(opt: VLtypeDefn, schema: VLschema): Option[VLanyOf] =
    opt match {
      case ref: VLobjRef =>
        ref.target match {
          case child: VLanyOf => Some(child)
          case bare: VLbareType => {
            schema.names.get(bare.name) match {
              case Some(child: VLanyOf) => Some(child)
              case _ => None
            }
          }
          case _ => None
        }
      case _ => None
    }
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

  def toCode(schema: VLschema = VLschema.empty,
             recursive: Boolean = true): String = {
    val markers = makeMarkers()
    val propsetters = defn.properties.map { prop =>
      makePropMethod(prop) } .mkString("\n")

    Seq(makeHelperClasses(defn.properties.map { _.vltype },
                          schema, recursive=false),
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
    CodeGen.allMixins.flatMap { case (regexp, marker) =>
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

  def toCode(vs: VLschema = VLschema.empty, recursive: Boolean = true) = ""

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
    val warning = "/* AUTOGENERATED by VL4S - do not edit by hand */"
    val pw = new PrintWriter(stream)

    pw.println(warning)
    pw.print(makeHeader(schema))

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
          val codetext = codeable.toCode(schema, recursive=true)

          if (codetext.nonEmpty) {
            pw.print(s"\n${codetext}")
          }
        }
      }
    }

    pw.println("\n" + warning)
    pw.close()
  }

  def makeHeader(schema: VLschema): String =
    s"""|package uk.rwpenney.vl4s
        |
        |import org.json4s.{ JString, JValue }
        |import scala.language.implicitConversions
        |
        |object MetaData {
        |  val schemaVersion: String = "${schema.version}"
        |}
        |
        |""" . stripMargin
}


object CodeGen {
  /** Convert VegaLite type descriptor into source-code generator object */
  def toCodeable(vltype: VLtypeDefn): TypeCoder = {
    vltype match {
      case nl: VLnullType =>    new NullCoder(nl)
      case bare: VLbareType =>  new BareCoder(bare)
      case arr: VLarrayOf =>    new ArrayCoder(arr)
      case mp: VLmapOf =>       new MapCoder(mp)
      case enum: VLenumDefn =>  new EnumCoder(enum)
      case ao: VLanyOf =>       new AnyOfCoder(ao)
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
    "type" -> "vtype",
    "wait" -> "vwait"
  )

  /** Conversion between VL raw types and Scala native types */
  val mapBareTypes = Map(
    "boolean" ->  "Boolean",
    "null" ->     "Unit",
    "number" ->   "Double",
    "object" ->   "Map[String, Any]",
    "string" ->   "String"
  )

  /** Convert VegaLite typename into valid Scala identifier */
  def cleanClassName(orig: String): String = {
    val exclusions = Set(' ', ',', ';', ':')
    orig.filter { !exclusions.contains(_) } .
    map {
      case '<' | '>' | '[' | ']' => '_'
      case c =>   c
    }
  }

  /** Marker traits to apply to VegaLite operators based on classname regexps */
  val markerInterfaces = Map(
    raw"^TopLevel".r -> "Vl4sTopLevelSpec"
  )

  /** Mixins to apply to VegaLite operators based on classname regexps */
  val shorthandInterfaces = Map(
    raw"^Encoding[A-Za-z]*".r -> "EncodingShorthands"
  )

  val allMixins = markerInterfaces ++ shorthandInterfaces
}
