package co.blocke.edi4s
package model

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap
import zio.*


trait Showable


trait Property:
  def dereference(schemas: Map[String, EdiEnum | EdiSchema]): ZIO[Any, CanonicalError, EdiEnum | EdiSchema]
  def dereferenceSegment(schemas: Map[String, EdiEnum | EdiSchema]): ZIO[Any, CanonicalError, EdiSchema] =
    dereference(schemas).flatMap {
      case _: EdiEnum => ZIO.fail(CanonicalError("Expecting EdiSchema but found EdiEnum"))
      case x: EdiSchema => ZIO.succeed(x)
    }


case class EdiInfo(title: String, version: String)


case class EdiEnum(`enum`: List[String], `type`: String, format: String) extends Showable


case class EdiSchema(
                      required: Option[List[String]],
                      `type`: String,
                      properties: mutable.LinkedHashMap[String, EdiRefProperty|EdiItemsProperty|EdiElementProperty],
                      `x-openedi-segment-id`: Option[String],
                      `x-openedi-composite-id`: Option[String],
                      `x-openedi-loop-id`: Option[String],
                      `x-openedi-syntax`: Option[List[String]]  // encoded assertions
                    ) extends Showable:
  def isRequired(p: String): Boolean = required.exists(_.contains(p))
  def isComposite: Boolean = `x-openedi-composite-id`.isDefined
  def isLoop: Boolean = `x-openedi-loop-id`.isDefined
  def getId: String =
    `x-openedi-segment-id`
      .orElse(`x-openedi-composite-id`)
      .orElse(`x-openedi-loop-id`)
      .getOrElse("unknown")


// Segment property with a reference the details in schemas (segment catalog)
case class EdiRefProperty(`$ref`: String) extends Showable with Property:
  def dereference(schemas: Map[String, EdiEnum | EdiSchema]): ZIO[Any, CanonicalError, EdiEnum | EdiSchema] =
    Canonical.extractRefKey(`$ref`).flatMap{ key =>
      schemas.get(key) match {
        case Some(v) => ZIO.succeed(v)
        case None => ZIO.fail(CanonicalError(s"Reference for ${`$ref`} not found in canonical schema"))
      }
    }

// Defines loops/arrays
case class EdiItemsProperty(
                             `type`: String,
                             minItems: Option[Int],
                             maxItems: Option[Int],
                             items: EdiRefProperty
                           ) extends Showable with Property:
  def dereference(schemas: Map[String, EdiEnum | EdiSchema]): ZIO[Any, CanonicalError, EdiEnum | EdiSchema] =
    items.dereference(schemas)
  def loopHasBody(schemas: Map[String, EdiEnum | EdiSchema]): ZIO[Any, CanonicalError, Boolean] =
    dereference(schemas).flatMap {
      case e: EdiSchema =>
        e.properties.headOption match {
          case Some((_, _: EdiRefProperty)) => ZIO.succeed(true)
          case _ => ZIO.succeed(false)
        }
      case _ => ZIO.succeed(false)
    }


// Defines fields within a segment: ST01, ST02, ...
case class EdiElementProperty(
                               `type`: String,
                               minLength: Option[Int],
                               maxLength: Option[Int],
                               format: Option[String],
                               `enum`: Option[List[String]],
                               allOf: Option[List[Map[String,String]]],
                               anyOf: Option[List[Map[String,String]]],
                               oneOf: Option[List[Map[String,String]]],
                               not: Option[List[Map[String,String]]],
                               `x-openedi-element-id`: Option[String]
                             ) extends Showable with Property:
  def dereference(schemas: Map[String, EdiEnum | EdiSchema]): ZIO[Any, CanonicalError, EdiEnum | EdiSchema] =
    ZIO.succeed(EdiSchema(None,`type`,mutable.LinkedHashMap.empty[String, EdiRefProperty|EdiItemsProperty|EdiElementProperty],`x-openedi-element-id`,None,None,None))


case class EdiComponents(schemas: Map[String, EdiEnum | EdiSchema])


case class EdiObject(openapi: String, info: EdiInfo, components: EdiComponents)

object Canonical:
  def extractRefKey(ref: String): ZIO[Any,CanonicalError,String] =
    val extractKey = ".*/([^/]+)$".r
    ref match {
      case extractKey(key) => ZIO.succeed(key)
      case _ => ZIO.fail(CanonicalError(s"Unable to extract a reference key from $ref"))
    }

/* BROKEN:
case class EdiElementProperty(
                               `type`: String,
                               minLength: Option[Int],
                               maxLength: Option[Int],
                               format: Option[String],
                               `enum`: Option[List[String]],
                               allOf: Option[List[Constraint]],  // https://json-schema.org/understanding-json-schema/reference/combining#allof
                               anyOf: Option[List[Constraint]],
                               oneOf: Option[List[Constraint]],
                               not: Option[List[Constraint]],
                               `x-openedi-element-id`: Option[String]
                             ) extends Showable with Property:

*/