package co.blocke.edi4s
package model4j

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap


class CanonicalError(msg: String) extends Exception(msg)


trait EnumOrSchema


trait Property:
  @throws[CanonicalError]
  def dereference(schemas: Map[String, EnumOrSchema]): EnumOrSchema

  @throws[CanonicalError]
  def dereferenceSegment(schemas: Map[String, EnumOrSchema]): EdiSchema =
    dereference(schemas) match {
      case _: EdiEnum => throw new CanonicalError("Expecting EdiSchema but found EdiEnum")
      case x: EdiSchema => x
    }


case class EdiInfo(title: String, version: String)


case class EdiEnum(`enum`: List[String], `type`: String, format: String) extends EnumOrSchema


case class EdiSchema(
                      required: Option[List[String]],
                      `type`: String,
                      properties: mutable.LinkedHashMap[String, Property],
                      `x-openedi-segment-id`: Option[String],
                      `x-openedi-composite-id`: Option[String],
                      `x-openedi-loop-id`: Option[String],
                      `x-openedi-syntax`: Option[List[String]]  // encoded assertions
                    ) extends EnumOrSchema:
  def isRequired(p: String): Boolean = required.exists(_.contains(p))
  def isComposite: Boolean = `x-openedi-composite-id`.isDefined
  def isLoop: Boolean = `x-openedi-loop-id`.isDefined
  def getId: String =
    `x-openedi-segment-id`
      .orElse(`x-openedi-composite-id`)
      .orElse(`x-openedi-loop-id`)
      .getOrElse("unknown")


trait RefOrItemsProperty


// Segment property with a reference the details in schemas (segment catalog)
case class EdiRefProperty(`$ref`: String) extends Property with RefOrItemsProperty:
  @throws[CanonicalError]
  def dereference(schemas: Map[String, EnumOrSchema]): EnumOrSchema =
    val key = Canonical.extractRefKey(`$ref`)
    schemas.get(key) match {
      case Some(v) => v
      case None => throw new CanonicalError(s"Reference for ${`$ref`} not found in canonical schema")
    }


// Defines loops/arrays
case class EdiItemsProperty(
                             `type`: String,
                             minItems: Option[Int],
                             maxItems: Option[Int],
                             items: EdiRefProperty
                           ) extends Property with RefOrItemsProperty:
  @throws[CanonicalError]
  def dereference(schemas: Map[String, EnumOrSchema]): EnumOrSchema =
    items.dereference(schemas)

  def loopHasBody(schemas: Map[String, EnumOrSchema]): Boolean =
    dereference(schemas) match {
      case e: EdiSchema =>
        e.properties.headOption match {
          case Some((_, _: EdiRefProperty)) => true
          case _ => false
        }
      case _ => false
    }


// Defines fields within a segment: ST01, ST02, ...
case class EdiElementProperty(
                               `type`: String,
                               minLength: Option[Int],
                               maxLength: Option[Int],
                               format: Option[String],
                               `enum`: Option[List[String]],
                               allOf: Option[List[Map[String,String]]], // https://json-schema.org/understanding-json-schema/reference/combining#allof
                               anyOf: Option[List[Map[String,String]]],
                               oneOf: Option[List[Map[String,String]]],
                               not: Option[List[Map[String,String]]],
                               `x-openedi-element-id`: Option[String]
                             ) extends Property:
  @throws[CanonicalError]
  def dereference(schemas: Map[String, EnumOrSchema]): EnumOrSchema =
    EdiSchema(None,`type`,mutable.LinkedHashMap.empty[String, Property],`x-openedi-element-id`,None,None,None)


case class EdiComponents(schemas: Map[String, EnumOrSchema])


case class EdiObject(openapi: String, info: EdiInfo, components: EdiComponents)


object Canonical:
  @throws[CanonicalError]
  def extractRefKey(ref: String): String =
    val extractKey = ".*/([^/]+)$".r
    ref match {
      case extractKey(key) => key
      case _ => throw new CanonicalError(s"Unable to extract a reference key from $ref")
    }
