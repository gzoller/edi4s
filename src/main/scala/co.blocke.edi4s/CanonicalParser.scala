package co.blocke.edi4s

import model.*

import co.blocke.scalajack.*
import zio.*

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap

object CanonicalParser:
  // Let compile-time macros deep-dive and generate JSON serilalizer for EdiObject and all subclasses...
  given sjEdiObject: ScalaJack[EdiObject] = ScalaJack.sjCodecOf[EdiObject]

  def readSpec( spec: String ): ZIO[Any, CanonicalError, EdiObject] =
    // ScalaJack  isn't natively ZIO-capable
    ZIO.attempt(sjEdiObject.fromJson(spec)).mapError {
      (t: Throwable) => CanonicalError(t.getMessage)
    }

  def show(edi: EdiObject, top: String): String =
    val schema = edi.components.schemas
    val topEdi = schema(top)
    val tab = 1
    CanonicalShow.show(topEdi, tab, schema)

//  private def convertSingleField(
//                             segment: EdiSchema,
//                             fname: String,
//                             prop: EdiElementProperty,
//                             catalog: Map[String, EdiEnum | EdiSchema],
//                           ): ZIO[Any, CanonicalError, RefinedSingleFieldSpec] =
//    for {
//      fields <- ZIO.foreach(segment.properties.toList) {
//        case (fname, sp: EdiElementProperty) => ZIO.succeed({
//          println(s"    field $fname")
//          RefinedSingleFieldSpec(fname, fname, "", segment.required.exists(_.contains(fname)), sp.`type`)
//        })
//      case (fname, _) =>
//      ZIO.fail(CanonicalError(s"Expected simple element property for $fname but got something else."))
//    }
//} yield fields

  private def convertCompositeField(
                             name: String,
                             prop: EdiRefProperty,
                             catalog: Map[String, EdiEnum | EdiSchema],
                           ): ZIO[Any, CanonicalError, RefinedCompositeFieldSpec] =
    for {
      segment <- prop.dereferenceSegment(catalog)
      fields <- convertSingleFields(segment, catalog)
    } yield RefinedCompositeFieldSpec(name, name, fields)

  private def convertSingleFields(
                             segment: EdiSchema,
                             catalog: Map[String, EdiEnum | EdiSchema],
                           ): ZIO[Any, CanonicalError, List[RefinedSingleFieldSpec]] =
    for {
      fields <- ZIO.foreach(segment.properties.toList) {
        case (fname, sp: EdiElementProperty) => ZIO.succeed({
          println(s"    composite field $fname")
          RefinedSingleFieldSpec(fname, fname, "", segment.required.exists(_.contains(fname)), sp.`type`)
        })
        case (fname, _) =>
          ZIO.fail(CanonicalError(s"Unexpected property type $fname"))
      }
    } yield fields

  private def convertFields(
                             segment: EdiSchema,
                             catalog: Map[String, EdiEnum | EdiSchema],
                           ): ZIO[Any, CanonicalError, List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec]] =
    for {
      fields <- ZIO.foreach(segment.properties.toList) {
        case (fname, sp: EdiElementProperty) => ZIO.succeed({
          println(s"    field $fname")
          RefinedSingleFieldSpec(fname, fname, "", segment.required.exists(_.contains(fname)), sp.`type`)
        })
        case (fname, sp: EdiRefProperty) =>
          convertCompositeField(fname, sp, catalog)
        case (fname, _) =>
          ZIO.fail(CanonicalError(s"Unexpected property type $fname"))
      }
    } yield fields

  private def convertSegmentProperty( name: String, isRequired: Boolean, prop: EdiRefProperty, catalog: Map[String, EdiEnum | EdiSchema]): ZIO[Any, CanonicalError, RefinedSegmentSpec] =
    for {
      _ <- ZIO.succeed(println("SEGMENT PROPERTY: "+name))
      segment <- prop.dereferenceSegment(catalog)
      fields  <- convertFields( segment, catalog )
    } yield RefinedBasicSegmentSpec(name, name, "", isRequired, Nil, fields)

  private def convertLoopProperty(
                                   name: String,
                                   isRequired: Boolean,
                                   prop: EdiItemsProperty,
                                   catalog: Map[String, EdiEnum | EdiSchema]
                                 ): ZIO[Any, CanonicalError, RefinedLoopSpec] =
    for {
      _ <- ZIO.succeed(println("LOOP PROPERTY (2): "+name))
      loopSchema <- prop.dereferenceSegment(catalog) // not the loop segment--the schema! The loop segment is actually the first property of loopSchema
      (loopFirstProp, loopBodyProp) <- loopSchema.properties.toList match
        case head :: tail => ZIO.succeed((head,tail))
        case _ => ZIO.fail(CanonicalError(s"Malformed list spec for $name"))
      loopSegmentSchema <- loopFirstProp match {
        case (_, e: EdiRefProperty) => e.dereferenceSegment(catalog)
        case _ => ZIO.fail(CanonicalError(s"Expected EdiRefProperty for first property of a loop with body ($name)"))
      }
      fields <- convertFields(loopSegmentSchema, catalog)
      _ <- ZIO.succeed(println(s"  === LOOP BODY ($name) === "))
      bodySegments <- ZIO.foreach(loopBodyProp) {
        case (name, prop: EdiRefProperty) => convertSegmentProperty(name, loopSegmentSchema.required.exists(_.contains(name)), prop, catalog)
        case (name, prop: EdiItemsProperty) =>
          prop.loopHasBody(catalog).flatMap { hasBody =>
            if hasBody then
              convertLoopProperty(name, loopSegmentSchema.required.exists(_.contains(name)), prop, catalog)
            else
              convertLoopPropertyNoBody(name, loopSegmentSchema.required.exists(_.contains(name)), prop, catalog)
          }
        case _ => ZIO.fail(CanonicalError(s"Element property not allowed in a loop body ($name)"))
      }
      _ <- ZIO.succeed(println(s"  === LOOP BODY END ($name) === "))
    } yield RefinedBasicLoopSpec(name, name, "", isRequired, Nil, fields, prop.minItems, prop.maxItems, bodySegments)

  private def convertLoopPropertyNoBody(
                                         name: String,
                                         isRequired: Boolean,
                                         prop: EdiItemsProperty,
                                         catalog: Map[String, EdiEnum | EdiSchema]
                                       ): ZIO[Any, CanonicalError, RefinedLoopSpec] =
    for {
      loopSchema <- prop.dereferenceSegment(catalog)
      fields <- prop.dereferenceSegment(catalog).flatMap(loopSegment => convertFields(loopSchema, catalog))
    } yield RefinedBasicLoopSpec(name, name, "", isRequired, Nil, fields, prop.minItems, prop.maxItems, Nil)

  def toRefined( edi: EdiObject, topLevel: String, document: String, version: String, partner: String ): ZIO[Any, CanonicalError, RefinedDocumentSpec] =
    val catalog = edi.components.schemas
    catalog.get(topLevel) match {
      case Some(top: EdiSchema) =>
        // get top-level properties, dereference them and filter those that are EdiSchema (segment) or EdiSegment (loop)
        val filtered: List[(String, EdiRefProperty | EdiItemsProperty)] = top.properties.collect{
          case (name, p: EdiRefProperty) => (name,p)
          case (name, p: EdiItemsProperty) => (name,p)
        }.toList
        for {
          props <- ZIO.foreach( filtered ) {
            case (name, single: EdiRefProperty) => convertSegmentProperty(name, top.isRequired(name), single, catalog)
            case (name, loop: EdiItemsProperty) =>
              loop.loopHasBody(catalog).flatMap{ hasBody =>
                if hasBody then
                  convertLoopProperty(name, top.isRequired(name), loop, catalog)
                else
                  convertLoopPropertyNoBody(name, top.isRequired(name), loop, catalog)
              }
          }
          _ <- ZIO.succeed(println(props.mkString("\n")))
          result <- ZIO.succeed(RefinedDocumentSpec(document, version, partner, Nil))
        } yield result
      case _ => ZIO.fail(CanonicalError(s"Can't find top level spec $topLevel in canonical spec, or it is the wrong type (EdiEnum)"))
    }
