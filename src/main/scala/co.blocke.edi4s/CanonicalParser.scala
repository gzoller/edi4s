package co.blocke.edi4s

import model.*

import co.blocke.scalajack.*
import zio.*

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap
import pprint.*

object CanonicalParser:
  // Let compile-time macros deep-dive and generate JSON serilalizer for EdiObject and all subclasses...
  given sjEdiObject: ScalaJack[EdiObject] = ScalaJack.sjCodecOf[EdiObject]
  given sjRefinedSpec: ScalaJack[RefinedDocumentSpec] = ScalaJack.sjCodecOf[RefinedDocumentSpec]

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

  private def convertCompositeField(
                             name: String,
                             canonicalName: String,
                             index: Int,
                             prop: EdiRefProperty,
                             catalog: Map[String, EdiEnum | EdiSchema],
                           ): ZIO[Any, CanonicalError, RefinedCompositeFieldSpec] =
    val canonicalNameWithIndex = canonicalName+f"${index+1}%02d"
    for {
      segment <- prop.dereferenceSegment(catalog)
      fields <- convertComponentFields(segment, canonicalNameWithIndex, catalog)
    } yield RefinedCompositeFieldSpec(name, canonicalNameWithIndex, None, index+1, "", segment.required.exists(_.contains(name)), fields)

  // Convert simple fields for components of a composite field
  private def convertComponentFields(
                             segment: EdiSchema,
                             canonicalNameWithIndex: String,
                             catalog: Map[String, EdiEnum | EdiSchema],
                           ): ZIO[Any, CanonicalError, List[RefinedSingleFieldSpec]] =
    for {
      fields <- ZIO.foreach(segment.properties.toList.zipWithIndex) {
        case ((fname, sp: EdiElementProperty),i) =>
          val elementIdZio: ZIO[Any, CanonicalError, Option[Int]] =
            ZIO
              .attempt(sp.`x-openedi-element-id`.map(_.toInt))
              .mapError(e => CanonicalError(s"Invalid element ID '${sp.`x-openedi-element-id`.getOrElse("")}': ${e.getMessage}"))
          elementIdZio.flatMap { elementId =>
            ZIO.succeed(
              RefinedSingleFieldSpec(
                fname,
                canonicalNameWithIndex + f"${i + 1}%02d",
                None,
                i + 1,
                "",
                segment.required.exists(_.contains(fname)),
                sp.`type`,
                sp.format,
                elementId
              )
            )
          }
        case ((fname, _),i) =>
          ZIO.fail(CanonicalError(s"Unexpected property type $fname"))
      }
    } yield fields

  private def convertFields(
                             segment: EdiSchema,
                             catalog: Map[String, EdiEnum | EdiSchema],
                           ): ZIO[Any, CanonicalError, List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec]] =
    for {
      fields <- ZIO.foreach(segment.properties.toList.zipWithIndex) {
        case ((fname, sp: EdiElementProperty),i) =>
          val enumValuesRef: Option[String] =
            for {
              allOf    <- sp.allOf
              first    <- allOf.headOption
              ref  <- first.get("$ref")
            } yield ref

          val elementIdZio: ZIO[Any, CanonicalError, Option[Int]] =
            ZIO
              .attempt(sp.`x-openedi-element-id`.map(_.toInt))
              .mapError(e => CanonicalError(s"Invalid element ID '${sp.`x-openedi-element-id`.getOrElse("")}': ${e.getMessage}"))

          elementIdZio.flatMap{ elementId =>
            enumValuesRef match {
              case Some(ref) =>
                Canonical.extractRefKey(ref).map { decodedRef =>
                  RefinedSingleFieldSpec(
                    fname,
                    segment.getId + f"${i + 1}%02d",
                    None,
                    i+1,
                    "",
                    segment.required.exists(_.contains(fname)),
                    sp.`type`,
                    sp.format,
                    elementId,
                    sp.`enum`.getOrElse(Nil),
                    Some(decodedRef)
                  )
                }

              case None =>
                ZIO.succeed(
                  RefinedSingleFieldSpec(
                    fname,
                    segment.getId + f"${i + 1}%02d",
                    None,
                    i+1,
                    "",
                    segment.required.exists(_.contains(fname)),
                    sp.`type`,
                    sp.format,
                    elementId,
                    sp.`enum`.getOrElse(Nil),
                    None
                  )
                )
              }
          }

        case ((fname, sp: EdiRefProperty),i) =>
          convertCompositeField(fname, segment.getId, i, sp, catalog)

        case ((fname, p),_) =>
          ZIO.fail(CanonicalError(s"Unexpected property type $fname with property $p"))
      }
    } yield fields

  private def convertNestedLoopSegment(name: String, isRequired: Boolean, loopSchema: EdiSchema, catalog: Map[String, EdiEnum | EdiSchema]): ZIO[Any, CanonicalError, RefinedSegmentSpec | RefinedLoopSpec] =
    for {
      (loopFirstProp, loopBodyProp) <- loopSchema.properties.toList match
        case head :: tail => ZIO.succeed((head,tail))
        case _ => ZIO.fail(CanonicalError(s"Malformed list spec for $name"))
      loopSegmentSchema <- loopFirstProp match {
        case (_, e: EdiRefProperty) => e.dereferenceSegment(catalog)
        case _ => ZIO.fail(CanonicalError(s"Expected EdiRefProperty for first property of a loop with body ($name)"))
      }
      fields <- convertFields(loopSegmentSchema, catalog)
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
      //      _ <- ZIO.succeed(println(s"  === LOOP BODY END ($name) === "))
      loopName = loopSegmentSchema.getId
    } yield RefinedLoopSpec(
      loopName,
      loopName,
      None,
      "",
      isRequired,
      loopSegmentSchema.`x-openedi-syntax`.getOrElse(Nil),
      fields,
      Some(1),
      Some(1),
      bodySegments,
      None
    )

  private def convertSegmentProperty( name: String, isRequired: Boolean, prop: EdiRefProperty, catalog: Map[String, EdiEnum | EdiSchema]): ZIO[Any, CanonicalError, RefinedSegmentSpec | RefinedLoopSpec] =
    for {
      segment <- prop.dereferenceSegment(catalog)
      refinedSpec <- if segment.isLoop then convertNestedLoopSegment(name, isRequired, segment, catalog)
          else
            convertFields( segment, catalog ).map( fields =>
              RefinedSegmentSpec(name, name, None, "", isRequired, segment.`x-openedi-syntax`.getOrElse(Nil), fields)
            )
    } yield refinedSpec

  private def convertLoopProperty(
                                   name: String,
                                   isRequired: Boolean,
                                   prop: EdiItemsProperty,
                                   catalog: Map[String, EdiEnum | EdiSchema]
                                 ): ZIO[Any, CanonicalError, RefinedLoopSpec] =
    for {
//      _ <- ZIO.succeed(println("LOOP PROPERTY (2): "+name))
      loopSchema <- prop.dereferenceSegment(catalog) // not the loop segment--the schema! The loop segment is actually the first property of loopSchema
      (loopFirstProp, loopBodyProp) <- loopSchema.properties.toList match
        case head :: tail => ZIO.succeed((head,tail))
        case _ => ZIO.fail(CanonicalError(s"Malformed list spec for $name"))
      loopSegmentSchema <- loopFirstProp match {
        case (_, e: EdiRefProperty) => e.dereferenceSegment(catalog)
        case _ => ZIO.fail(CanonicalError(s"Expected EdiRefProperty for first property of a loop with body ($name)"))
      }
      fields <- convertFields(loopSegmentSchema, catalog)
//      _ <- ZIO.succeed(println(s"  === LOOP BODY ($name) === "))
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
//      _ <- ZIO.succeed(println(s"  === LOOP BODY END ($name) === "))
      loopName = loopSegmentSchema.getId
    } yield RefinedLoopSpec(
      loopName,
      loopName,
      None,
      "",
      isRequired,
      loopSegmentSchema.`x-openedi-syntax`.getOrElse(Nil),
      fields,
      prop.minItems,
      prop.maxItems,
      bodySegments,
      None
    )

  private def convertLoopPropertyNoBody(
                                         name: String,
                                         isRequired: Boolean,
                                         prop: EdiItemsProperty,
                                         catalog: Map[String, EdiEnum | EdiSchema]
                                       ): ZIO[Any, CanonicalError, RefinedLoopSpec] =
    for {
      loopSchema <- prop.dereferenceSegment(catalog)
      fields <- prop.dereferenceSegment(catalog).flatMap(loopSegment => convertFields(loopSchema, catalog))
    } yield RefinedLoopSpec(name, name, None, "", isRequired, loopSchema.`x-openedi-syntax`.getOrElse(Nil), fields, prop.minItems, prop.maxItems, Nil)

  private type RefinedUnion = RefinedSegmentSpec | RefinedLoopSpec
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
          case (name, single: EdiRefProperty) =>
            convertSegmentProperty(name, top.isRequired(name), single, catalog)
              .map(identity[RefinedUnion])
          case (name, loop: EdiItemsProperty) =>
            loop.loopHasBody(catalog).flatMap{ hasBody =>
              val action =
                if hasBody then
                  convertLoopProperty(name, top.isRequired(name), loop, catalog)
                else
                  convertLoopPropertyNoBody(name, top.isRequired(name), loop, catalog)
              action.map(identity[RefinedUnion])
            }
        }
        result <- ZIO.succeed(RefinedDocumentSpec(document, version, partner, props))
      } yield result
      case _ => ZIO.fail(CanonicalError(s"Can't find top level spec $topLevel in canonical spec, or it is the wrong type (EdiEnum)"))
    }
