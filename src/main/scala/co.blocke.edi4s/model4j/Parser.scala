package co.blocke.edi4s.model4j

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap


object CanonicalParser:


//  def readSpec( spec: String ): ZIO[Any, CanonicalError, EdiObject] =
//    // ScalaJack  isn't natively ZIO-capable
//    ZIO.attempt(sjEdiObject.fromJson(spec)).mapError {
//      (t: Throwable) => CanonicalError(t.getMessage)
//    }


  private def convertCompositeField(
                                     name: String,
                                     canonicalName: String,
                                     index: Int,
                                     prop: EdiRefProperty,
                                     catalog: Map[String, EnumOrSchema],
                                   ): RefinedCompositeFieldSpec =
    val canonicalNameWithIndex = canonicalName+f"${index+1}%02d"
    val segment = prop.dereferenceSegment(catalog)
    val fields = convertComponentFields(segment, canonicalNameWithIndex, catalog)
    RefinedCompositeFieldSpec(name, canonicalNameWithIndex, None, index+1, "", segment.required.exists(_.contains(name)), fields)


  // Convert simple fields for components of a composite field
  @throws[CanonicalError]
  private def convertComponentFields(
                                      segment: EdiSchema,
                                      canonicalNameWithIndex: String,
                                      catalog: Map[String, EnumOrSchema],
                                    ): List[RefinedSingleFieldSpec] =
    segment.properties.toList.zipWithIndex.map {
      case ((fname, sp: EdiElementProperty), i) =>
        val elementId = sp.`x-openedi-element-id`.map(_.toInt)
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
      case ((fname, _), i) =>
        throw new CanonicalError(s"Unexpected property type $fname")
    }


  @throws[CanonicalError]
  private def convertFields(
                             segment: EdiSchema,
                             catalog: Map[String, EnumOrSchema],
                           ): List[RefinedFieldSpec] =
    segment.properties.toList.zipWithIndex.map {
      case ((fname, sp: EdiElementProperty), i) =>
        val enumValuesRef: Option[String] =
          for {
            allOf <- sp.allOf
            first <- allOf.headOption
            ref <- first.get("$ref")
          } yield ref
        val elementId = sp.`x-openedi-element-id`.map(_.toInt)
        enumValuesRef match {
          case Some(ref) =>
            RefinedSingleFieldSpec(
              fname,
              segment.getId + f"${i + 1}%02d",
              None,
              i + 1,
              "",
              segment.required.exists(_.contains(fname)),
              sp.`type`,
              sp.format,
              elementId,
              sp.`enum`.getOrElse(Nil),
              Some(Canonical.extractRefKey(ref))
            )

          case None =>
            RefinedSingleFieldSpec(
              fname,
              segment.getId + f"${i + 1}%02d",
              None,
              i + 1,
              "",
              segment.required.exists(_.contains(fname)),
              sp.`type`,
              sp.format,
              elementId,
              sp.`enum`.getOrElse(Nil),
              None
            )
        }

      case ((fname, sp: EdiRefProperty), i) =>
        convertCompositeField(fname, segment.getId, i, sp, catalog)

      case ((fname, p), _) =>
        throw new CanonicalError(s"Unexpected property type $fname with property $p")
    }


  @throws[CanonicalError]
  private def convertNestedLoopSegment(name: String, isRequired: Boolean, loopSchema: EdiSchema, catalog: Map[String, EnumOrSchema]): RefinedLoopSpec =
    val (loopFirstProp, loopBodyProp) = loopSchema.properties.toList match
      case head :: tail => (head,tail)
      case _ => throw new CanonicalError(s"Malformed list spec for $name")
    val loopSegmentSchema = loopFirstProp match
      case (_, e: EdiRefProperty) => e.dereferenceSegment(catalog)
      case _ => throw new CanonicalError(s"Expected EdiRefProperty for first property of a loop with body ($name)")
    val fields = convertFields(loopSegmentSchema, catalog)
    val bodySegments = loopBodyProp.map {
      case (name, prop: EdiRefProperty) => convertSegmentProperty(name, loopSegmentSchema.required.exists(_.contains(name)), prop, catalog)
      case (name, prop: EdiItemsProperty) =>
        if prop.loopHasBody(catalog) then
          convertLoopProperty(name, loopSegmentSchema.required.exists(_.contains(name)), prop, catalog)
        else
          convertLoopPropertyNoBody(name, loopSegmentSchema.required.exists(_.contains(name)), prop, catalog)
      case _ => throw new CanonicalError(s"Element property not allowed in a loop body ($name)")
    }
    val loopName = loopSegmentSchema.getId
    RefinedLoopSpec(
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


  @throws[CanonicalError]
  private def convertSegmentProperty(
                                      name: String,
                                      isRequired: Boolean,
                                      prop: EdiRefProperty,
                                      catalog: Map[String, EnumOrSchema]
                                    ): RefinedSingleOrLoopSegmentSpec =
    val segment = prop.dereferenceSegment(catalog)
    if segment.isLoop then
      convertNestedLoopSegment(name, isRequired, segment, catalog)
    else
      RefinedSegmentSpec(name, name, None, "", isRequired, segment.`x-openedi-syntax`.getOrElse(Nil), convertFields(segment, catalog))


  @throws[CanonicalError]
  private def convertLoopProperty(
                                   name: String,
                                   isRequired: Boolean,
                                   prop: EdiItemsProperty,
                                   catalog: Map[String, EnumOrSchema]
                                 ): RefinedLoopSpec =
    val loopSchema = prop.dereferenceSegment(catalog) // not the loop segment--the schema! The loop segment is actually the first property of loopSchema
    val (loopFirstProp, loopBodyProp) = loopSchema.properties.toList match
      case head :: tail => (head, tail)
      case _ => throw new CanonicalError(s"Malformed list spec for $name")
    val loopSegmentSchema = loopFirstProp match
      case (_, e: EdiRefProperty) => e.dereferenceSegment(catalog)
      case _ => throw new CanonicalError(s"Expected EdiRefProperty for first property of a loop with body ($name)")
    val fields = convertFields(loopSegmentSchema, catalog)
    val bodySegments = loopBodyProp.map {
      case (name, prop: EdiRefProperty) => convertSegmentProperty(name, loopSegmentSchema.required.exists(_.contains(name)), prop, catalog)
      case (name, prop: EdiItemsProperty) =>
        if prop.loopHasBody(catalog) then
          convertLoopProperty(name, loopSegmentSchema.required.exists(_.contains(name)), prop, catalog)
        else
          convertLoopPropertyNoBody(name, loopSegmentSchema.required.exists(_.contains(name)), prop, catalog)
      case _ => throw new CanonicalError(s"Element property not allowed in a loop body ($name)")
    }
    val loopName = loopSegmentSchema.getId
    RefinedLoopSpec(
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


  @throws[CanonicalError]
  private def convertLoopPropertyNoBody(
                                         name: String,
                                         isRequired: Boolean,
                                         prop: EdiItemsProperty,
                                         catalog: Map[String, EnumOrSchema]
                                       ): RefinedLoopSpec =
    val loopSchema = prop.dereferenceSegment(catalog)
    val ediSchema = prop.dereferenceSegment(catalog)
    val fields = convertFields(ediSchema, catalog)
    RefinedLoopSpec(name, name, None, "", isRequired, loopSchema.`x-openedi-syntax`.getOrElse(Nil), fields, prop.minItems, prop.maxItems, Nil)


  @throws[CanonicalError]
  def toRefined( edi: EdiObject, topLevel: String, document: String, version: String, partner: String ): RefinedDocumentSpec =
    val catalog = edi.components.schemas
    catalog.get(topLevel) match {
      case Some(top: EdiSchema) =>
        // get top-level properties, dereference them and filter those that are EdiSchema (segment) or EdiSegment (loop)
        val filtered: List[(String, RefOrItemsProperty)] = top.properties.collect{
          case (name, p: EdiRefProperty) => (name,p)
          case (name, p: EdiItemsProperty) => (name,p)
        }.toList

        val props = filtered.map {
          case (name, single: EdiRefProperty) =>
            convertSegmentProperty(name, top.isRequired(name), single, catalog)
          case (name, loop: EdiItemsProperty) =>
            if loop.loopHasBody(catalog) then
              convertLoopProperty(name, top.isRequired(name), loop, catalog)
            else
              convertLoopPropertyNoBody(name, top.isRequired(name), loop, catalog)
        }
        RefinedDocumentSpec(document, version, partner, props)
      case _ => throw new CanonicalError(s"Can't find top level spec $topLevel in canonical spec, or it is the wrong type (EdiEnum)")
    }

