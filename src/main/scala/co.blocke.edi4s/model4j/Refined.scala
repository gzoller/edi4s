package co.blocke.edi4s
package model4j

case class RefinedDocumentSpec(
                                name: String,
                                version: String,
                                partner: String,
                                segments: List[RefinedSingleOrLoopSegmentSpec]
                              )


trait RefinedFieldSpec:
  val name: String
  val canonicalName: String
  val humanName: Option[String]
  val index: Int
  val description: String
  val required: Boolean


case class RefinedSingleFieldSpec(
                                   name: String,  // initially canonical name but may be renamed
                                   canonicalName: String,  // name used in the canonical spec
                                   humanName: Option[String],
                                   index: Int,
                                   description: String,
                                   required: Boolean,
                                   dataType: String,
                                   format: Option[String],  // X12 formatting of the value, eg. date field
                                   elementId: Option[Int],
                                   validValues: List[String] = Nil,  // for a concise (often single) valid value
                                   validValuesRef: Option[String] = None  // ref into Canonical schema to avoid massive duplication
                                 ) extends RefinedFieldSpec


case class RefinedCompositeFieldSpec(
                                      name: String,
                                      canonicalName: String,
                                      humanName: Option[String],
                                      index: Int,
                                      description: String,
                                      required: Boolean,
                                      components: List[RefinedSingleFieldSpec]
                                    ) extends RefinedFieldSpec


trait RefinedSingleOrLoopSegmentSpec:
  val name: String
  val canonicalName: String
  val humanName: Option[String]
  val description: String
  val required: Boolean
  val assertions: List[String]
  val fields: List[RefinedFieldSpec]


case class RefinedSegmentSpec(
                               name: String,  // initially canonical name but may be renamed
                               canonicalName: String,  // name used in the canonical spec
                               humanName: Option[String],
                               description: String,
                               required: Boolean,
                               assertions: List[String],
                               fields: List[RefinedFieldSpec]
                             ) extends RefinedSingleOrLoopSegmentSpec


case class RefinedLoopSpec(
                            name: String,  // initially canonical name but may be renamed
                            canonicalName: String,  // name used in the canonical spec
                            humanName: Option[String],
                            description: String,
                            required: Boolean,
                            assertions: List[String],
                            fields: List[RefinedFieldSpec],
                            minRepeats: Option[Int],
                            maxRepeats: Option[Int],
                            body: List[RefinedSingleOrLoopSegmentSpec], // may be empty
                            nested: Option[RefinedLoopSpec] = None  // if present this is an HL loop
                          ) extends RefinedSingleOrLoopSegmentSpec:
  // EDI has "loops" that are really segments (EdiSchema) with type "object" (not array) and the `x-openedi-loop-id` flag set.
  // They are *not* EdiItemsProperty! What this means is they are a block of segments that look like a loop that does't repeat,
  // much like a struct in C.  We indicate this here by setting both minRepeats and maxRepeats to Some(1).
  def isStruct: Boolean = minRepeats.contains(1) && maxRepeats.contains(1)
  def isRepeatable: Boolean = maxRepeats.exists(_ > 1)

