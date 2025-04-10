package co.blocke.edi4s
package model


trait Assertion // TODO

trait RefinedSegmentSpec:
  val name: String
  val canonicalName: String
  val description: String
  val required: Boolean
  val assertions: List[Assertion]
  val fields: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec]


trait RefinedLoopSpec extends RefinedSegmentSpec:
  val minRepeats: Option[Int]
  val maxRepeats: Option[Int]
  val body: List[RefinedSegmentSpec]


case class RefinedDocumentSpec(
                                name: String,
                                version: String,
                                partner: String,
                                segments: List[RefinedSegmentSpec]
                              )


case class RefinedSingleFieldSpec(
                             name: String,  // initially canonical name but may be renamed
                             canonicalName: String,  // name used in the canonical spec
                             description: String,
                             required: Boolean,
                             dataType: String,
                           )
case class RefinedCompositeFieldSpec( name: String, canonicalName: String, components: List[RefinedSingleFieldSpec] )


case class RefinedBasicSegmentSpec(
                             name: String,  // initially canonical name but may be renamed
                             canonicalName: String,  // name used in the canonical spec
                             description: String,
                             required: Boolean,
                             assertions: List[Assertion],
                             fields: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec]
                             ) extends RefinedSegmentSpec


case class RefinedBasicLoopSpec(
                               name: String,  // initially canonical name but may be renamed
                               canonicalName: String,  // name used in the canonical spec
                               description: String,
                               required: Boolean,
                               assertions: List[Assertion],
                               fields: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                               minRepeats: Option[Int],
                               maxRepeats: Option[Int],
                               body: List[RefinedSegmentSpec] // may be empty
                             ) extends RefinedLoopSpec


case class RefinedHLSpec(
                            name: String,  // initially canonical name but may be renamed
                            canonicalName: String,  // name used in the canonical spec
                            description: String,
                            required: Boolean,
                            assertions: List[Assertion],
                            fields: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                            minRepeats: Option[Int],
                            maxRepeats: Option[Int],
                            body: List[RefinedSegmentSpec], // may be empty
                            nested: List[RefinedHLSpec]
                          ) extends RefinedLoopSpec
