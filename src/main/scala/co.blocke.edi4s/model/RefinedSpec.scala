package co.blocke.edi4s
package model

case class RefinedDocumentSpec(
                                name: String,
                                version: String,
                                partner: String,
                                segments: List[RefinedSegmentSpec | RefinedLoopSpec]
                              )


case class RefinedSingleFieldSpec(
                                   name: String,  // initially canonical name but may be renamed
                                   canonicalName: String,  // name used in the canonical spec
                                   index: Int,
                                   description: String,
                                   required: Boolean,
                                   dataType: String,
                                   format: Option[String],  // X12 formatting of the value, eg. date field
                                   elementId: Option[Int],
                                   validValues: List[String] = Nil,  // for a concise (often single) valid value
                                   validValuesRef: Option[String] = None  // ref into Canonical schema to avoid massive duplication
                                 )


case class RefinedCompositeFieldSpec(
                                      name: String,
                                      canonicalName: String,
                                      index: Int,
                                      description: String,
                                      required: Boolean,
                                      components: List[RefinedSingleFieldSpec] )


case class RefinedSegmentSpec(
                                    name: String,  // initially canonical name but may be renamed
                                    canonicalName: String,  // name used in the canonical spec
                                    description: String,
                                    required: Boolean,
                                    assertions: List[String],
                                    fields: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec]
                                  )


case class RefinedLoopSpec(
                                 name: String,  // initially canonical name but may be renamed
                                 canonicalName: String,  // name used in the canonical spec
                                 description: String,
                                 required: Boolean,
                                 assertions: List[String],
                                 fields: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                                 minRepeats: Option[Int],
                                 maxRepeats: Option[Int],
                                 body: List[RefinedSegmentSpec | RefinedLoopSpec], // may be empty
                                 nested: Option[List[RefinedLoopSpec]] = None  // if present this is an HL loop
                               )

