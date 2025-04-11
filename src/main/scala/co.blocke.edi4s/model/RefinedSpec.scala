package co.blocke.edi4s
package model

class Assertion(val i: Int) // TODO

case class RefinedDocumentSpec(
                                name: String,
                                version: String,
                                partner: String,
                                segments: List[RefinedSegmentSpec | RefinedLoopSpec]
                              )


case class RefinedSingleFieldSpec(
                                   name: String,  // initially canonical name but may be renamed
                                   canonicalName: String,  // name used in the canonical spec
                                   description: String,
                                   required: Boolean,
                                   dataType: String,
                                 )
case class RefinedCompositeFieldSpec( name: String, canonicalName: String, components: List[RefinedSingleFieldSpec] )


case class RefinedSegmentSpec(
                                    name: String,  // initially canonical name but may be renamed
                                    canonicalName: String,  // name used in the canonical spec
                                    description: String,
                                    required: Boolean,
                                    assertions: List[Assertion],
                                    fields: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec]
                                  )


case class RefinedLoopSpec(
                                 name: String,  // initially canonical name but may be renamed
                                 canonicalName: String,  // name used in the canonical spec
                                 description: String,
                                 required: Boolean,
                                 assertions: List[Assertion],
                                 fields: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                                 minRepeats: Option[Int],
                                 maxRepeats: Option[Int],
                                 body: List[RefinedSegmentSpec | RefinedLoopSpec], // may be empty
                                 nested: Option[List[RefinedLoopSpec]] = None  // if present this is an HL loop
                               )

