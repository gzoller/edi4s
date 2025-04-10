package co.blocke.edi4s.model

case class DependsOnSpec(
  field: String,
  condition: String,
  value: Option[String] = None,
  minValue: Option[String] = None,
  maxValue: Option[String] = None
)

case class SegmentFieldSpec(
 name: String,
 presence: String,
 dependsOn: Option[DependsOnSpec] = None
)

case class SegmentSpec(
  name: String,
  presence: String,
  fields: List[SegmentFieldSpec],
  loopBody: Option[SegmentFieldSpec] = None,
  minRepetitions: Option[Int] = None,
  maxRepetitions: Option[Int] = None
)

case class DocumentSpec(
  documentName: String,
  version: String,
  segments: List[SegmentSpec]
)
