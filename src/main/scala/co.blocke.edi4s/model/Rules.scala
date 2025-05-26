package co.blocke.edi4s
package model


case class MappingSpec(
                        rules: List[SegmentAssignment]
                      )

sealed trait SegmentAssignment:
  def canonicalName: String


case class FieldsSegmentAssignment(
                                    canonicalName: String,
                                    fieldAssignments: List[FieldAssignment]
                                  ) extends SegmentAssignment


case class LoopSegmentAssignment(
                                    canonicalName: String,
                                    fieldAssignments: List[FieldAssignment],
                                    body: List[SegmentAssignment], // used for loops
                                    nested: Option[LoopSegmentAssignment]  // for nested HL loops
                                  ) extends SegmentAssignment

// For src-optional, target-required segments
case class OrElseFieldsSegmentAssignment(
                                    canonicalName: String,
                                    someAssignment: SegmentAssignment,
                                    noneAssignment: SegmentAssignment
                                  ) extends SegmentAssignment

//---------------------

enum ValueKind {
  case Direct, Constant, Context
}

sealed trait FieldAssignment:
  val isPlaceholder: Boolean

case class GeneralFieldAssignment(
                                  targetField: String,
                                  value: String,
                                  valueKind: ValueKind,
                                  isPlaceholder: Boolean = false
                                ) extends FieldAssignment

// For src-optional, target-required fields
case class OrElseFieldAssignment(
                                  targetField: String,
                                  orElseValue: String,
                                  orElseValueKind: ValueKind,
                                  isPlaceholder: Boolean = true
                                ) extends FieldAssignment

case class EnumMatchFieldAssignment(
                                  matchField: String, // usually a REF01-like field
                                  cases: Map[String, List[FieldAssignment]],
                                  isPlaceholder: Boolean = false
                                ) extends FieldAssignment

//-------- For enumerated fields

case class EnumeratedDependency( field: String, companionField: String)

//-------- HL spec rule

sealed trait HLSpecRule

case class PromoteTargetLevel( promoteDesc: String, toDesc: String, toHL03: String ) extends HLSpecRule
case class FlattenSrcLevel( flatten: List[(String,String)] ) extends HLSpecRule   // List[(Description,HL03_from_src)]
case class LevelsOk() extends HLSpecRule

/* ======= HL Alignment
Case 0: All levels align--no action needed

Case 1: Merged levels on target --> produce PromoteTargetLevel("C")
Src:       Target:
A          A
B          B
C          D/C
D

Case 2: Missing levels on target --> produce FlattenSrcLevel(List("B","C"))
Src:       Target:
A          A
B          D
C
D

Otherwise (for now): Toss error--incompatible structure
*/