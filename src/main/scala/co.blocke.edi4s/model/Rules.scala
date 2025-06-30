package co.blocke.edi4s
package model

import Availability.*


//-------------------------------->> Field Assignments

sealed trait FieldAssignment:
  val targetField: String
  val availability: (Availability, Availability)
  val orElse: Option[FieldAssignment] // given if src availability is (OPTIONAL,REQUIRED)

case class DirectAssignment(
                             targetField: String,
                             availability: (Availability,Availability), // src/target
                             orElse: Option[FieldAssignment] = None
                           ) extends FieldAssignment

case class ConstantAssignment(
                             targetField: String,
                             value: String,
                             availability: (Availability,Availability), // src/target
                             orElse: Option[FieldAssignment] = None
                           ) extends FieldAssignment

case class PlaceholderAssignment(
                               targetField: String,
                               availability: (Availability, Availability), // src/target
                               dummyValue: String = "???"
                             ) extends FieldAssignment:
  val orElse: Option[FieldAssignment] = None

case class ProfileAssignment(
                              targetField: String,
                              profileId: String,
                              path: String,
                              documentId: Option[String],  // None if applies to all documents
                              availability: (Availability,Availability), // src/target
                              orElse: Option[FieldAssignment] = None
                            ) extends FieldAssignment

case class MatchFieldAssignment(
                               targetField: String, // usually a REF01-like field
                               cases: Map[String, List[FieldAssignment]],
                               availability: (Availability,Availability), // src/target
                               orElse: Option[FieldAssignment] = None
                             ) extends FieldAssignment

// TODO: ComputedAssignment
// TODO: DirectAssignment with converter (eg unit conversion)


//-------------------------------->> Segment Assignments

case class MappingSpec(
                        rules: List[SegmentAssignment]
                      ):
  override def toString: String =
    rules.map(printAssignment(_, 0)).mkString("\n")

  private def printAssignment(assign: SegmentAssignment, indent: Int): String =
    val pad = "  " * indent
    val immaculate = if assign.availability._1 == MISSING then " (immaculate)" else ""
    assign match
      case s: SingleSegmentAssignment =>
        s"$pad- SingleSegmentAssignment(${s.canonicalName})$immaculate"

      case n: NoOpSegmentAssignment =>
        s"$pad- NoOpSegmentAssignment(${n.canonicalName})$immaculate"

      case l: LoopSegmentAssignment =>
        val head = s"$pad- LoopSegmentAssignment(${l.canonicalName})$immaculate"
        val bodyStr =
          if l.body.nonEmpty then
            l.body.map(sa => printAssignment(sa, indent + 1)).mkString("\n")
          else ""
        val nestedStr =
          l.nested.map { n =>
            val nestedHeader = s"${pad}  >> Nested:"
            val nestedBody = printAssignment(n, indent + 2)
            s"$nestedHeader\n$nestedBody"
          }.getOrElse("")
        List(head, bodyStr, nestedStr).filter(_.nonEmpty).mkString("\n")


sealed trait SegmentAssignment:
  val canonicalName: String
  val fieldAssignments: List[FieldAssignment]
  val availability: (Availability, Availability)

case class SingleSegmentAssignment(
                                    canonicalName: String,
                                    fieldAssignments: List[FieldAssignment],
                                    availability: (Availability,Availability),
                                    orElseAssignments: Option[SingleSegmentAssignment] // if avail is (OPTIONAL,REQUIRED) and src is None
                                  ) extends SegmentAssignment


case class LoopSegmentAssignment(
                                  canonicalName: String,
                                  fieldAssignments: List[FieldAssignment],
                                  body: List[SegmentAssignment], // used for loops
                                  // TODO: When we handle missing nest levels we'll need a top-level rule to flatten data
                                  //hlRule: HLSpecRule,
                                  // The machinery is all there--just not wired up.
                                  nested: Option[LoopSegmentAssignment],  // for nested HL loops
                                  availability: (Availability,Availability),
                                  orElseAssignments: Option[LoopSegmentAssignment] // if avail is (OPTIONAL,REQUIRED) and src is None
                                ) extends SegmentAssignment


case class NoOpSegmentAssignment(
                                  canonicalName: String,
                                  availability: (Availability, Availability)
                                ) extends SegmentAssignment: // used to help with data/rule alignment during mapping
  val fieldAssignments: List[FieldAssignment] = Nil

//-------- For enumerated fields -- link 2 fields together for assignment

case class EnumeratedDependency( field: String, companionField: String)

//-------- HL spec rule  (may deprecate?)
sealed trait HLSpecRule
case class PromoteTargetLevel( promoteDesc: String, toDesc: String, toHL03: String ) extends HLSpecRule
case class FlattenSrcLevel( flatten: List[(String,String)] ) extends HLSpecRule   // List[(Description,HL03_from_src)]
case class LevelsOk() extends HLSpecRule
