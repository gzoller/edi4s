package co.blocke.edi4s
package model


enum Availability:
  case REQUIRED
  case OPTIONAL
  case MISSING


sealed trait Difference2:
  val name: String
  val canonicalName: String
  val availability: (Availability, Availability)


sealed trait SegmentDifference2 extends Difference2:
  val assertions: Option[(List[String], List[String])]
  val fieldDiff: List[FieldDifference2]

case class SingleSegmentDifference2(
                                    name: String,
                                    canonicalName: String,
                                    availability: (Availability, Availability),
                                    assertions: Option[(List[String],List[String])] = None,
                                    fieldDiff: List[FieldDifference2] = Nil
                                  ) extends SegmentDifference2:
  override def toString: String = canonicalName + availability + s" :: $assertions"


case class LoopSegmentDifference2(
                                     name: String,
                                     canonicalName: String,
                                     availability: (Availability, Availability),
                                     bodyDiff: List[SegmentDifference2] = Nil,
                                     nested: Option[LoopSegmentDifference2] = None,
                                     hlSpecRule: HLSpecRule = LevelsOk(),
                                     assertions: Option[(List[String],List[String])] = None,
                                     fieldDiff: List[FieldDifference2] = Nil,
                                     minDiff: Option[(Option[Int], Option[Int])] = None,
                                     maxDiff: Option[(Option[Int], Option[Int])] = None
                                   ) extends SegmentDifference2:
  override def toString: String =
    val header = s"$canonicalName$availability" + s" :: $assertions"

    val bodyIndented = bodyDiff
      .map(_.toString.linesIterator.map("   " + _).mkString("\n"))
      .mkString("\n")

    val nestedIndented = nested match
      case Some(n) =>
        "\n" + n.toString.linesIterator.map("   " + _).mkString("\n")
      case None => ""

    if bodyDiff.isEmpty && nested.isEmpty then header
    else
      val bodyPart = if bodyDiff.nonEmpty then s"\n$bodyIndented" else ""
      s"$header$bodyPart$nestedIndented"


trait FieldDifference2 extends Difference2

case class SingleFieldDifference2(
                             name: String,
                             canonicalName: String,
                             availability: (Availability, Availability)
                           ) extends FieldDifference2

case class CompositeFieldDifference2(
                                     name: String,
                                     canonicalName: String,
                                     availability: (Availability, Availability),
                                     fieldDiff: List[FieldDifference2]
                                   ) extends FieldDifference2