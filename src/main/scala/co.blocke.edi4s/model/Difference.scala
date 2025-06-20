package co.blocke.edi4s
package model


enum Availability:
  case REQUIRED
  case OPTIONAL
  case MISSING


sealed trait Difference:
  val name: String
  val canonicalName: String
  val availability: (Availability, Availability)


sealed trait SegmentDifference extends Difference:
  val assertions: Option[(List[String], List[String])]
  val fieldDiff: List[FieldDifference]


case class SingleSegmentDifference(
                                    name: String,
                                    canonicalName: String,
                                    availability: (Availability, Availability),
                                    assertions: Option[(List[String],List[String])] = None,
                                    fieldDiff: List[FieldDifference] = Nil,
                                  ) extends SegmentDifference:
  override def toString: String = canonicalName + availability + s" :: $assertions"


case class LoopSegmentDifference(
                                  name: String,
                                  canonicalName: String,
                                  availability: (Availability, Availability),
                                  bodyDiff: List[SegmentDifference] = Nil,
                                  nested: Option[LoopSegmentDifference] = None,
                                  hlSpecRule: HLSpecRule = LevelsOk(),
                                  hlDiscriminator: Option[String] = None,
                                  assertions: Option[(List[String],List[String])] = None,
                                  fieldDiff: List[FieldDifference] = Nil,
                                  minDiff: Option[(Option[Int], Option[Int])] = None,
                                  maxDiff: Option[(Option[Int], Option[Int])] = None
                                   ) extends SegmentDifference:
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


trait FieldDifference extends Difference

case class SingleFieldDifference(
                             name: String,
                             canonicalName: String,
                             availability: (Availability, Availability),
                             dataType: Option[(String,String)] = None,
                             format: Option[(Option[String], Option[String])] = None,
                             elementId: Option[(Option[Int], Option[Int])] = None,
                             validValues: Option[(List[String],List[String])] = None,
                             validValuesRef: Option[(Option[String], Option[String])] = None
                           ) extends FieldDifference

case class CompositeFieldDifference(
                                     name: String,
                                     canonicalName: String,
                                     availability: (Availability, Availability),
                                     fieldDiff: List[FieldDifference]
                                   ) extends FieldDifference
