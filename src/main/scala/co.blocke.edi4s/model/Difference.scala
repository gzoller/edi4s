package co.blocke.edi4s
package model

import table.*
import pprint.*


sealed trait Difference:
  val path: Path
  val name: String
  val canonicalName: String
  val presence: (Boolean,Boolean)
  val required: (Boolean,Boolean)
  def isOk: Boolean = presence._1 == presence._2 && (required._1 == required._2 || required._1)
  def render(nestLevel: Int = 0): List[BodyRow]


sealed trait FieldDifference extends Difference


// TODO: Do something more intelligent with missing+required (display and highlighting) to show importance
sealed trait SegmentDifference extends Difference:
  val assertions: Option[(List[String], List[String])]
  val fieldDiff: List[FieldDifference]
  def render(nestLevel: Int): List[BodyRow] =
    val rationalName = if name == canonicalName then name else s"$canonicalName ($name)"
    val (r1,r2) = required match {
      case (true,false) => ("required","optional")
      case (false,true) => ("optional","required")
      case _ => ("","")
    }
    val presenceRow = presence match
      case (true,true) =>
        Row(
          List(
            Cell(rationalName, indent = nestLevel, style = Some(Style.SECONDARY)),
            Cell(r1),
            Cell(rationalName, indent = nestLevel, style = Some(Style.SECONDARY)),
            Cell(r2)
          )
        )
      case (true,false) =>
        Row(
          List(
            Cell(rationalName, indent = nestLevel, style = Some(Style.SECONDARY)),
            Cell(r1),
            Cell(rationalName, indent = nestLevel, style = Some(Style.MUTED)),
            Cell("(missing)")
          )
        )
      case (false,true) =>
        Row(
          List(
            Cell(rationalName, indent = nestLevel, style = Some(Style.MUTED)),
            Cell("(missing)", style = Some(Style.WARN)),
            Cell(rationalName, indent = nestLevel, style = Some(Style.SECONDARY)),
            Cell(r2)
          )
        )
      case _ =>
        Row(
          List(
            Cell(rationalName, indent = nestLevel, style = Some(Style.MUTED)),
            Cell("(missing)", style = Some(Style.MUTED)),
            Cell(rationalName, indent = nestLevel, style = Some(Style.MUTED)),
            Cell("(missing)", style = Some(Style.MUTED))
          )
        )
    val fieldRows = fieldDiff.flatMap(_.render(nestLevel+1))
    List(presenceRow) ++ fieldRows


case class SingleFieldDifference(
                                  path: Path,
                                  name: String,
                                  canonicalName: String,
                                  presence: (Boolean,Boolean),
                                  required: (Boolean,Boolean),
                                  dataType: Option[(String,String)] = None,
                                  format: Option[(Option[String], Option[String])] = None,
                                  elementId: Option[(Option[Int], Option[Int])] = None,
                                  validValues: Option[(List[String],List[String])] = None,
                                  validValuesRef: Option[(Option[String], Option[String])] = None
                                ) extends FieldDifference:
  def render(nestLevel: Int): List[BodyRow] =
    val rationalName = if name == canonicalName then name else s"$canonicalName ($name)"
    val presenceRow = presence match
      case (true, true) =>
        Row(
          List(
            Cell(rationalName, indent = nestLevel, style = Some(Style.NEUTRAL)),
            Cell("OK", style=Some(Style.TERTIARY)),
            Cell(rationalName, indent = nestLevel, style = Some(Style.NEUTRAL)),
            Cell("OK", style=Some(Style.TERTIARY))
          )
        )
      case (true, false) =>
        Row(
          List(
            Cell(rationalName, indent = nestLevel, style = Some(Style.NEUTRAL)),
            Cell(""),
            Cell(rationalName, indent = nestLevel, style = Some(Style.NEUTRAL)),
            Cell("(missing)")
          )
        )
      case (false, true) =>
        Row(
          List(
            Cell(rationalName, indent = nestLevel, style = Some(Style.MUTED)),
            Cell("(missing)", style = Some(Style.WARN)),
            Cell(rationalName, indent = nestLevel, style = Some(Style.NEUTRAL)),
            Cell("")
          )
        )
      case _ =>
        Row(
          List(
            Cell(rationalName, indent = nestLevel, style = Some(Style.MUTED)),
            Cell("(missing)", style = Some(Style.MUTED)),
            Cell(rationalName, indent = nestLevel, style = Some(Style.MUTED)),
            Cell("(missing)", style = Some(Style.MUTED))
          )
        )
    val dataTypeRow = dataType.map{ case (a,b) =>
      Row(
        List(
          Cell("--"+"Data Type:", indent = nestLevel+1, style = Some(Style.TERTIARY)),
          Cell(a, style = Some(Style.NEUTRAL)),
          Cell("--"+"Data Type:", indent = nestLevel+1, style = Some(Style.TERTIARY)),
          Cell(b, style = Some(Style.NEUTRAL))
        )
      )
    }
    val formatRow = format.map{ case (a,b) =>
      val matchStyle = (a, b) match
        case (None, Some(_)) => Some(Style.WARN) // trouble if only given in target--src must be careful
        case _ => Some(Style.NEUTRAL)
      Row(
        List(
          Cell("--"+"Format:", indent = nestLevel+1, style = Some(Style.TERTIARY)),
          Cell(a.getOrElse("(not given)"), style = matchStyle),
          Cell("--"+"Format:", indent = nestLevel+1, style = Some(Style.TERTIARY)),
          Cell(b.getOrElse("(not given)"), style = matchStyle)
        )
      )
    }
    val elementIdRow = elementId.map { case (a, b) =>
      val matchStyle = (a, b) match
        case (None, Some(_)) => Some(Style.WARN) // trouble if only given in target--src must be careful
        case _ => Some(Style.NEUTRAL)
      Row(
        List(
          Cell("--"+"Element ID:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(a.map(_.toString).getOrElse("(not given)"), style = matchStyle),
          Cell("--"+"Element ID:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(b.map(_.toString).getOrElse("(not given)"), style = matchStyle)
        )
      )
    }
    val validValuesRow = validValues.map{ case(a,b) =>
      Row(
        List(
          Cell("--"+"Valid Values:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(a.mkString(","), style = Some(Style.NEUTRAL)),
          Cell("--"+"Valid Values:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(b.mkString(","), style = {
            if a.toSet.subsetOf(b.toSet) then
              Some(Style.NEUTRAL)
            else
              Some(Style.WARN)
          })
        )
      )
    }
    val validValuesRefRow = validValuesRef.map{ case(a,b) =>
      Row(
        List(
          Cell("--"+"Valid Values Ref:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(a.getOrElse("(not given)"), style = Some(Style.NEUTRAL)),
          Cell("--"+"Valid Values Ref:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(b.getOrElse("(not given)"), style = Some(Style.NEUTRAL))
        )
      )
    }
    List(presenceRow) ++ dataTypeRow.toList ++ formatRow.toList ++ elementIdRow.toList ++ validValuesRow.toList ++ validValuesRefRow.toList


case class CompositeFieldDifference(
                                     path: Path,
                                     name: String,
                                     canonicalName: String,
                                     presence: (Boolean,Boolean),
                                     required: (Boolean,Boolean),
                                     fieldDiff: List[FieldDifference]
                                   ) extends FieldDifference:
  def render(nestLevel: Int = 0): List[BodyRow] = List.empty


case class SimpleSegmentDifference(
                                    path: Path,
                                    name: String,
                                    canonicalName: String,
                                    presence: (Boolean,Boolean),
                                    required: (Boolean,Boolean),
                                    assertions: Option[(List[String],List[String])] = None,
                                    fieldDiff: List[FieldDifference]
                                  ) extends SegmentDifference


case class LoopSegmentDifference(
                                  path: Path,
                                  name: String,  // initially canonical name but may be renamed
                                  canonicalName: String,  // name used in the canonical spec
                                  presence: (Boolean,Boolean),
                                  required: (Boolean,Boolean),
                                  assertions: Option[(List[String],List[String])] = None,
                                  fieldDiff: List[FieldDifference],
                                  minDiff: Option[(Option[Int], Option[Int])] = None,
                                  maxDiff: Option[(Option[Int], Option[Int])] = None,
                                  bodyDiff: List[SegmentDifference],
                                  nested: Option[List[LoopSegmentDifference]] = None
                                ) extends SegmentDifference:
  override def render(nestLevel: Int): List[BodyRow] =
    val superRows = super.render(nestLevel)
    val minRow = minDiff.map { case (a, b) =>
      val matchStyle = (a, b) match
        case (None, Some(_)) => Some(Style.WARN) // trouble if only given in target--src must be careful
        case _ => Some(Style.NEUTRAL)
      Row(
        List(
          Cell("--" + "Min Repeats:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(a.map(_.toString).getOrElse("(not given)"), style = matchStyle),
          Cell("--" + "Min Repeats:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(b.map(_.toString).getOrElse("(not given)"), style = matchStyle)
        )
      )
    }
    val maxRow = maxDiff.map { case (a, b) =>
      val matchStyle = (a, b) match
        case (None, Some(_)) => Some(Style.WARN) // trouble if only given in target--src must be careful
        case _ => Some(Style.NEUTRAL)
      Row(
        List(
          Cell("--" + "Max Repeats:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(a.map(_.toString).getOrElse("(not given)"), style = matchStyle),
          Cell("--" + "Max Repeats:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(b.map(_.toString).getOrElse("(not given)"), style = matchStyle)
        )
      )
    }
    val bodyRows = bodyDiff.flatMap( bd => bd.render(nestLevel+1) )
    val nestedRows = nested.map( _.flatMap( n => n.render(nestLevel+1) ) ).toList.flatten
    superRows ++ minRow.toList ++ maxRow.toList ++ bodyRows ++ nestedRows

// Used as a kind of exception -- halts further diff comparison
case class DifferenceError(
                              path: Path,
                              message: String
                            ) extends SegmentDifference:
  val name: String = ""
  val canonicalName: String = ""
  val presence: (Boolean, Boolean) = (true,true)
  val required: (Boolean, Boolean) = (true,true)
  val assertions: Option[(List[String], List[String])] = None
  val pathDiff: Option[(String, String)] = None
  val fieldDiff: List[FieldDifference] = Nil

case class FieldDifferenceError(
                            path: Path,
                            message: String
                          ) extends FieldDifference:
  val name: String = ""
  val canonicalName: String = ""
  val presence: (Boolean, Boolean) = (true,true)
  val required: (Boolean, Boolean) = (true,true)
  val assertions: Option[(List[String], List[String])] = None
  val pathDiff: Option[(String, String)] = None
  val fieldDiff: List[FieldDifference] = Nil
  def render(nestLevel: Int = 0): List[BodyRow] = List.empty