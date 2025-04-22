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
    val presenceRow = Difference.presenceRow( rationalName, nestLevel, presence, required, Style.NEUTRAL )
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
      val (labelStyle, matchStyle) = (a, b) match
        case (None, Some(_)) => (Some(Style.TERTIARY),Some(Style.WARN)) // trouble if only given in target--src must be careful
        case (_,None) => (Some(Style.MUTED),Some(Style.MUTED)) // none given in target--don't care
        case _ => (Some(Style.TERTIARY),Some(Style.WARN))
      Row(
        List(
          Cell("--"+"Format:", indent = nestLevel+1, style = labelStyle),
          Cell(a.getOrElse("(not given)"), style = matchStyle),
          Cell("--"+"Format:", indent = nestLevel+1, style = labelStyle),
          Cell(b.getOrElse("(not given)"), style = matchStyle)
        )
      )
    }
    val elementIdRow = elementId.map { case (a, b) =>
      val (labelStyle, matchStyle) = (a, b) match
        case (None, Some(_)) => (Some(Style.TERTIARY),Some(Style.WARN)) // trouble if only given in target--src must be careful
        case (_,None) => (Some(Style.MUTED),Some(Style.MUTED)) // none given in target--don't care
        case _ => (Some(Style.TERTIARY),Some(Style.WARN))
      Row(
        List(
          Cell("--"+"Element ID:", indent = nestLevel + 1, style = labelStyle),
          Cell(a.map(_.toString).getOrElse("(not given)"), style = matchStyle),
          Cell("--"+"Element ID:", indent = nestLevel + 1, style = labelStyle),
          Cell(b.map(_.toString).getOrElse("(not given)"), style = matchStyle)
        )
      )
    }
    val validValuesRow = validValues.map{ case(a,b) =>
      val (matchValueA, matchValueB, matchStyleA, matchStyleB) = if a.forall(b.contains) then
        ("OK", "OK", Some(Style.TERTIARY), Some(Style.TERTIARY))
      else
        (a.mkString(","), b.mkString(","), Some(Style.WARN), Some(Style.WARN))
      Row(
        List(
          Cell("--"+"Valid Values:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(matchValueA, style = matchStyleA),
          Cell("--"+"Valid Values:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(matchValueB, style = matchStyleB)
        )
      )
    }
    val validValuesRefRow = validValuesRef.map{ case(a,b) =>
      Row(
        List(
          Cell("--"+"Valid Values Ref:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(a.getOrElse("(not given)"), style = Some(Style.WARN)),
          Cell("--"+"Valid Values Ref:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
          Cell(b.getOrElse("(not given)"), style = Some(Style.WARN))
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
  def render(nestLevel: Int = 0): List[BodyRow] =
    val rationalName = if name == canonicalName then name else s"$canonicalName ($name)"
    val presenceRow = Difference.presenceRow(rationalName, nestLevel, presence, required, Style.NEUTRAL)
    val fieldsRows = fieldDiff.flatMap(_.render(nestLevel+1))
    List(presenceRow) ++ fieldsRows


sealed trait SegmentDifference extends Difference:
  val assertions: Option[(List[String], List[String])]
  val fieldDiff: List[FieldDifference]

  def render(nestLevel: Int): List[BodyRow] =
    val rationalName = if name == canonicalName then name else s"$canonicalName ($name)"
    val (r1, r2) = required match {
      case (true, false) => ("required", "optional")
      case (false, true) => ("optional", "required")
      case _ => ("", "")
    }
    val presenceRow = Difference.presenceRow(rationalName, nestLevel, presence, required, Style.SECONDARY)
    val fieldRows = if Difference.isMuted(presenceRow) then List.empty else fieldDiff.flatMap(_.render(nestLevel + 1))
    List(presenceRow) ++ fieldRows


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
    val nestedRows = nested.map( _.flatMap( n =>
      n.presence match {
        case (true,false) => List(Row(List(
          Cell(n.canonicalName, indent = nestLevel+1, style=Some(Style.ALERT)),
          Cell({if n.required._1 then "required" else "optional"}, style=Some(Style.ALERT)),
          Cell(n.canonicalName, indent = nestLevel+1, style=Some(Style.ALERT)),
          Cell("missing", style=Some(Style.ALERT))
        )))
        case (false, true) if !n.required._2 => List.empty // no issue if src is missing if not required in target
        case (false, true) => List(Row(List(
          Cell(n.canonicalName, indent = nestLevel+1, style=Some(Style.ALERT)),
          Cell("missing", style=Some(Style.ALERT)),
          Cell(n.canonicalName, indent = nestLevel+1, style=Some(Style.ALERT)),
          Cell({if n.required._1 then "required" else "optional"}, style=Some(Style.ALERT))
        )))
        case _ => n.render(nestLevel+1)
      }
    ) ).toList.flatten
    superRows ++ minRow.toList ++ maxRow.toList ++ bodyRows ++ nestedRows


case class HLSegmentDifference(
                                path: Path,
                                name: String,  // initially canonical name but may be renamed
                                canonicalName: String,  // name used in the canonical spec
                                presence: (Boolean,Boolean),
                                required: (Boolean,Boolean),
                              ) extends SegmentDifference:
  val assertions: Option[(List[String], List[String])] = None
  val fieldDiff: List[FieldDifference] = List.empty


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


object Difference:

  def isMuted(row: Row): Boolean = row.cells.forall(_.style.contains(Style.MUTED))

  def presenceRow( label: String, nestLevel: Int, presence: (Boolean,Boolean), required: (Boolean,Boolean), okStyle: Style ): Row =
    presence match {
      case (_, false) => // target missing--nothing else matters
        Row(
          List(
            Cell(label, indent = nestLevel, style = Some(Style.MUTED)),
            Cell("skipped", style = Some(Style.MUTED)),
            Cell(label, indent = nestLevel, style = Some(Style.MUTED)),
            Cell("missing", style = Some(Style.MUTED))
          )
        )
      case (false, _) if !required._2 => // src missing, target present/optional
        Row(
          List(
            Cell(label, indent = nestLevel, style = Some(Style.MUTED)),
            Cell("missing", style = Some(Style.MUTED)),
            Cell(label, indent = nestLevel, style = Some(Style.MUTED)),
            Cell("optional", style = Some(Style.MUTED))
          )
        )
      case (false, _) if required._2 => // src missing, target present/required
        Row(
          List(
            Cell(label, indent = nestLevel, style = Some(Style.MUTED)),
            Cell("missing", style = Some(Style.WARN)),
            Cell(label, indent = nestLevel, style = Some(okStyle)),
            Cell("required", style = Some(Style.WARN))
          )
        )
      case (true, _) if required == (false, true) => // src optional, target required
        Row(
          List(
            Cell(label, indent = nestLevel, style = Some(okStyle)),
            Cell("optional", style = Some(Style.WARN)),
            Cell(label, indent = nestLevel, style = Some(okStyle)),
            Cell("required", style = Some(Style.WARN))
          )
        )
      case (true, _) if required == (true, false) => // src required, target optional
        Row(
          List(
            Cell(label, indent = nestLevel, style = Some(okStyle)),
            Cell("OK (required)", style = Some(Style.TERTIARY)),
            Cell(label, indent = nestLevel, style = Some(okStyle)),
            Cell("OK (optional)", style = Some(Style.TERTIARY))
          )
        )
      case _ => // all other combos
        Row(
          List(
            Cell(label, indent = nestLevel, style = Some(okStyle)),
            Cell("OK", style = Some(Style.TERTIARY)),
            Cell(label, indent = nestLevel, style = Some(okStyle)),
            Cell("OK", style = Some(Style.TERTIARY))
          )
        )
    }