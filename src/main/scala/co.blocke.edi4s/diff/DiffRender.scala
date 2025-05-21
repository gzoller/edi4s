package co.blocke.edi4s
package diff

import model.*
import table.*


extension( fd: FieldDifference )
  def render(nestLevel: Int): List[BodyRow] = fd match {
    case sfd: SingleFieldDifference =>
      val rationalName = if sfd.name == sfd.canonicalName then sfd.name else s"${sfd.canonicalName} (${sfd.name})"
      val presenceRow = DiffRender.presenceRow(rationalName, nestLevel, sfd.presence, sfd.required, Style.NEUTRAL)
      val dataTypeRow = sfd.dataType.map { case (a, b) =>
        Row(
          List(
            Cell("--" + "Data Type:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
            Cell(a, style = Some(Style.NEUTRAL)),
            Cell("--" + "Data Type:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
            Cell(b, style = Some(Style.NEUTRAL))
          )
        )
      }
      val formatRow = sfd.format.map { case (a, b) =>
        val (labelStyle, matchStyle) = (a, b) match
          case (None, Some(_)) => (Some(Style.TERTIARY), Some(Style.WARN)) // trouble if only given in target--src must be careful
          case (_, None) => (Some(Style.MUTED), Some(Style.MUTED)) // none given in target--don't care
          case _ => (Some(Style.TERTIARY), Some(Style.WARN))
        Row(
          List(
            Cell("--" + "Format:", indent = nestLevel + 1, style = labelStyle),
            Cell(a.getOrElse("(not given)"), style = matchStyle),
            Cell("--" + "Format:", indent = nestLevel + 1, style = labelStyle),
            Cell(b.getOrElse("(not given)"), style = matchStyle)
          )
        )
      }
      val elementIdRow = sfd.elementId.map { case (a, b) =>
        val (labelStyle, matchStyle) = (a, b) match
          case (None, Some(_)) => (Some(Style.TERTIARY), Some(Style.WARN)) // trouble if only given in target--src must be careful
          case (_, None) => (Some(Style.MUTED), Some(Style.MUTED)) // none given in target--don't care
          case _ => (Some(Style.TERTIARY), Some(Style.WARN))
        Row(
          List(
            Cell("--" + "Element ID:", indent = nestLevel + 1, style = labelStyle),
            Cell(a.map(_.toString).getOrElse("(not given)"), style = matchStyle),
            Cell("--" + "Element ID:", indent = nestLevel + 1, style = labelStyle),
            Cell(b.map(_.toString).getOrElse("(not given)"), style = matchStyle)
          )
        )
      }
      val validValuesRow = sfd.validValues.map { case (a, b) =>
        val (matchValueA, matchValueB, matchStyleA, matchStyleB) = if a.forall(b.contains) then
          ("OK", "OK", Some(Style.TERTIARY), Some(Style.TERTIARY))
        else
          (a.mkString(","), b.mkString(","), Some(Style.WARN), Some(Style.WARN))
        Row(
          List(
            Cell("--" + "Valid Values:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
            Cell(matchValueA, style = matchStyleA),
            Cell("--" + "Valid Values:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
            Cell(matchValueB, style = matchStyleB)
          )
        )
      }
      val validValuesRefRow = sfd.validValuesRef.map { case (a, b) =>
        Row(
          List(
            Cell("--" + "Valid Values Ref:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
            Cell(a.getOrElse("(not given)"), style = Some(Style.WARN)),
            Cell("--" + "Valid Values Ref:", indent = nestLevel + 1, style = Some(Style.TERTIARY)),
            Cell(b.getOrElse("(not given)"), style = Some(Style.WARN))
          )
        )
      }
      List(presenceRow) ++ dataTypeRow.toList ++ formatRow.toList ++ elementIdRow.toList ++ validValuesRow.toList ++ validValuesRefRow.toList

    case cfd: CompositeFieldDifference =>
      val rationalName = if cfd.name == cfd.canonicalName then cfd.name else s"${cfd.canonicalName} (${cfd.name})"
      val presenceRow = DiffRender.presenceRow(rationalName, nestLevel, cfd.presence, cfd.required, Style.NEUTRAL)
      val fieldsRows = cfd.fieldDiff.flatMap(_.render(nestLevel+1))
      List(presenceRow) ++ fieldsRows

    case fde: FieldDifferenceError => List.empty
  }


extension (sd: SegmentDifference)
  private def renderSeg(nestLevel: Int, s: SegmentDifference): List[BodyRow] =
    val rationalName = if s.name == s.canonicalName then s.name else s"${s.canonicalName} (${s.name})"
    val (r1, r2) = s.required match {
      case (true, false) => ("required", "optional")
      case (false, true) => ("optional", "required")
      case _ => ("", "")
    }
    val presenceRow = DiffRender.presenceRow(rationalName, nestLevel, s.presence, s.required, Style.SECONDARY)
    val fieldRows = if DiffRender.isMuted(presenceRow) then List.empty else s.fieldDiff.flatMap(_.render(nestLevel + 1))
    List(presenceRow) ++ fieldRows

  def render(nestLevel: Int = 0): List[BodyRow] = sd match {
    case lsd: LoopSegmentDifference =>
      val superRows = renderSeg(nestLevel, lsd)
      val minRow = lsd.minDiff.map { case (a, b) =>
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
      val maxRow = lsd.maxDiff.map { case (a, b) =>
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
      val bodyRows = lsd.bodyDiff.flatMap( bd => bd.render(nestLevel+1) )
      val nestedRows = lsd.nested.map( _.flatMap( n =>
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
    case s: SegmentDifference => renderSeg(nestLevel, s)
  }


object DiffRender:

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
