package co.blocke.edi4s

import model.*
import zio.*


object DifferenceEngine:

  def computeDifference( src: RefinedDocumentSpec, target: RefinedDocumentSpec ): ZIO[Any, DifferenceError, List[SegmentDifference]] =
    if src.name != target.name then ZIO.fail(DifferenceError(s"These documents represent different EDI specifications (${src.name} and ${target.name})"))
    else
      val diffs = diffWalk(src.segments, target.segments)
      println("DIFFS: \n"+diffs.mkString("\n"))
      ZIO.succeed(diffs)

  def differenceTable( diffs: List[SegmentDifference], title: String ): Table =
    val cells = diffs.foldLeft( List.empty[List[String]] ){ case (acc, d) => d.render("", acc) }
    Table(
      title,
      List("Difference","Source","Target"),
      List(35,40,40),
      cells,
      "color_text"
    )

  private def diffWalk(
                        s: List[RefinedSegmentSpec | RefinedLoopSpec],
                        t: List[RefinedSegmentSpec | RefinedLoopSpec]
                      ): List[SegmentDifference] = (s, t) match {
    case (Nil, Nil) => Nil
    case (sh :: st, Nil) =>
      val diff = createPresenceOnlyDiff(Some(sh), None)
      diff.toList ++ diffWalk(st, Nil)
    case (Nil, th :: tt) =>
      val diff = createPresenceOnlyDiff(None, Some(th))
      diff.toList ++ diffWalk(Nil, tt)

    case (sh :: st, th :: tt) =>
      val sName = nameOf(sh)
      val tName = nameOf(th)

      if (sName == tName) {
        createMatchedDiff(sh, th).toList ++ diffWalk(st, tt)
      } else {
        val sIndexInT = t.indexWhere(nameOf(_) == sName)
        val tIndexInS = s.indexWhere(nameOf(_) == tName)

        if (sIndexInT != -1) {
          val missing = t.take(sIndexInT).flatMap(ti => createPresenceOnlyDiff(None, Some(ti)))
          missing ++ diffWalk(s, t.drop(sIndexInT))
        } else if (tIndexInS != -1) {
          val missing = s.take(tIndexInS).flatMap(si => createPresenceOnlyDiff(Some(si), None))
          missing ++ diffWalk(s.drop(tIndexInS), t)
        } else {
          val left = createPresenceOnlyDiff(Some(sh), None)
          val right = createPresenceOnlyDiff(None, Some(th))
          left.toList ++ right.toList ++ diffWalk(st, tt)
        }
      }
  }

  private def nameOf(x: RefinedSegmentSpec | RefinedLoopSpec): String = x match {
    case s: RefinedSegmentSpec => s.name
    case l: RefinedLoopSpec => l.name
  }

  private def createPresenceOnlyDiff(
                                      source: Option[RefinedSegmentSpec | RefinedLoopSpec],
                                      target: Option[RefinedSegmentSpec | RefinedLoopSpec]
                                    ): Option[SegmentDifference] = (source, target) match {
    case (Some(s: RefinedSegmentSpec), None) =>
      Some(SimpleSegmentDifference(s.name, s.canonicalName, presence = Some(true -> false), required = Some(s.required -> false), assertions = Some(s.assertions -> Nil), fieldDiff = Nil))
    case (None, Some(t: RefinedSegmentSpec)) =>
      Some(SimpleSegmentDifference(t.name, t.canonicalName, presence = Some(false -> true), required = Some(false -> t.required), assertions = Some(Nil -> t.assertions), fieldDiff = Nil))
    case (Some(l: RefinedLoopSpec), None) =>
      Some(LoopSegmentDifference(l.name, l.canonicalName, presence = Some(true -> false), required = Some(l.required -> false), assertions = Some(l.assertions -> Nil), fieldDiff = Nil))
    case (None, Some(r: RefinedLoopSpec)) =>
      Some(LoopSegmentDifference(r.name, r.canonicalName, presence = Some(false -> true), required = Some(false -> r.required), assertions = Some(Nil -> r.assertions), fieldDiff = Nil))
    case _ => None
  }

  private def createMatchedDiff(
                                 s: RefinedSegmentSpec | RefinedLoopSpec,
                                 t: RefinedSegmentSpec | RefinedLoopSpec
                               ): Option[SegmentDifference] = (s, t) match {
    case (sl: RefinedLoopSpec, tl: RefinedLoopSpec) =>
      val required = Some(sl.required -> tl.required).filter { case (a, b) => a != b }
      val assertions = if (sl.assertions != tl.assertions) Some(sl.assertions -> tl.assertions) else None
      if (required.nonEmpty || assertions.nonEmpty)
        Some(LoopSegmentDifference(sl.name, sl.canonicalName, presence = None, required = required, assertions = assertions, fieldDiff = Nil))
      else None

    case (ss: RefinedSegmentSpec, ts: RefinedSegmentSpec) =>
      val required = Some(ss.required -> ts.required).filter { case (a, b) => a != b }
      val assertions = if (ss.assertions != ts.assertions) Some(ss.assertions -> ts.assertions) else None
      if (required.nonEmpty || assertions.nonEmpty)
        Some(SimpleSegmentDifference(ss.name, ss.canonicalName, presence = None, required = required, assertions = assertions, fieldDiff = Nil))
      else None

    case _ => None
  }
