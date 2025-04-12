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
    case (sh :: st, Nil) => createPresenceOnlyDiff(Some(sh), None).toList ++ diffWalk(st, Nil)
    case (Nil, th :: tt) => createPresenceOnlyDiff(None, Some(th)).toList ++ diffWalk(Nil, tt)

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
      Some(SimpleSegmentDifference(s.name, s.canonicalName, presence = Some(true -> false), required = Some(s.required -> false), assertions = Some(s.assertions -> Nil), fieldDiff = s.fields.map(f => createFieldDiff(Some(f), None)).flatten))
    case (None, Some(t: RefinedSegmentSpec)) =>
      Some(SimpleSegmentDifference(t.name, t.canonicalName, presence = Some(false -> true), required = Some(false -> t.required), assertions = Some(Nil -> t.assertions), fieldDiff = t.fields.map(f => createFieldDiff(None, Some(f))).flatten))
    case (Some(l: RefinedLoopSpec), None) =>
      Some(LoopSegmentDifference(l.name, l.canonicalName, presence = Some(true -> false), required = Some(l.required -> false), assertions = Some(l.assertions -> Nil), fieldDiff = l.fields.map(f => createFieldDiff(Some(f), None)).flatten))
    case (None, Some(r: RefinedLoopSpec)) =>
      Some(LoopSegmentDifference(r.name, r.canonicalName, presence = Some(false -> true), required = Some(false -> r.required), assertions = Some(Nil -> r.assertions), fieldDiff = r.fields.map(f => createFieldDiff(None, Some(f))).flatten))
    case _ => None
  }

  private def createMatchedDiff(
                                 s: RefinedSegmentSpec | RefinedLoopSpec,
                                 t: RefinedSegmentSpec | RefinedLoopSpec
                               ): Option[SegmentDifference] = (s, t) match {
    case (sl: RefinedLoopSpec, tl: RefinedLoopSpec) =>
      val required = Some(sl.required -> tl.required).filter(_ != _)
      val assertions = Some(sl.assertions -> tl.assertions).filter(_ != _)
      val fields = diffFields(sl.fields, tl.fields)

      if (required.nonEmpty || assertions.nonEmpty || fields.nonEmpty)
        Some(LoopSegmentDifference(sl.name, sl.canonicalName, presence = None, required = required, assertions = assertions, fieldDiff = fields))
      else None

    case (ss: RefinedSegmentSpec, ts: RefinedSegmentSpec) =>
      val required = Some(ss.required -> ts.required).filter(_ != _)
      val assertions = Some(ss.assertions -> ts.assertions).filter(_ != _)
      val fields = diffFields(ss.fields, ts.fields)

      if (required.nonEmpty || assertions.nonEmpty || fields.nonEmpty)
        Some(SimpleSegmentDifference(ss.name, ss.canonicalName, presence = None, required = required, assertions = assertions, fieldDiff = fields))
      else None

    case _ => None
  }

  private def diffFields(
                          a: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                          b: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec]
                        ): List[FieldDifference] = (a, b) match {
    case (Nil, Nil) => Nil
    case (ah :: at, Nil) => createFieldDiff(Some(ah), None).toList ++ diffFields(at, Nil)
    case (Nil, bh :: bt) => createFieldDiff(None, Some(bh)).toList ++ diffFields(Nil, bt)
    case (ah :: at, bh :: bt) =>
      val aName = ah match {
        case f: RefinedSingleFieldSpec => f.name
        case c: RefinedCompositeFieldSpec => c.name
      }
      val bName = bh match {
        case f: RefinedSingleFieldSpec => f.name
        case c: RefinedCompositeFieldSpec => c.name
      }
      if (aName == bName) {
        createFieldDiff(Some(ah), Some(bh)).toList ++ diffFields(at, bt)
      } else {
        val aIndexInB = b.indexWhere(f => (f match {
          case x: RefinedSingleFieldSpec => x.name
          case x: RefinedCompositeFieldSpec => x.name
        }) == aName)
        val bIndexInA = a.indexWhere(f => (f match {
          case x: RefinedSingleFieldSpec => x.name
          case x: RefinedCompositeFieldSpec => x.name
        }) == bName)
        if (aIndexInB != -1) {
          b.take(aIndexInB).flatMap(f => createFieldDiff(None, Some(f))) ++ diffFields(a, b.drop(aIndexInB))
        } else if (bIndexInA != -1) {
          a.take(bIndexInA).flatMap(f => createFieldDiff(Some(f), None)) ++ diffFields(a.drop(bIndexInA), b)
        } else {
          createFieldDiff(Some(ah), None).toList ++ createFieldDiff(None, Some(bh)).toList ++ diffFields(at, bt)
        }
      }
  }

  private def createFieldDiff(
                               s: Option[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                               t: Option[RefinedSingleFieldSpec | RefinedCompositeFieldSpec]
                             ): Option[FieldDifference] = (s, t) match {
    case (Some(ss: RefinedSingleFieldSpec), Some(st: RefinedSingleFieldSpec)) =>
      val presence = None
      val required = Some(ss.required -> st.required).filter(_ != _)
      val dataType = Some(ss.dataType -> st.dataType).filter(_ != _)
      val format = Some(ss.format -> st.format).filter(_ != _)
      val elementId = Some(ss.elementId -> st.elementId).filter(_ != _)
      val validValues = Some(ss.validValues -> st.validValues).filter(_ != _)
      val validValuesRef = Some(ss.validValuesRef -> st.validValuesRef).filter(_ != _)

      if (List(required, dataType, format, elementId, validValues, validValuesRef).exists(_.nonEmpty))
        Some(SingleFieldDifference(ss.name, ss.canonicalName, presence, required, dataType, format, elementId, validValues, validValuesRef))
      else None

    case (Some(sc: RefinedCompositeFieldSpec), Some(tc: RefinedCompositeFieldSpec)) =>
      val presence = None
      val required = Some(sc.required -> tc.required).filter(_ != _)
      val innerFieldDiffs = diffFields(sc.components, tc.components)
      if (required.nonEmpty || innerFieldDiffs.nonEmpty)
        Some(CompositeFieldDifference(sc.name, sc.canonicalName, presence, required, innerFieldDiffs))
      else None

    case (Some(sf: RefinedSingleFieldSpec), None) =>
      Some(SingleFieldDifference(sf.name, sf.canonicalName, presence = Some(true -> false), required = Some(sf.required -> false), dataType = None, format = None, elementId = None, validValues = None, validValuesRef = None))
    case (None, Some(sf: RefinedSingleFieldSpec)) =>
      Some(SingleFieldDifference(sf.name, sf.canonicalName, presence = Some(false -> true), required = Some(false -> sf.required), dataType = None, format = None, elementId = None, validValues = None, validValuesRef = None))

    case (Some(cf: RefinedCompositeFieldSpec), None) =>
      Some(CompositeFieldDifference(cf.name, cf.canonicalName, presence = Some(true -> false), required = Some(cf.required -> false), fieldDiff = cf.components.map(c => createFieldDiff(Some(c), None)).flatten))
    case (None, Some(cf: RefinedCompositeFieldSpec)) =>
      Some(CompositeFieldDifference(cf.name, cf.canonicalName, presence = Some(false -> true), required = Some(false -> cf.required), fieldDiff = cf.components.map(c => createFieldDiff(None, Some(c)).get)))

    case _ => None
  }
