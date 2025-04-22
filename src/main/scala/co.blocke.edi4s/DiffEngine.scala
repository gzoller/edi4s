package co.blocke.edi4s

import model.*
import scala.collection.mutable
import scala.annotation.tailrec
import pprint.*

object DiffEngine:

  //
  //  TOP-LEVEL
  //
  def compareSpecs(
                    src: RefinedDocumentSpec,
                    edi: RefinedDocumentSpec,
                    target: RefinedDocumentSpec
                  ): List[Difference] =
    compareSegmentLists(Path(), src.segments, edi.segments, target.segments)

  @tailrec
  private def compareSegmentLists(
                           path: Path,
                           src: List[RefinedSegmentSpec | RefinedLoopSpec],
                           edi: List[RefinedSegmentSpec | RefinedLoopSpec],
                           target: List[RefinedSegmentSpec | RefinedLoopSpec],
                           acc: List[SegmentDifference] = List.empty
  ): List[SegmentDifference] =
    val nextLoop = (src, edi, target) match {
      case (_, Nil, _ :: _) | (_ :: _, Nil, _) =>
        (Nil,Nil,Nil,acc :+ DifferenceError(path, "Exhausted EDI standard segments before either src/target--they have extra (non-standard) fields"))
      case (Nil, Nil, Nil) =>
        (Nil,Nil,Nil,acc) // done comparing lists
      case (sH :: sT, eH :: eT, tH :: tT) =>
        // 3-way match -> compare sH and tH
        if rawCName(sH) == rawCName(eH) && rawCName(eH) == rawCName(tH) then
          (sH,eH,tH) match
            case (sHS: RefinedSegmentSpec, eHS: RefinedSegmentSpec, tHS: RefinedSegmentSpec) =>
              (sT, eT, tT, acc :+ compareTwoSegments(path, sHS, eHS, tHS))
            case (sHS: RefinedLoopSpec, eHS: RefinedLoopSpec, tHS: RefinedLoopSpec) =>
              (sT, eT, tT, acc :+ compareTwoLoops(path, sHS, eHS, tHS))
            case _ =>
              (Nil,Nil,Nil, acc :+ DifferenceError(path, s"Matching elements ${rawCName(sH)} have different major types (loop vs segment)"))

        // src has edi segment, target does not
        else if rawCName(sH) == rawCName(eH) && rawCName(eH) != rawCName(tH) then
          (sH, eH) match
            case (sHS: RefinedSegmentSpec, eHS: RefinedSegmentSpec) =>
              (sT,eT,target, acc :+ burnSrcSegment(path, sHS, eHS))
            case (sHS: RefinedLoopSpec, eHS: RefinedLoopSpec) =>
              (sT,eT,target,acc) // TODO: Fix acc
            case _ =>
              (Nil,Nil,Nil, acc :+ DifferenceError(path, s"Source element ${rawCName(sH)} has a different major types (loop vs segment) than EDI standard"))

        // src does not have edi segment, target does
        else if rawCName(sH) != rawCName(eH) && rawCName(eH) == rawCName(tH) then
          (eH, tH) match
            case (eHS: RefinedSegmentSpec, tHS: RefinedSegmentSpec) =>
              (src,eT,tT, acc :+ burnTargetSegment(path, eHS, tHS))
            case (eHS: RefinedLoopSpec, tHS: RefinedLoopSpec) =>
              (src,eT,tT,acc :+ burnTargetLoop(path, eHS, tHS)) // TODO: Fix acc
            case _ =>
              (Nil,Nil,Nil, acc :+ DifferenceError(path, s"Target element ${rawCName(tH)} has a different major types (loop vs segment) than EDI standard"))

        // neither have edi segment
        else
          (src, eT, target, acc :+ SimpleSegmentDifference(path, nameOf(eH), canonicalNameOf(eH), (false, false), (isRequired(sH), isRequired(tH)), None, List.empty))

      case (Nil, eH :: eT, tH :: tT) =>
        (eH, tH) match
          case (eHS: RefinedSegmentSpec, tHS: RefinedSegmentSpec) =>
            (Nil,eT,tT, acc :+ burnTargetSegment(path, eHS, tHS))
          case (eHS: RefinedLoopSpec, tHS: RefinedLoopSpec) =>
            (Nil,eT,tT,acc :+ burnTargetLoop(path, eHS, tHS)) // TODO: Fix acc
          case _ =>
            (Nil,Nil,Nil, acc :+ DifferenceError(path, s"Target element ${rawCName(tH)} has a different major types (loop vs segment) than EDI standard"))
      case (sH :: sT, eH :: eT, Nil) =>
        (sH, eH) match
          case (sHS: RefinedSegmentSpec, eHS: RefinedSegmentSpec) =>
            (sT,eT,Nil, acc :+ burnSrcSegment(path, sHS, eHS))
          case (sHS: RefinedLoopSpec, eHS: RefinedLoopSpec) =>
            (sT,eT,Nil,acc) // TODO: Fix acc
          case _ =>
            (Nil,Nil,Nil, acc :+ DifferenceError(path, s"Source element ${rawCName(sH)} has a different major types (loop vs segment) than EDI standard"))
      case (Nil, eH :: eT, Nil) =>
        (Nil, eT, Nil, acc :+ burnEdiSegment(path, eH))
    }
    nextLoop match {
      case (Nil,Nil,Nil,nextAcc) =>
        nextAcc
      case (s,e,t,nextAcc) =>
        compareSegmentLists(path, s, e, t, nextAcc)
    }


  private def compareTwoLoops(path: Path, src: RefinedLoopSpec, edi: RefinedLoopSpec, target: RefinedLoopSpec): LoopSegmentDifference =
    val nestedDiff = if src.canonicalName == "HL" then
      Some(nestedCompare(path, src.nested, edi, target.nested))
    else
      None
    LoopSegmentDifference(
      path,
      nameOf(src),
      canonicalNameOf(src),
      (true,true),
      (src.required, src.required),
      Option.when(src.assertions.sorted != target.assertions.sorted)(
        (src.assertions, target.assertions)
      ),
      compareSegmentFields(path.dot(src.canonicalName), src.fields, edi.fields, target.fields),
      Option.when(src.minRepeats != target.minRepeats) {
        (src.minRepeats, target.minRepeats)
      },
      Option.when(src.maxRepeats != target.maxRepeats) {
        (src.maxRepeats, target.maxRepeats)
      },
      compareSegmentLists(path.dot(canonicalNameOf(src)), src.body, edi.body, target.body),
      nestedDiff
    )



  private def compareTwoSegments(
                                path: Path,
                                src: RefinedSegmentSpec,
                                edi: RefinedSegmentSpec,
                                target: RefinedSegmentSpec
                                ): SegmentDifference =
    SimpleSegmentDifference(
      path,
      src.name,
      src.canonicalName,
      (true,true),
      (src.required, target.required),
      Option.when(src.assertions.sorted != target.assertions.sorted)(
        (src.assertions, target.assertions)
      ),
      compareSegmentFields(path.dot(src.canonicalName), src.fields, edi.fields, target.fields)
  )

  private def burnEdiSegment(
                              path: Path,
                              edi: RefinedSegmentSpec | RefinedLoopSpec
                            ): SegmentDifference =
    SimpleSegmentDifference(
      path,
      nameOf(edi),
      canonicalNameOf(edi),
      (false, false),
      (isRequired(edi), false),
      None,
      {
        edi match {
          case e: RefinedSegmentSpec => e.fields.map(f => burnEdiField(path: Path, f))
          case e: RefinedLoopSpec => e.fields.map(f => burnEdiField(path: Path, f))
        }
      }
    )

  // src exists, target not
  private def burnSrcSegment(
                                  path: Path,
                                  src: RefinedSegmentSpec,
                                  edi: RefinedSegmentSpec
                                ): SegmentDifference =
    SimpleSegmentDifference(
      path,
      src.name,
      src.canonicalName,
      (true,false),
      (src.required, false),
      None,
      burnSrcSegmentFields(path.dot(src.canonicalName), src.fields, edi.fields)
    )

  // src exists, target not
  private def burnTargetSegment(
                              path: Path,
                              edi: RefinedSegmentSpec,
                              target: RefinedSegmentSpec
                            ): SegmentDifference =
    SimpleSegmentDifference(
      path,
      target.name,
      target.canonicalName,
      (false,true),
      (false, target.required),
      None,
      burnTargetSegmentFields(path.dot(target.canonicalName), edi.fields, target.fields)
    )

  // src exists, target not
  private def burnTargetLoop(
                                 path: Path,
                                 edi: RefinedLoopSpec,
                                 target: RefinedLoopSpec
                               ): SegmentDifference =
    LoopSegmentDifference(
      path,
      target.name,
      target.canonicalName,
      (false, true),
      (false, target.required),
      None,
      burnTargetSegmentFields(path.dot(target.canonicalName), edi.fields, target.fields),
      None,
      None,
      List.empty,
      None
    )

  @tailrec
  private def compareSegmentFields(
                                    path: Path,
                                    src: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                                    edi: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                                    target: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                                    acc: List[FieldDifference] = List.empty
                                  ): List[FieldDifference] =
    val nextLoop = (src, edi, target) match {
      case (_, Nil, _ :: _) | (_ :: _, Nil, _) =>
        (Nil, Nil, Nil, acc :+ FieldDifferenceError(path, "Exhausted EDI standard fields before either src/target--they have extra (non-standard) fields"))
      case (Nil, Nil, Nil) =>
        (Nil, Nil, Nil, acc) // done comparing lists
      case (sH :: sT, eH :: eT, tH :: tT) =>
        // 3-way match -> compare sH and tH
        if canonicalFieldNameOf(sH) == canonicalFieldNameOf(eH) && canonicalFieldNameOf(eH) == canonicalFieldNameOf(tH) then
          (sH, eH, tH) match
            case (sHS: RefinedSingleFieldSpec, eHS: RefinedSingleFieldSpec, tHS: RefinedSingleFieldSpec) =>
              (sT, eT, tT, acc :+ compareTwoSingleFields(path, sHS, tHS))
            case (sHS: RefinedCompositeFieldSpec, eHS: RefinedCompositeFieldSpec, tHS: RefinedCompositeFieldSpec) =>
              (sT, eT, tT, acc) // TODO: fix acc
            case _ =>
              (Nil, Nil, Nil, acc :+ FieldDifferenceError(path, s"Matching fields ${fieldNameOf(sH)} have different major types (simple vs composite)"))
        else if canonicalFieldNameOf(sH) == canonicalFieldNameOf(eH) && canonicalFieldNameOf(eH) != canonicalFieldNameOf(tH) then
          (sH, eH, tH) match
            case (sHS: RefinedSingleFieldSpec, eHS: RefinedSingleFieldSpec, tHS: RefinedSingleFieldSpec) =>
              (sT, eT, target, acc :+ burnSingleField(path, sHS, true))
            case (sHS: RefinedCompositeFieldSpec, eHS: RefinedCompositeFieldSpec, tHS: RefinedCompositeFieldSpec) =>
              (sT, eT, target, acc) // TODO: fix acc
            case _ =>
              (Nil, Nil, Nil, acc :+ FieldDifferenceError(path, s"Matching fields ${fieldNameOf(sH)} have different major types (simple vs composite)"))
        else if canonicalFieldNameOf(sH) != canonicalFieldNameOf(eH) && canonicalFieldNameOf(eH) == canonicalFieldNameOf(tH) then
          (sH, eH, tH) match
            case (sHS: RefinedSingleFieldSpec, eHS: RefinedSingleFieldSpec, tHS: RefinedSingleFieldSpec) =>
              (src, eT, tT, acc :+ burnSingleField(path, tHS, false))
            case (sHS: RefinedCompositeFieldSpec, eHS: RefinedCompositeFieldSpec, tHS: RefinedCompositeFieldSpec) =>
              (src, eT, tT, acc) // TODO: fix acc
            case _ =>
              (Nil, Nil, Nil, acc :+ FieldDifferenceError(path, s"Matching fields ${fieldNameOf(sH)} have different major types (simple vs composite)"))
        else
          (src, eT, target, acc :+ SingleFieldDifference(path, fieldNameOf(eH), canonicalFieldNameOf(eH), (false, false), (isFieldRequired(sH), isFieldRequired(tH)), None, None))
      case (Nil, eH :: eT, tH :: tT) =>
        (eH, tH) match
          case (eHS: RefinedSingleFieldSpec, tHS: RefinedSingleFieldSpec) =>
            (Nil,eT,tT, acc :+ burnSingleField(path, tHS, false))
          case (eHS: RefinedCompositeFieldSpec, tHS: RefinedCompositeFieldSpec) =>
            (Nil,eT,tT,acc ) // TODO: Fix acc
          case _ =>
            (Nil,Nil,Nil, acc :+ FieldDifferenceError(path, s"Target field ${fieldNameOf(tH)} has a different major types (loop vs segment) than EDI standard"))
      case (sH :: sT, eH :: eT, Nil) =>
        (sH, eH) match
          case (sHS: RefinedSingleFieldSpec, eHS: RefinedSingleFieldSpec) =>
            (sT,eT,Nil, acc :+ burnSingleField(path, sHS, true))
          case (sHS: RefinedCompositeFieldSpec, eHS: RefinedCompositeFieldSpec) =>
            (sT,eT,Nil,acc) // TODO: Fix acc
          case _ =>
            (Nil,Nil,Nil, acc :+ FieldDifferenceError(path, s"Source element ${fieldNameOf(sH)} has a different major types (loop vs segment) than EDI standard"))
      case (Nil, eH :: eT, Nil) =>
        (Nil, eT, Nil, acc :+ burnEdiField(path, eH))
    }
    nextLoop match {
      case (Nil,Nil,Nil,nextAcc) =>
        nextAcc
      case (s,e,t,nextAcc) =>
        compareSegmentFields(path, s, e, t, nextAcc)
    }


  private def compareTwoSingleFields(path: Path, src: RefinedSingleFieldSpec, target: RefinedSingleFieldSpec): FieldDifference =
    SingleFieldDifference(
      path,
      src.name,
      src.canonicalName,
      (true,true),
      (src.required, target.required),
      Option.when(src.dataType != target.dataType)(
        (src.dataType, target.dataType)
      ),
      Option.when(src.format != target.format)(
        (src.format, target.format)
      ),
      Option.when(src.elementId != target.elementId)(
        (src.elementId, target.elementId)
      ),
      Option.when(src.validValues.sorted != target.validValues.sorted)(
        (src.validValues, target.validValues)
      ),
      Option.when(src.validValuesRef != target.validValuesRef)(
        (src.validValuesRef, target.validValuesRef)
      )
    )

  private def burnSrcSegmentFields(
                              path: Path,
                              src: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                              edi: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec]
                            ): List[FieldDifference] = List.empty // TODO!

  private def burnTargetSegmentFields(
                                    path: Path,
                                    edi: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                                    target: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec]
                                  ): List[FieldDifference] = List.empty // TODO!

  private def burnSingleField(path: Path, f: RefinedSingleFieldSpec, isSrc: Boolean): FieldDifference =
    SingleFieldDifference(
      path,
      f.name,
      f.canonicalName,
      {
        if isSrc then (true, false) else (false, true)
      },
      {
        if isSrc then (f.required, false) else (false, f.required)
      },
      None,
      None,
      None,
      None
    )

  private def burnEdiField(path: Path, f: RefinedSingleFieldSpec | RefinedCompositeFieldSpec): FieldDifference =
    SingleFieldDifference(
      path,
      fieldNameOf(f),
      canonicalFieldNameOf(f),
      (false,false),
      {
        f match {
          case ff: RefinedSingleFieldSpec => (ff.required, ff.required)
          case ff: RefinedCompositeFieldSpec => (ff.required, ff.required)
        }
      },
      None,
      None,
      None,
      None
    )


  // TODO: Sew Path through scanTarget, loop, and advanceTo()

  private def burnSrcHL(path: Path, target: RefinedLoopSpec): LoopSegmentDifference = ???
  private def burnTargetHL(path: Path, target: RefinedLoopSpec): LoopSegmentDifference = ???

  private def nestedCompare(path: Path, src: Option[RefinedLoopSpec], edi: RefinedLoopSpec, target: Option[RefinedLoopSpec]): List[LoopSegmentDifference] = {
    println("Comparing nested loop: " + src.map(s=>canonicalNameOf(s)).getOrElse("none")+" and "+target.map(s=>canonicalNameOf(s)).getOrElse("none"))
    @tailrec
    def scanTarget(t: Option[RefinedLoopSpec], name: String, acc: List[LoopSegmentDifference]): (Boolean, List[LoopSegmentDifference]) =
      t match {
        case Some(tt) if canonicalNameOf(tt) == name => (true, acc)
        case Some(tt) => scanTarget(tt.nested, name, acc :+ burnTargetHL(path.nest(canonicalNameOf(tt)), tt))
        case None => (false, acc)
      }

    @tailrec
    def loop(x: Option[RefinedLoopSpec], y: Option[RefinedLoopSpec], result: List[LoopSegmentDifference]): List[LoopSegmentDifference] = (x, y) match {
      case (None, None) => result
      case (None, Some(tt)) => loop(None, tt.nested, result :+ burnTargetHL(path.nest(canonicalNameOf(tt)), tt))
      case (Some(xx), None) => loop(xx.nested, None, result :+ burnSrcHL(path.nest(canonicalNameOf(xx)), xx))
      case (Some(xx), Some(yy)) if xx.name == yy.name =>
        result :+ compareTwoLoops(path, xx, edi, yy)
      case (Some(xx), Some(_)) =>
        val (found, acc) = scanTarget(y, xx.name, Nil)
        if found then
          val y2 = advanceTo(y, xx.name).flatMap(_.nested)
          result :+ compareTwoLoops(path, xx, edi, y2.get)
        else
          loop(xx.nested, y, result :+ burnSrcHL(path.nest(canonicalNameOf(xx)), xx))
    }

    @tailrec
    def advanceTo(y: Option[RefinedLoopSpec], name: String): Option[RefinedLoopSpec] =
      y match
        case Some(t) if canonicalNameOf(t) == name => y
        case Some(t) => advanceTo(t.nested, name)
        case None => None

    loop(src, target, Nil)
  }


  // Utilities
  //-----------------------------------------------------------
  private inline def isRequired(x: RefinedSegmentSpec | RefinedLoopSpec): Boolean = x match {
    case s: RefinedSegmentSpec => s.required
    case l: RefinedLoopSpec => l.required
  }

  private inline def isFieldRequired(x: RefinedSingleFieldSpec | RefinedCompositeFieldSpec): Boolean = x match {
    case s: RefinedSingleFieldSpec => s.required
    case l: RefinedCompositeFieldSpec => l.required
  }

  private inline def nameOf(x: RefinedSegmentSpec | RefinedLoopSpec): String = x match {
    case s: RefinedSegmentSpec => s.name
    case l: RefinedLoopSpec => l.name
  }

  private inline def rawCName(x: RefinedSegmentSpec | RefinedLoopSpec): String = x match {
    case s: RefinedSegmentSpec => s.canonicalName
    case l: RefinedLoopSpec => l.canonicalName
  }

  private def canonicalNameOf(x: RefinedSegmentSpec | RefinedLoopSpec): String = x match {
    case s: RefinedSegmentSpec => s.canonicalName
    case l: RefinedLoopSpec =>
      if l.canonicalName == "HL" && l.description.nonEmpty then
        l.canonicalName + s"[${l.description}]"
      else
        l.canonicalName
  }

  private def fieldNameOf(x: RefinedSingleFieldSpec | RefinedCompositeFieldSpec): String = x match {
    case s: RefinedSingleFieldSpec => s.name
    case l: RefinedCompositeFieldSpec => l.name
  }

  private def canonicalFieldNameOf(x: RefinedSingleFieldSpec | RefinedCompositeFieldSpec): String = x match {
    case s: RefinedSingleFieldSpec => s.canonicalName
    case l: RefinedCompositeFieldSpec => l.canonicalName
  }

  private def equalIgnoringBrackets(a: String, b: String): Boolean = {
    def stripBrackets(s: String): String =
      s.replaceAll("\\[.*?\\]", "")

    stripBrackets(a) == stripBrackets(b)
  }