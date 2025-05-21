package co.blocke.edi4s
package diff

import co.blocke.edi4s.model.*
import pprint.*

import scala.annotation.tailrec
import scala.collection.mutable

object DiffEngine:

  //
  //  TOP-LEVEL
  //
  def compareSpecs(
                    src: RefinedDocumentSpec,
                    edi: RefinedDocumentSpec,
                    target: RefinedDocumentSpec
                  ): List[SegmentDifference] =
    compareSegmentLists(Path(), src.segments, edi.segments, target.segments)

  @tailrec
  private def compareSegmentLists(
                           path: Path,
                           src: List[RefinedSingleOrLoopSegmentSpec],
                           edi: List[RefinedSingleOrLoopSegmentSpec],
                           target: List[RefinedSingleOrLoopSegmentSpec],
                           acc: List[SegmentDifference] = List.empty
  ): List[SegmentDifference] =
    val nextLoop = (src, edi, target) match {
      case (_, Nil, _ :: _) | (_ :: _, Nil, _) =>
        (Nil,Nil,Nil,acc :+ DifferenceError(path, "Exhausted EDI standard segments before either src/target--they have extra (non-standard) fields"))
      case (Nil, Nil, Nil) =>
        (Nil,Nil,Nil,acc) // done comparing lists
      case (sH :: sT, eH :: eT, tH :: tT) =>
        // 3-way match -> compare sH and tH
        if sH.canonicalName == eH.canonicalName && eH.canonicalName == tH.canonicalName then
          (sH,eH,tH) match
            case (sHS: RefinedSegmentSpec, eHS: RefinedSegmentSpec, tHS: RefinedSegmentSpec) =>
              (sT, eT, tT, acc :+ compareTwoSegments(path, sHS, eHS, tHS))
            case (sHS: RefinedLoopSpec, eHS: RefinedLoopSpec, tHS: RefinedLoopSpec) =>
              (sT, eT, tT, acc :+ compareTwoLoops(path, sHS, eHS, tHS))
            case _ =>
              (Nil,Nil,Nil, acc :+ DifferenceError(path, s"Matching elements ${sH.canonicalName} have different major types (loop vs segment)"))

        // src has edi segment, target does not
        else if sH.canonicalName == eH.canonicalName && eH.canonicalName != tH.canonicalName then
          (sH, eH) match
            case (sHS: RefinedSegmentSpec, eHS: RefinedSegmentSpec) =>
              (sT,eT,target, acc :+ burnSrcSegment(path, sHS))
            case (sHS: RefinedLoopSpec, eHS: RefinedLoopSpec) =>
              (sT,eT,target,acc :+ burnSrcLoop(path, sHS))
            case _ =>
              (Nil,Nil,Nil, acc :+ DifferenceError(path, s"Source element ${sH.canonicalName} has a different major types (loop vs segment) than EDI standard"))

        // src does not have edi segment, target does
        else if sH.canonicalName != eH.canonicalName && eH.canonicalName == tH.canonicalName then
          (eH, tH) match
            case (eHS: RefinedSegmentSpec, tHS: RefinedSegmentSpec) =>
              (src,eT,tT, acc :+ burnTargetSegment(path, tHS))
            case (eHS: RefinedLoopSpec, tHS: RefinedLoopSpec) =>
              (src,eT,tT,acc :+ burnTargetLoop(path, tHS))
            case _ =>
              (Nil,Nil,Nil, acc :+ DifferenceError(path, s"Target element ${tH.canonicalName} has a different major types (loop vs segment) than EDI standard"))

        // neither have edi segment
        else
          (src, eT, target, acc :+ SimpleSegmentDifference(path, eH.name, canonicalNameOf(eH), (false, false), (sH.required, tH.required), None, List.empty))

      case (Nil, eH :: eT, tH :: tT) =>
        (eH, tH) match
          case (eHS: RefinedSegmentSpec, tHS: RefinedSegmentSpec) =>
            (Nil,eT,tT, acc :+ burnTargetSegment(path, tHS))
          case (eHS: RefinedLoopSpec, tHS: RefinedLoopSpec) =>
            (Nil,eT,tT,acc :+ burnTargetLoop(path, tHS))
          case _ =>
            (Nil,Nil,Nil, acc :+ DifferenceError(path, s"Target element ${tH.canonicalName} has a different major types (loop vs segment) than EDI standard"))
      case (sH :: sT, eH :: eT, Nil) =>
        (sH, eH) match
          case (sHS: RefinedSegmentSpec, eHS: RefinedSegmentSpec) =>
            (sT,eT,Nil, acc :+ burnSrcSegment(path, sHS))
          case (sHS: RefinedLoopSpec, eHS: RefinedLoopSpec) =>
            (sT,eT,Nil,acc :+ burnSrcLoop(path, sHS))
          case _ =>
            (Nil,Nil,Nil, acc :+ DifferenceError(path, s"Source element ${sH.canonicalName} has a different major types (loop vs segment) than EDI standard"))
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
      src.name,
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
                              edi: RefinedSingleOrLoopSegmentSpec
                            ): SegmentDifference =
    SimpleSegmentDifference(
      path,
      edi.name,
      canonicalNameOf(edi),
      (false, false),
      (edi.required, false),
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
                                  src: RefinedSegmentSpec
                                ): SegmentDifference =
    SimpleSegmentDifference(
      path,
      src.name,
      src.canonicalName,
      (true,false),
      (src.required, false),
      None,
      burnSegmentFields(path.dot(src.canonicalName), src.fields, true)
    )

  // src exists, target not
  private def burnTargetSegment(
                              path: Path,
                              target: RefinedSegmentSpec
                            ): SegmentDifference =
    SimpleSegmentDifference(
      path,
      target.name,
      target.canonicalName,
      (false,true),
      (false, target.required),
      None,
      burnSegmentFields(path.dot(target.canonicalName), target.fields, false)
    )

  private def burnSrcLoop(
                                 path: Path,
                                 src: RefinedLoopSpec
                               ): SegmentDifference =
    LoopSegmentDifference(
      path,
      src.name,
      src.canonicalName,
      (true, false),
      (src.required, false),
      None,
      burnSegmentFields(path.dot(src.canonicalName), src.fields, true),
      None,
      None,
      List.empty,
      None
    )

  private def burnTargetLoop(
                              path: Path,
                              target: RefinedLoopSpec
                            ): SegmentDifference =
    LoopSegmentDifference(
      path,
      target.name,
      target.canonicalName,
      (false, true),
      (false, target.required),
      None,
      burnSegmentFields(path.dot(target.canonicalName), target.fields, false),
      None,
      None,
      List.empty,
      None
    )

  @tailrec
  private def compareSegmentFields(
                                    path: Path,
                                    src: List[RefinedFieldSpec],
                                    edi: List[RefinedFieldSpec],
                                    target: List[RefinedFieldSpec],
                                    acc: List[FieldDifference] = List.empty
                                  ): List[FieldDifference] =
    val nextLoop = (src, edi, target) match {
      case (_, Nil, _ :: _) | (_ :: _, Nil, _) =>
        (Nil, Nil, Nil, acc :+ FieldDifferenceError(path, "Exhausted EDI standard fields before either src/target--they have extra (non-standard) fields"))
      case (Nil, Nil, Nil) =>
        (Nil, Nil, Nil, acc) // done comparing lists
      case (sH :: sT, eH :: eT, tH :: tT) =>
        // 3-way match -> compare sH and tH
        if sH.canonicalName == eH.canonicalName && eH.canonicalName == tH.canonicalName then
          (sH, eH, tH) match
            case (sHS: RefinedSingleFieldSpec, eHS: RefinedSingleFieldSpec, tHS: RefinedSingleFieldSpec) =>
              (sT, eT, tT, acc :+ compareTwoSingleFields(path, sHS, tHS))
            case (sHS: RefinedCompositeFieldSpec, eHS: RefinedCompositeFieldSpec, tHS: RefinedCompositeFieldSpec) =>
              (sT, eT, tT, acc :+ compareTwoCompositeFields(path, sHS, eHS, tHS))
            case _ =>
              (Nil, Nil, Nil, acc :+ FieldDifferenceError(path, s"Matching fields ${sH.name} have different major types (simple vs composite)"))
        else if sH.canonicalName == eH.canonicalName && eH.canonicalName != tH.canonicalName then
          (sH, eH, tH) match
            case (sHS: RefinedSingleFieldSpec, eHS: RefinedSingleFieldSpec, tHS: RefinedSingleFieldSpec) =>
              (sT, eT, target, acc :+ burnSingleField(path, sHS, true))
            case (sHS: RefinedCompositeFieldSpec, eHS: RefinedCompositeFieldSpec, tHS: RefinedCompositeFieldSpec) =>
              (sT, eT, target, acc :+ burnCompositeField(path, sHS, true))
            case _ =>
              (Nil, Nil, Nil, acc :+ FieldDifferenceError(path, s"Matching fields ${sH.name} have different major types (simple vs composite)"))
        else if sH.canonicalName != eH.canonicalName && eH.canonicalName == tH.canonicalName then
          (sH, eH, tH) match
            case (sHS: RefinedSingleFieldSpec, eHS: RefinedSingleFieldSpec, tHS: RefinedSingleFieldSpec) =>
              (src, eT, tT, acc :+ burnSingleField(path, tHS, false))
            case (sHS: RefinedCompositeFieldSpec, eHS: RefinedCompositeFieldSpec, tHS: RefinedCompositeFieldSpec) =>
              (src, eT, tT, acc :+ burnCompositeField(path, tHS, false))
            case _ =>
              (Nil, Nil, Nil, acc :+ FieldDifferenceError(path, s"Matching fields ${sH.name} have different major types (simple vs composite)"))
        else
          (src, eT, target, acc :+ SingleFieldDifference(path, eH.name, eH.canonicalName, (false, false), (sH.required, tH.required), None, None))
      case (Nil, eH :: eT, tH :: tT) =>
        (eH, tH) match
          case (eHS: RefinedSingleFieldSpec, tHS: RefinedSingleFieldSpec) =>
            (Nil,eT,tT, acc :+ burnSingleField(path, tHS, false))
          case (eHS: RefinedCompositeFieldSpec, tHS: RefinedCompositeFieldSpec) =>
            (Nil,eT,tT,acc :+ burnCompositeField(path, tHS, false))
          case _ =>
            (Nil,Nil,Nil, acc :+ FieldDifferenceError(path, s"Target field ${tH.name} has a different major types (loop vs segment) than EDI standard"))
      case (sH :: sT, eH :: eT, Nil) =>
        (sH, eH) match
          case (sHS: RefinedSingleFieldSpec, eHS: RefinedSingleFieldSpec) =>
            (sT,eT,Nil, acc :+ burnSingleField(path, sHS, true))
          case (sHS: RefinedCompositeFieldSpec, eHS: RefinedCompositeFieldSpec) =>
            (sT,eT,Nil,acc :+ burnCompositeField(path, sHS, true))
          case _ =>
            (Nil,Nil,Nil, acc :+ FieldDifferenceError(path, s"Source element ${sH.name} has a different major types (loop vs segment) than EDI standard"))
      case (Nil, eH :: eT, Nil) =>
        (Nil, eT, Nil, acc :+ burnEdiField(path, eH))
    }
    nextLoop match {
      case (Nil,Nil,Nil,nextAcc) =>
        nextAcc
      case (s,e,t,nextAcc) =>
        compareSegmentFields(path, s, e, t, nextAcc)
    }


  private def compareTwoCompositeFields(path: Path, src: RefinedCompositeFieldSpec, edi: RefinedCompositeFieldSpec, target: RefinedCompositeFieldSpec): FieldDifference =
    CompositeFieldDifference(
      path,
      src.name,
      src.canonicalName,
      (true,true),
      (src.required, target.required),
      compareSegmentFields(path.dot(src.canonicalName), src.components, edi.components, target.components)
    )

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

  private def burnSegmentFields(
                              path: Path,
                              doomed: List[RefinedFieldSpec],
                              isSrc: Boolean
                            ): List[FieldDifference] =
    doomed.map {
      case f: RefinedSingleFieldSpec => burnSingleField(path, f, isSrc)
      case f: RefinedCompositeFieldSpec => burnCompositeField(path, f, isSrc)
    }

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

  private def burnCompositeField(path: Path, f: RefinedCompositeFieldSpec, isSrc: Boolean): FieldDifference =
    CompositeFieldDifference(
      path,
      f.name,
      f.canonicalName,
      {
        if isSrc then (true, false) else (false, true)
      },
      {
        if isSrc then (f.required, false) else (false, f.required)
      },
      List.empty
    )

  private def burnEdiField(path: Path, f: RefinedFieldSpec): FieldDifference =
    SingleFieldDifference(
      path,
      f.name,
      f.canonicalName,
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

  private def burnSrcHL(path: Path, src: RefinedLoopSpec): LoopSegmentDifference =
    LoopSegmentDifference(
      path,
      src.name,
      canonicalNameOf(src),
      (true, false),
      (src.required, false),
      None,
      burnSegmentFields(path, src.fields, false),
      None,
      None,
      src.body.map {
        case bd: RefinedSegmentSpec => burnTargetSegment(path, bd)
        case bd: RefinedLoopSpec => burnTargetLoop(path, bd)
      },
      None
    )

  private def burnTargetHL(path: Path, target: RefinedLoopSpec): LoopSegmentDifference =
    LoopSegmentDifference(
      path,
      target.name,
      canonicalNameOf(target),
      (false, true),
      (false, target.required),
      None,
      burnSegmentFields(path, target.fields, false),
      None,
      None,
      target.body.map {
        case bd: RefinedSegmentSpec => burnTargetSegment(path, bd)
        case bd: RefinedLoopSpec => burnTargetLoop(path, bd)
      },
      None
    )

  private def nestedCompare(path: Path, src: Option[RefinedLoopSpec], edi: RefinedLoopSpec, target: Option[RefinedLoopSpec]): List[LoopSegmentDifference] = {
//    println("Comparing nested loop: " + src.map(s=>canonicalNameOf(s)).getOrElse("none")+" and "+target.map(s=>canonicalNameOf(s)).getOrElse("none"))
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
      case (None, Some(tt)) =>
//        println("Source missing "+canonicalNameOf(tt))
        loop(None, tt.nested, result :+ burnTargetHL(path.nest(canonicalNameOf(tt)), tt))
      case (Some(xx), None) =>
//        println("Target missing "+canonicalNameOf(xx))
        loop(xx.nested, None, result :+ burnSrcHL(path.nest(canonicalNameOf(xx)), xx))
      case (Some(xx), Some(yy)) if canonicalNameOf(xx) == canonicalNameOf(yy) =>
//        println("Match: "+canonicalNameOf(xx)+"/"+canonicalNameOf(yy))
        result :+ compareTwoLoops(path, xx, edi, yy)
      case (Some(xx), Some(_)) =>
        val (found, acc) = scanTarget(y, canonicalNameOf(xx), Nil)
        if found then
          val y2 = advanceTo(y, canonicalNameOf(xx)).flatMap(_.nested)
//          println("Match: " + canonicalNameOf(xx) + "/" + canonicalNameOf(y2.get))
          result :+ compareTwoLoops(path, xx, edi, y2.get)
        else
//          println("Target missing "+canonicalNameOf(xx))
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

  private def canonicalNameOf(x: RefinedSingleOrLoopSegmentSpec): String = x match {
    case s: RefinedSegmentSpec => s.canonicalName
    case l: RefinedLoopSpec =>
      if l.canonicalName == "HL" && l.description.nonEmpty then
        l.canonicalName + s"[${l.description}]"
      else
        l.canonicalName
  }


  // Final patch step for detectAndPatchMissingHL
  private def patchMissingHLIntoTarget(
                                        target: RefinedDocumentSpec,
                                        beforeDesc: String,
                                        hl03Value: String,
                                        missingDesc: String,
                                      ): RefinedDocumentSpec = {

    def findHLInSpec(desc: String, loop: RefinedLoopSpec): Option[RefinedLoopSpec] =
      if loop.canonicalName == "HL" && loop.description == desc then Some(loop)
      else loop.nested.flatMap(findHLInSpec(desc, _))

    // Patch a specific HL loop by sewing in a clone with modified HL03 value
    def patchLoop(loop: RefinedLoopSpec): RefinedLoopSpec =
      findHLInSpec(beforeDesc, loop) match
        case Some(before) =>
          before.nested match
            case Some(originalChild) =>
              val patchedChild = {
                val newFields = originalChild.fields.map {
                  case f: RefinedSingleFieldSpec if f.canonicalName == "HL03" =>
                    f.copy(validValues = List(hl03Value))
                  case other => other
                }
                originalChild.copy(description = missingDesc, fields = newFields, nested = Some(originalChild))
              }
              val patchedBefore = before.copy(nested = Some(patchedChild))

              // Reconstruct the full chain from top using replacement
              def walk(current: RefinedLoopSpec): RefinedLoopSpec =
                if current eq before then patchedBefore
                else current.copy(nested = current.nested.map(walk))

              walk(loop)

            case None =>
              println(s"[WARN] No nested HL under HL[$beforeDesc] to clone.")
              loop
        case None =>
          println(s"[WARN] Could not locate HL[$beforeDesc] in target spec")
          loop

    // Apply patch only to the top-level HL loop, leave everything else alone
    val patchedSegments = target.segments.map {
      case hl: RefinedLoopSpec if hl.canonicalName == "HL" =>
        patchLoop(hl)
      case other => other
    }

    target.copy(segments = patchedSegments)
  }


  // Returns either original or patched target
  def detectAndPatchMissingHL(
                               src: RefinedDocumentSpec,
                               differences: List[SegmentDifference],
                               target: RefinedDocumentSpec
                             ): Option[RefinedDocumentSpec] = {

    @tailrec
    def findSurroundingHL(desc: String, loop: RefinedLoopSpec): Option[(RefinedLoopSpec, RefinedLoopSpec)] =
      loop.nested match
        case Some(nested) if nested.description == desc =>
          nested.nested.map(after => (loop, after))
        case Some(nested) =>
          findSurroundingHL(desc, nested)
        case None =>
          None

    // Step 1: Detect missing HL levels
    def collectMissingHLs(diffs: List[SegmentDifference]): List[LoopSegmentDifference] =
      diffs.flatMap {
        case hl: LoopSegmentDifference if hl.canonicalName.startsWith("HL[") =>
          val bodyMissing = collectMissingHLs(hl.bodyDiff)
          val nestedMissing = hl.nested.toList.flatten.flatMap(n => collectMissingHLs(List(n)))
          val self = if !hl.presence._2 then List(hl) else Nil
          self ++ bodyMissing ++ nestedMissing
        case _ => Nil
      }

    def findHLInSpec(desc: String, loop: RefinedLoopSpec): Option[RefinedLoopSpec] = {
      if loop.canonicalName == "HL" && loop.description == desc then Some(loop)
      else loop.nested.flatMap(findHLInSpec(desc, _))
    }

    collectMissingHLs(differences).headOption.flatMap { missingHL =>
      val desc = missingHL.canonicalName.stripPrefix("HL[").stripSuffix("]")

      for {
        loop <- src.segments.collectFirst { case l: RefinedLoopSpec => findHLInSpec(desc, l) }.flatten
        loopDesc = loop.description
        hl03Field <- loop.fields.collectFirst { case f: RefinedSingleFieldSpec if f.canonicalName == "HL03" => f }
        hl03Value <- hl03Field.validValues.headOption
        topHL <- src.segments.collectFirst { case l: RefinedLoopSpec if l.canonicalName == "HL" => l }
        (before, after) <- findSurroundingHL(desc, topHL)
        targetTopHL <- target.segments.collectFirst { case l: RefinedLoopSpec if l.canonicalName == "HL" => l }
        afterHLInTarget <- findHLInSpec(after.description, targetTopHL)
        targetHL03Field <- afterHLInTarget.fields.collectFirst { case f: RefinedSingleFieldSpec if f.canonicalName == "HL03" => f }
        if targetHL03Field.validValues.size > 1 && targetHL03Field.validValues.contains(hl03Value)
      } yield patchMissingHLIntoTarget(target, before.description, hl03Value, loopDesc)

    }
  }