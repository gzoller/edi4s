package co.blocke.edi4s
package diff

import co.blocke.edi4s.model.*
import pprint.*

import zio.*
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
                  ): ZIO[Any, DifferenceError, List[SegmentDifference]] =
    compareSegmentLists(Path(), src.segments, edi.segments, target.segments)

  /*
    This logic is admittedly a little convoluted. Both src and target are based on a canonical edi spec, however... both can
    go "shopping" in the canonical spec and pull segments they want to use and ignore others. So they are both perfect
    subsets, but they may well be *different* subsets to one another. Therefor we must use the canonical spec as our
    "north star" to be a concrete reference for the known segments and their ordering.
   */
  private def compareSegmentLists(
                           path: Path,
                           src: List[RefinedSingleOrLoopSegmentSpec],
                           edi: List[RefinedSingleOrLoopSegmentSpec],
                           target: List[RefinedSingleOrLoopSegmentSpec],
                           acc: List[SegmentDifference] = List.empty
  ): ZIO[Any, DifferenceError, List[SegmentDifference]] =
    (src, edi, target) match {

      // ===== Ran out of EDI segments before we ran out of src/target segmens. This should never happen.
      case (_, Nil, _ :: _) | (_ :: _, Nil, _) =>
        ZIO.fail(DifferenceError("Exhausted EDI standard segments before either src/target--they have extra (non-standard) segments"))

      // =====  All done.
      case (Nil, Nil, Nil) =>
        ZIO.succeed(acc) // done comparing lists

      // =====  Normative case: we have some segment available to compare for src/edi/target
      case (sH :: sT, eH :: eT, tH :: tT) =>
        // 3-way match -> compare sH and tH directly, like-for-like
        if sH.canonicalName == eH.canonicalName && eH.canonicalName == tH.canonicalName then
          (sH,eH,tH) match
            case (sHS: RefinedSegmentSpec, eHS: RefinedSegmentSpec, tHS: RefinedSegmentSpec) =>
              // We need the for loop drama here b/c compareTwoLoops returns a ZIO--may fail with an error if loop is incompatible HL
              for {
                loopCompare <- compareTwoSegments(path, sHS, eHS, tHS)
                nextRecursion <- compareSegmentLists(path, sT, eT, tT, acc :+ loopCompare)
              } yield nextRecursion
            case (sHS: RefinedLoopSpec, eHS: RefinedLoopSpec, tHS: RefinedLoopSpec) =>
              // We need the for loop drama here b/c compareTwoLoops returns a ZIO--may fail with an error if loop is incompatible HL
              for {
                loopCompare <- compareTwoLoops(path, sHS, eHS, tHS)
                nextRecursion <- compareSegmentLists(path, sT, eT, tT, acc :+ loopCompare)
              } yield nextRecursion
            case _ =>
              ZIO.fail( DifferenceError(s"Matching elements ${sH.canonicalName} have different major type (loop vs segment)"))

        // src has edi segment, target does not (advance src and edi but leave target unchanged)
        else if sH.canonicalName == eH.canonicalName && eH.canonicalName != tH.canonicalName then
          (sH, eH) match
            case (sHS: RefinedSegmentSpec, eHS: RefinedSegmentSpec) =>
              compareSegmentLists( path, sT, eT, target, acc :+ burnSrcSegment(path, sHS))
            case (sHS: RefinedLoopSpec, eHS: RefinedLoopSpec) =>
              compareSegmentLists( path, sT, eT, target, acc :+ burnSrcLoop(path, sHS))
            case _ =>
              ZIO.fail(DifferenceError(s"Source element ${sH.canonicalName} has a different major type (loop vs segment) than EDI standard"))

        // src does not have edi segment, target does (advance target and edi but leave src unchanged)
        else if sH.canonicalName != eH.canonicalName && eH.canonicalName == tH.canonicalName then
          (eH, tH) match
            case (eHS: RefinedSegmentSpec, tHS: RefinedSegmentSpec) =>
              compareSegmentLists( path, src, eT, tT, acc :+ burnTargetSegment(path, tHS))
            case (eHS: RefinedLoopSpec, tHS: RefinedLoopSpec) =>
              compareSegmentLists( path, src, eT, tT, acc :+ burnTargetLoop(path, tHS))
            case _ =>
              ZIO.fail(DifferenceError(s"Target element ${tH.canonicalName} has a different major type (loop vs segment) than EDI standard"))

        // neither have edi segment
        else
          compareSegmentLists( path, src, eT, target, acc :+ SimpleSegmentDifference(path, eH.name, canonicalNameOf(eH), (false, false), (sH.required, tH.required), None, List.empty))

      // ===== Ran out of src segments but there are more target segments left
      case (Nil, eH :: eT, tH :: tT) =>
        (eH, tH) match
          case (eHS: RefinedSegmentSpec, tHS: RefinedSegmentSpec) if eHS.canonicalName == tHS.canonicalName =>
            compareSegmentLists( path, Nil, eT, tT, acc :+ burnTargetSegment(path, tHS))
          case (eHS: RefinedLoopSpec, tHS: RefinedLoopSpec) if eHS.canonicalName == tHS.canonicalName  =>
            compareSegmentLists( path, Nil, eT, tT, acc :+ burnTargetLoop(path, tHS))
          case _ => // no match--advance edi and try again
            compareSegmentLists( path, Nil, eT, target, acc)

      // ===== Ran out of target segments but there are more src segments left
      case (sH :: sT, eH :: eT, Nil) =>
        (sH, eH) match
          case (sHS: RefinedSegmentSpec, eHS: RefinedSegmentSpec) if sHS.canonicalName == eHS.canonicalName =>
            compareSegmentLists( path, sT, eT, Nil, acc :+ burnSrcSegment(path, sHS))
          case (sHS: RefinedLoopSpec, eHS: RefinedLoopSpec) if sHS.canonicalName == eHS.canonicalName =>
            compareSegmentLists( path, sT, eT, Nil, acc :+ burnSrcLoop(path, sHS))
          case _ => // no match--advance edi and try again
            compareSegmentLists( path, src, eT, Nil, acc)

      // ===== Ran out of src *and* target segments but there are more edi segments left
      case (Nil, eH :: eT, Nil) =>
        compareSegmentLists( path, Nil, eT, Nil, acc :+ burnEdiSegment(path, eH) )
    }


  // So this 2-phase drama is to account for src or target spec re-writing in the cases of FlattenSrcLevel or PromoteTargetLevel, respectively.
  // Once we settle on the final specs we can then call part2 to do the real comparison, like-for-like. Of course there is a chance we can't
  // get src/target HL hierarchies to align, in which case an error is returned.
  private def compareTwoLoops(
                               path: Path,
                               src: RefinedLoopSpec,
                               edi: RefinedLoopSpec,
                               target: RefinedLoopSpec
                             ): ZIO[Any, DifferenceError, LoopSegmentDifference] =

    if src.canonicalName == "HL" then
      val hlSrc = DiffUtil.getHLLevels(src)
      val hlTarget = DiffUtil.getHLLevels(target)

      for {
        hlRule <- DiffUtil.analyzeHLStructures(hlSrc, hlTarget)
        result <- hlRule match
          case promo: PromoteTargetLevel =>
            for {
              promotedTarget <- DiffUtil.promoteHLLevel(src, target, promo)
              result <- compareTwoLoops_part2(path, src, edi, promotedTarget, Some(hlRule))
            } yield result

          case _ =>
            compareTwoLoops_part2(path, src, edi, target, Some(hlRule))
      } yield result

    else
      compareTwoLoops_part2(path, src, edi, target)


  private def compareTwoLoops_part2(
                               path: Path,
                               src: RefinedLoopSpec,
                               edi: RefinedLoopSpec,
                               target: RefinedLoopSpec,
                               hlRule: Option[HLSpecRule] = None): ZIO[Any, DifferenceError, LoopSegmentDifference] =
    // For HL loops we need to compare the nesting hierarchy and ensure it is compatible, and if so, issue a transformation rule
    for {
      bodyDiff <- compareSegmentLists(path.dot(canonicalNameOf(src)), src.body, edi.body, target.body)
      nestedDiffOpt <- (src.nested, target.nested) match
        case (Some(srcNext), Some(targetNext)) =>
          compareTwoLoops_part2(path, srcNext, edi, targetNext).map(Some(_))
        case (None, None) =>
          ZIO.succeed(None)
        case _ =>
          ZIO.fail(DifferenceError("HL nesting mismatch after alignment"))
      fieldDiff <- compareSegmentFields(path.dot(src.canonicalName), src.fields, edi.fields, target.fields)
    } yield LoopSegmentDifference(
      path = path,
      name = src.name,
      canonicalName = canonicalNameOf(src),
      presence = (true, canonicalNameOf(target) == canonicalNameOf(src)),
      required = (src.required, target.required),
      assertions = Option.when(src.assertions.sorted != target.assertions.sorted)(
        (src.assertions, target.assertions)
      ),
      fieldDiff = fieldDiff,
      minDiff = Option.when(src.minRepeats != target.minRepeats)(src.minRepeats, target.minRepeats),
      maxDiff = Option.when(src.maxRepeats != target.maxRepeats)(src.maxRepeats, target.maxRepeats),
      bodyDiff = bodyDiff,
      hlRule = hlRule,
      hlDiscriminator = getHLdiscriminator(target),
      nested = nestedDiffOpt
    )


  private def compareTwoSegments(
                                path: Path,
                                src: RefinedSegmentSpec,
                                edi: RefinedSegmentSpec,
                                target: RefinedSegmentSpec
                                ): ZIO[Any, DifferenceError, SegmentDifference] =
    compareSegmentFields(path.dot(src.canonicalName), src.fields, edi.fields, target.fields)
      .flatMap( fieldDiff =>
        ZIO.succeed(SimpleSegmentDifference(
          path,
          src.name,
          src.canonicalName,
          (true,true),
          (src.required, target.required),
          Option.when(src.assertions.sorted != target.assertions.sorted)(
            (src.assertions, target.assertions)
          ),
          fieldDiff
        ))
  )

  // Used when there are no mor src or target segments but here are unvisited edi segments.
  // We then create differences for each remaining edi segment noting they are not present,
  // and noting their required status accordingly.
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
      None,
      hlDiscriminator = getHLdiscriminator(src)
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
      None,
      hlDiscriminator = getHLdiscriminator(target)
    )

  private def compareSegmentFields(
                                   path: Path,
                                   src: List[RefinedFieldSpec],
                                   edi: List[RefinedFieldSpec],
                                   target: List[RefinedFieldSpec],
                                   acc: List[FieldDifference] = List.empty
                                 ): ZIO[Any, DifferenceError, List[FieldDifference]] =
    (src, edi, target) match {
      case (_, Nil, _ :: _) | (_ :: _, Nil, _) =>
        ZIO.fail(DifferenceError("Exhausted EDI standard fields before either src/target--they have extra (non-standard) fields"))

      case (Nil, Nil, Nil) =>
        ZIO.succeed(acc)

      case (sH :: sT, eH :: eT, tH :: tT) =>
        // 3-way match -> compare sH and tH
        if sH.canonicalName == eH.canonicalName && eH.canonicalName == tH.canonicalName then
          (sH, eH, tH) match
            case (sHS: RefinedSingleFieldSpec, eHS: RefinedSingleFieldSpec, tHS: RefinedSingleFieldSpec) =>
              compareSegmentFields( path, sT, eT, tT, acc :+ compareTwoSingleFields(path, sHS, tHS))
            case (sHS: RefinedCompositeFieldSpec, eHS: RefinedCompositeFieldSpec, tHS: RefinedCompositeFieldSpec) =>
              // We need the for loop drama here b/c compareTwoLoops returns a ZIO--may fail with an error if loop is incompatible HL
              for {
                loopCompare <- compareTwoCompositeFields(path, sHS, eHS, tHS)
                updatedAcc = acc :+ loopCompare
                nextRecursion <- compareSegmentFields(path, sT, eT, tT, updatedAcc)
              } yield nextRecursion
            case _ =>
              ZIO.fail( DifferenceError(s"Matching fields ${sH.name} have different major types (simple vs composite)"))

        else if sH.canonicalName == eH.canonicalName && eH.canonicalName != tH.canonicalName then
          (sH, eH, tH) match
            case (sHS: RefinedSingleFieldSpec, eHS: RefinedSingleFieldSpec, tHS: RefinedSingleFieldSpec) =>
              compareSegmentFields( path, sT, eT, target, acc :+ burnSingleField(path, sHS, true))
            case (sHS: RefinedCompositeFieldSpec, eHS: RefinedCompositeFieldSpec, tHS: RefinedCompositeFieldSpec) =>
              compareSegmentFields( path, sT, eT, target, acc :+ burnCompositeField(path, sHS, true))
            case _ =>
              ZIO.fail( DifferenceError(s"Matching fields ${sH.name} have different major types (simple vs composite)"))

        else if sH.canonicalName != eH.canonicalName && eH.canonicalName == tH.canonicalName then
          (sH, eH, tH) match
            case (sHS: RefinedSingleFieldSpec, eHS: RefinedSingleFieldSpec, tHS: RefinedSingleFieldSpec) =>
              compareSegmentFields( path, src, eT, tT, acc :+ burnSingleField(path, tHS, false))
            case (sHS: RefinedCompositeFieldSpec, eHS: RefinedCompositeFieldSpec, tHS: RefinedCompositeFieldSpec) =>
              compareSegmentFields( path, src, eT, tT, acc :+ burnCompositeField(path, tHS, false))
            case _ =>
              ZIO.fail( DifferenceError(s"Matching fields ${sH.name} have different major types (simple vs composite)"))

        else
          compareSegmentFields( path, src, eT, target, acc :+ SingleFieldDifference(path, eH.name, eH.canonicalName, (false, false), (sH.required, tH.required), None, None))

      case (Nil, eH :: eT, tH :: tT) =>
        (eH, tH) match
          case (eHS: RefinedSingleFieldSpec, tHS: RefinedSingleFieldSpec) =>
            compareSegmentFields( path, Nil, eT, tT, acc :+ burnSingleField(path, tHS, false))
          case (eHS: RefinedCompositeFieldSpec, tHS: RefinedCompositeFieldSpec) =>
            compareSegmentFields( path, Nil, eT, tT, acc :+ burnCompositeField(path, tHS, false))
          case _ =>
            ZIO.fail( DifferenceError(s"Target field ${tH.name} has a different major types (loop vs segment) than EDI standard"))

      case (sH :: sT, eH :: eT, Nil) =>
        (sH, eH) match
          case (sHS: RefinedSingleFieldSpec, eHS: RefinedSingleFieldSpec) =>
            compareSegmentFields( path, sT, eT, Nil, acc :+ burnSingleField(path, sHS, true))
          case (sHS: RefinedCompositeFieldSpec, eHS: RefinedCompositeFieldSpec) =>
            compareSegmentFields( path, sT, eT, Nil, acc :+ burnCompositeField(path, sHS, true))
          case _ =>
            ZIO.fail( DifferenceError(s"Source field ${sH.name} has a different major types (loop vs segment) than EDI standard"))

      case (Nil, eH :: eT, Nil) =>
        compareSegmentFields( path, Nil, eT, Nil, acc :+ burnEdiField(path, eH))
    }


  private def compareTwoCompositeFields(
                                         path: Path,
                                         src: RefinedCompositeFieldSpec,
                                         edi: RefinedCompositeFieldSpec,
                                         target: RefinedCompositeFieldSpec
                                       ): ZIO[Any, DifferenceError, FieldDifference] =
    compareSegmentFields(path.dot(src.canonicalName), src.components, edi.components, target.components)
      .flatMap( fieldDiff =>
        ZIO.succeed(CompositeFieldDifference(
          path,
          src.name,
          src.canonicalName,
          (true,true),
          (src.required, target.required),
          fieldDiff
        ))
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
      {
        if f.validValues == Nil then None
        else if isSrc then
          Some((f.validValues,Nil))
        else
          Some((Nil,f.validValues))
      }
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
      None,
      hlDiscriminator = getHLdiscriminator(src),
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
      None,
      hlDiscriminator = getHLdiscriminator(target)
    )


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

  private inline def getHLdiscriminator( loop: RefinedLoopSpec ): Option[String] = {
    Option.when(loop.canonicalName == "HL"){
      loop.fields.find(_.canonicalName == "HL03").map(_.asInstanceOf[RefinedSingleFieldSpec].validValues.mkString(","))}.flatten
  }
