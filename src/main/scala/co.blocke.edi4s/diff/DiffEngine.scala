package co.blocke.edi4s
package diff

import co.blocke.edi4s.model.*
import pprint.*
import scala.annotation.tailrec

import zio.*
import Availability.*

object DiffEngine:

  //
  //  TOP-LEVEL
  //
  def compareSpecs(
                    src: RefinedDocumentSpec,
                    edi: RefinedDocumentSpec,
                    target: RefinedDocumentSpec
                  ): ZIO[Any, DifferenceError, List[SegmentDifference]] =
    compareSegmentLists(src.segments, edi.segments, target.segments)

  private def compareSegmentLists(
                           src: List[RefinedSingleOrLoopSegmentSpec],
                           edi: List[RefinedSingleOrLoopSegmentSpec],
                           target: List[RefinedSingleOrLoopSegmentSpec],
                           acc: List[SegmentDifference] = List.empty
                         ): ZIO[Any, DifferenceError, List[SegmentDifference]] =

    (src, edi, target) match {
      // All lists traversed -- Done!
      case (Nil, Nil, Nil) =>
        ZIO.succeed(acc)

      // src+target exhausted, more edi
      case (Nil, eH :: eT, Nil) =>
        eH match {
          case e: RefinedSegmentSpec =>
            compareSegmentLists(src, eT, target, acc :+ SingleSegmentDifference(eH.name, eH.canonicalName, (MISSING,MISSING)))
          case e: RefinedLoopSpec =>
            compareSegmentLists(src, eT, target, acc :+ LoopSegmentDifference(eH.name, eH.canonicalName, (MISSING, MISSING)))
        }

      // edi exhausted, more src or target --> ERROR!
      case (_, Nil, _) => ZIO.fail(DifferenceError("Canonical spec exhausted while there were still more (\"extra\") src or target segments."))

      // More src+edi, target exhausted
      case (sH :: sT, eH :: eT, Nil) =>
        val srcAvail = if sH.required then REQUIRED else OPTIONAL
        val nextEdi = if sH.canonicalName == eH.canonicalName then eT else edi
        eH match {
          case e: RefinedSegmentSpec =>
            compareSegmentLists(sT, nextEdi, target, acc :+ SingleSegmentDifference(eH.name, eH.canonicalName, (srcAvail,MISSING)))
          case e: RefinedLoopSpec =>
            makeLoopLabel(eH.canonicalName, sH.fields).flatMap { loopLabel =>
              compareSegmentLists(sT, nextEdi, target, acc :+ LoopSegmentDifference(eH.name, loopLabel, (srcAvail, MISSING)))
            }
        }

      // More edi+target, src exhausted
      case (Nil, eH :: eT, tH :: tT) =>
        val targetAvail = if tH.required then REQUIRED else OPTIONAL
        // TODO: Error caused by tracking increments improperly in the match statement.... Probably need to skip something...
        if eH.canonicalName == tH.canonicalName then
          (eH, tH) match {
            case (e: RefinedSegmentSpec, _: RefinedSegmentSpec) =>
              for {
                (assertDiffs, _, _, fieldDiff) <- segmentDetailCompare((MISSING, targetAvail), None, e, tH)
                nextRecursion <- compareSegmentLists(src, eT, tT, acc :+ SingleSegmentDifference(eH.name, eH.canonicalName, (MISSING, targetAvail), assertDiffs, fieldDiff))
              } yield nextRecursion
            case (e: RefinedLoopSpec, t: RefinedLoopSpec) =>
              for {
                loopLabel <- makeLoopLabel(eH.canonicalName, tH.fields)
                (assertDiffs, minDiff, maxDiff, fieldDiff) <- segmentDetailCompare((MISSING, targetAvail), None, e, tH)
                bodyDiff <- compareSegmentLists(Nil, e.body, t.body)
                nextRecursion <- compareSegmentLists(src, eT, tT,
                  acc :+ LoopSegmentDifference(eH.name, loopLabel, (MISSING, targetAvail), bodyDiff, None, LevelsOk(), getHLdiscriminator(t), assertDiffs, fieldDiff, minDiff, maxDiff))
              } yield nextRecursion
            case (_, _) =>
              ZIO.fail(DifferenceError(s"Field types for ${eH.canonicalName} and ${tH.canonicalName} do not match."))
          }
        else
          eH match {
            case e: RefinedSegmentSpec =>
              compareSegmentLists(src, edi, tT, acc :+ SingleSegmentDifference(tH.name, tH.canonicalName, (MISSING, targetAvail)))
            case e: RefinedLoopSpec =>
              for {
                loopLabel <- makeLoopLabel(tH.canonicalName, tH.fields)
                nextRecursion <- compareSegmentLists(src, edi, tT, acc :+ LoopSegmentDifference(tH.name, loopLabel, (MISSING, targetAvail)))
              } yield nextRecursion
          }

      // Look for matches
      case (sH :: sT, eH :: eT, tH :: tT) =>
        val (avail, nextS, nextT, doCompare) = (sH.canonicalName, eH.canonicalName, tH.canonicalName) match {
          // Case 1: all 3 match -> process and increment all 3
          case (s,e,t) if s == e && e == t =>
            val srcAvail = if sH.required then REQUIRED else OPTIONAL
            val targetAvail = if tH.required then REQUIRED else OPTIONAL
            ( (srcAvail,targetAvail), sT, tT, true )
          // Case 2: src+edi match -> process and increment src+edi
          case (s,e,t) if s == e =>
            val srcAvail = if sH.required then REQUIRED else OPTIONAL
            ( (srcAvail,MISSING), sT, target, false )
          // Case 3: edi+target match -> process and increment edi+target
          case (s,e,t) if e == t =>
            val targetAvail = if tH.required then REQUIRED else OPTIONAL
            ( (MISSING,targetAvail), src, tT, false )
          // Case 4: none match -> no process and increment edi
          case (s,e,t) =>
            ( (MISSING,MISSING), src, target, false )
        }
        eH match {
          case e: RefinedSegmentSpec =>
            for {
              (assertDiffs, _, _, fieldDiff) <- segmentDetailCompare(avail, Some(sH), eH, tH)
              nextRecursion <- compareSegmentLists(nextS, eT, nextT, acc :+ SingleSegmentDifference(eH.name, eH.canonicalName, avail, assertDiffs, fieldDiff))
            } yield nextRecursion
          case e: RefinedLoopSpec =>
            if doCompare then
              (sH, tH) match {
                case (_s: RefinedLoopSpec, _t: RefinedLoopSpec) =>
                  for {
                    loopCompare <- compareTwoLoops(_s, e, _t)
                    (assertDiffs, minDiff, maxDiff, fieldDiff) <- segmentDetailCompare(avail, Some(_s), e, _t)
                    nextRecursion <- compareSegmentLists(nextS, eT, nextT, acc :+ loopCompare.copy(availability = avail, assertions = assertDiffs, minDiff = minDiff, maxDiff = maxDiff, fieldDiff = fieldDiff))
                  } yield nextRecursion
                case _ =>
                  ZIO.fail(DifferenceError("While src/target segments matched they were not both loop specs"))
              }
            else
              for {
                loopLabel <- makeLoopLabel(eH.canonicalName, sH.fields)
                (assertDiffs, minDiff, maxDiff, fieldDiff) <- segmentDetailCompare(avail, Some(sH), eH, tH)
                nextRecursion <- compareSegmentLists(nextS, eT, nextT, acc :+ LoopSegmentDifference(eH.name, loopLabel, avail, assertions = assertDiffs, minDiff = minDiff, maxDiff = maxDiff, fieldDiff = fieldDiff))
              } yield nextRecursion
        }
    }


  private def segmentDetailCompare(
                             avail: (Availability, Availability),
                             s: Option[RefinedSingleOrLoopSegmentSpec],
                             e: RefinedSingleOrLoopSegmentSpec,
                             t: RefinedSingleOrLoopSegmentSpec
                           ): ZIO[Any, DifferenceError, (Option[(List[String], List[String])], Option[(Option[Int], Option[Int])], Option[(Option[Int], Option[Int])], List[FieldDifference])] = {

    val (minDiff, maxDiff) = (s, t) match {
      case (Some(_s: RefinedLoopSpec), _t: RefinedLoopSpec) =>
        (
          Option.when(_s.minRepeats != _t.minRepeats)(_s.minRepeats, _t.minRepeats),
          Option.when(_s.maxRepeats != _t.maxRepeats)(_s.maxRepeats, _t.maxRepeats)
        )
      case (None, _t: RefinedLoopSpec) =>  // when only target is specified
        (
          Some((None, _t.minRepeats)),
          Some((None, _t.maxRepeats)),
        )
      case _ => (None, None)  // no min/max if not a loop
    }

    avail match {
      case (_, Availability.MISSING) =>
        ZIO.succeed((None, None, None, Nil))
      case _ =>
        val assertionsDiff = if avail._1 == MISSING then Some((Nil, t.assertions))
        else Option.when(s.get.assertions.sorted != t.assertions.sorted)((s.get.assertions, t.assertions)) // get is safe here is avail._1 != MISSING
        for {
          fieldDiffs <- compareSegmentFields(s.map(_.fields).getOrElse(Nil), e.fields, t.fields)
        } yield (assertionsDiff, minDiff, maxDiff, fieldDiffs)
    }
  }

  // Special handling of HL labels
  private def makeLoopLabel( loopCanName: String, fields: List[RefinedFieldSpec] ): ZIO[Any, DifferenceError, String] =
    if loopCanName != "HL"
      then ZIO.succeed(loopCanName)
    else
      fields.find(_.canonicalName == "HL03").map(_.asInstanceOf[RefinedSingleFieldSpec].validValues.mkString(",")) match {
        case None      => ZIO.fail(DifferenceError("No HL03 field given for HL loop"))
        case Some(hl3) => ZIO.succeed(s"HL[$hl3]")
      }


  // So this 2-phase drama is to account for src or target spec re-writing in the cases of FlattenSrcLevel or PromoteTargetLevel, respectively.
  // Once we settle on the final specs we can then call part2 to do the real comparison, like-for-like. Of course there is a chance we can't
  // get src/target HL hierarchies to align, in which case an error is returned.
  private def compareTwoLoops(
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
              result <- compareTwoLoops_part2(src, edi, promotedTarget, hlRule)
            } yield result

          case _ =>
            compareTwoLoops_part2(src, edi, target)
      } yield result

    else
      compareTwoLoops_part2(src, edi, target)

  private def compareTwoLoops_part2( s: RefinedLoopSpec, e: RefinedLoopSpec, t: RefinedLoopSpec, hlRule: HLSpecRule = LevelsOk() ): ZIO[Any, DifferenceError, LoopSegmentDifference] =
    // Make sure we're all LoopSpecs
    for {
      bodyDiffs <- compareSegmentLists(s.body, e.body, t.body)
      loopLabel <- makeLoopLabel( e.canonicalName, s.fields )
      nestedDiff <- (s.nested, t.nested) match
        case (Some(ns), Some(nt)) =>
          val srcAvail = if ns.required then REQUIRED else OPTIONAL
          val targetAvail = if nt.required then REQUIRED else OPTIONAL
          for {
            (assertDiffs, minDiff, maxDiff, fieldDiff) <- segmentDetailCompare((srcAvail, targetAvail), Some(ns), e, nt)
            nextRecursion <- compareTwoLoops(ns, e, nt).map(ld => Some(ld.copy(availability = (srcAvail,targetAvail), assertions = assertDiffs, minDiff = minDiff, maxDiff = maxDiff, fieldDiff = fieldDiff)))
          } yield nextRecursion
        case (None, None) =>
          ZIO.succeed(None)
        case _ =>
          ZIO.fail(DifferenceError(s"Mismatched nesting in loop $loopLabel"))
    } yield LoopSegmentDifference(e.name, loopLabel, (MISSING,MISSING), bodyDiffs, nestedDiff, hlRule, getHLdiscriminator(s))

  private inline def getHLdiscriminator(loop: RefinedLoopSpec): Option[String] = {
    Option.when(loop.canonicalName == "HL") {
      loop.fields.find(_.canonicalName == "HL03").map(_.asInstanceOf[RefinedSingleFieldSpec].validValues.mkString(","))
    }.flatten
  }

  // ------------------------ Everything above this line computes the differences between segments. Everything below this line computes differences in fields. -------------------

  private def compareSegmentFields(
                                  src: List[RefinedFieldSpec],
                                  edi: List[RefinedFieldSpec],
                                  target: List[RefinedFieldSpec],
                                  acc: List[FieldDifference] = List.empty
                                ): ZIO[Any, DifferenceError, List[FieldDifference]] =
    (src, edi, target) match {
      // All lists traversed -- Done!
      case (Nil, Nil, Nil) =>
        ZIO.succeed(acc)

      // src+target exhausted, more etl
      case (Nil, eH :: eT, Nil) =>
        eH match {
          case e: RefinedSingleFieldSpec =>
            compareSegmentFields(src, eT, target, acc :+ SingleFieldDifference(eH.name, eH.canonicalName, (MISSING, MISSING)))
          case e: RefinedCompositeFieldSpec =>
            compareSegmentFields(src, eT, target, acc :+ CompositeFieldDifference(eH.name, eH.canonicalName, (MISSING, MISSING), Nil))
        }

      // edi exhausted, more src or target --> ERROR!
      case (_, Nil, _) => ZIO.fail(DifferenceError("Canonical spec exhausted while there were still more (\"extra\") src or target fields."))

      // More src+edi, target exhausted
      case (sH :: sT, eH :: eT, Nil) =>
        val srcAvail = if sH.required then REQUIRED else OPTIONAL
        val nextEdi = if sH.canonicalName == eH.canonicalName then eT else edi
        eH match {
          case e: RefinedSingleFieldSpec =>
            compareSegmentFields(sT, nextEdi, target, acc :+ SingleFieldDifference(eH.name, eH.canonicalName, (srcAvail, MISSING)))
          case e: RefinedCompositeFieldSpec =>
            compareSegmentFields(sT, nextEdi, target, acc :+ CompositeFieldDifference(eH.name, eH.canonicalName, (srcAvail, MISSING), Nil))
        }

      // More edi+target, src exhausted
      case (Nil, eH :: eT, tH :: tT) =>
        val targetAvail = if tH.required then REQUIRED else OPTIONAL
        if tH.canonicalName == eH.canonicalName then
          (eH, tH) match {
            case (e: RefinedSingleFieldSpec, _: RefinedSingleFieldSpec) =>
              compareSegmentFields(src, eT, tT, acc :+ SingleFieldDifference(eH.name, eH.canonicalName, (MISSING, targetAvail)))
            case (e: RefinedCompositeFieldSpec, t: RefinedCompositeFieldSpec) =>
              for {
                componentFieldDiffs <- compareSegmentFields( src, e.components, tH.asInstanceOf[RefinedCompositeFieldSpec].components)
                nextRecursion <- compareSegmentFields(src, eT, tT, acc :+ CompositeFieldDifference(eH.name, eH.canonicalName, (MISSING, targetAvail), componentFieldDiffs))
              } yield nextRecursion
            case (_,_) =>
              ZIO.fail(DifferenceError(s"Field types for ${eH.canonicalName} and ${tH.canonicalName} do not match."))
          }
        else
          compareSegmentFields(src, edi, tT, acc :+ SingleFieldDifference(eH.name, eH.canonicalName, (MISSING, targetAvail)))

      // Look for matches
      case (sH :: sT, eH :: eT, tH :: tT) =>
        val (avail, nextS, nextT, doCompare) = (sH.canonicalName, eH.canonicalName, tH.canonicalName) match {
          // Case 1: all 3 match -> process and increment all 3
          case (s, e, t) if s == e && e == t =>
            val srcAvail = if sH.required then REQUIRED else OPTIONAL
            val targetAvail = if tH.required then REQUIRED else OPTIONAL
            ((srcAvail, targetAvail), sT, tT, true)
          // Case 2: src+edi match -> process and increment src+edi
          case (s, e, t) if s == e =>
            val srcAvail = if sH.required then REQUIRED else OPTIONAL
            ((srcAvail, MISSING), sT, target, false)
          // Case 3: edi+target match -> process and increment edi+target
          case (s, e, t) if e == t =>
            val targetAvail = if tH.required then REQUIRED else OPTIONAL
            ((MISSING, targetAvail), src, tT, false)
          // Case 4: none match -> no process and increment edi
          case (s, e, t) =>
            ((MISSING, MISSING), src, target, false)
        }
        eH match {
          case e: RefinedSingleFieldSpec =>
            if doCompare then
              (sH, tH) match {
                case (_s:RefinedSingleFieldSpec, _t: RefinedSingleFieldSpec) =>
                  val base = SingleFieldDifference(eH.name, eH.canonicalName, avail)
                  compareSegmentFields(nextS, eT, nextT, acc :+ compareFieldDetail(_s,_t,base))
                case _ =>
                  ZIO.fail(DifferenceError(s"Src/target fields for ${e.canonicalName} have mismatched type"))
              }
            else
              compareSegmentFields(nextS, eT, nextT, acc :+ SingleFieldDifference(eH.name, eH.canonicalName, avail))
          case e: RefinedCompositeFieldSpec =>
            ZIO.fail(DifferenceError("Only single value (non-composite) fields are supported at this time."))
        }
    }

  private def compareFieldDetail(src: RefinedSingleFieldSpec, target: RefinedSingleFieldSpec, base: SingleFieldDifference): SingleFieldDifference =
    val dtDiff = Option.when(src.dataType != target.dataType)(
      (src.dataType, target.dataType)
    )
    val fmtDiff = Option.when(src.format != target.format)(
      (src.format, target.format)
    )
    val elemDiff = Option.when(src.elementId != target.elementId)(
      (src.elementId, target.elementId)
    )
    val valuesDiff = Option.when(src.validValues.sorted != target.validValues.sorted)(
      (src.validValues, target.validValues)
    )
    val valRefDiff = Option.when(src.validValuesRef != target.validValuesRef)(
      (src.validValuesRef, target.validValuesRef)
    )
    base.copy(dataType = dtDiff, format = fmtDiff, elementId = elemDiff, validValues = valuesDiff, validValuesRef = valRefDiff)
