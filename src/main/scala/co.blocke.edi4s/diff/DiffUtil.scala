package co.blocke.edi4s
package diff

import zio.*
import model.*
import Availability.*


object DiffUtil:

  // Returns HL nesting structure List[(description, HL03_value)]
  def getHLLevels(loop: RefinedLoopSpec): List[(String, String)] =
    def recurse(current: Option[RefinedLoopSpec]): List[(String, String)] =
      current match
        case Some(l) =>
          val hl03Values = l.fields.collectFirst {
            case f: RefinedSingleFieldSpec if f.canonicalName == "HL03" =>
              f.validValues.mkString(",")
          }.getOrElse("?")
          (l.description, hl03Values) :: recurse(l.nested)
        case None => Nil

    recurse(Some(loop))

  // Determine if any levels are blended or missing--or if the levels are fundamentally incompatible
  def analyzeHLStructures(
                           srcHL: List[(String, String)],
                           targetHL: List[(String, String)]
                         ): ZIO[Any, DifferenceError, HLSpecRule] = {


    val srcDescs    = srcHL.map(_._1)
    val targetDescs = targetHL.map(_._1) //targetHL.map(_._1)

    if srcDescs == targetDescs then {
      ZIO.succeed(LevelsOk())
    } else {
      val missingSrc   = srcHL.filterNot { case (desc, _) => targetDescs.contains(desc) }
      val extraTarget  = targetHL.filterNot { case (desc, _) => srcDescs.contains(desc) }
      val missingHL03s = missingSrc.map(_._2).toSet

      // Try promotion: look for a target level whose HL03 contains a missing HL03
      val promotionOpt: Option[PromoteTargetLevel] = targetHL.flatMap { case (targetDesc, targetHL03) =>
        val hl03Set = targetHL03.split(',').map(_.trim).toSet
        missingSrc.find { case (missingDesc, srcHL03) =>
          hl03Set.contains(srcHL03)
        }.map {
          case (missingDesc, missingHL03) =>
            PromoteTargetLevel(
              promoteDesc = targetDesc,
              toDesc = missingDesc,
              toHL03 = missingHL03
            )
        }
      }.headOption

      promotionOpt match
        case Some(promo) =>
          ZIO.succeed(promo)

        case None if missingSrc.nonEmpty && extraTarget.isEmpty =>
          ZIO.succeed(FlattenSrcLevel(missingSrc))

        case None =>
          val extras = extraTarget.map(_._1).mkString(", ")
          ZIO.fail(DifferenceError(s"Target HL hierarchy contains unknown/unmapped levels: $extras"))
    }
  }


  // Applies the promotionRule to the target spec and returns the patched RefinedLoopSpec (if nothing went wrong)
  def promoteHLLevel(
                      src: RefinedLoopSpec,
                      target: RefinedLoopSpec,
                      promotionRule: PromoteTargetLevel
                    ): ZIO[Any, DifferenceError, RefinedLoopSpec] = {

    def cloneWithNewHL03(loop: RefinedLoopSpec, newDesc: String, newHL03: String): RefinedLoopSpec = {
      val updatedFields = loop.fields.map {
        case f: RefinedSingleFieldSpec if f.canonicalName == "HL03" =>
          f.copy(validValues = List(newHL03))
        case other => other
      }
      loop.copy(description = newDesc, fields = updatedFields)
    }

    def recurse(srcLoop: RefinedLoopSpec, tgtLoop: RefinedLoopSpec): ZIO[Any, DifferenceError, RefinedLoopSpec] = {
      val loopMatch = srcLoop.canonicalName == tgtLoop.canonicalName &&
        srcLoop.description == tgtLoop.description

      (srcLoop.nested, tgtLoop.nested) match {
        case (Some(sNext), Some(tNext)) if loopMatch =>
          recurse(sNext, tNext).map(updated => tgtLoop.copy(nested = Some(updated)))

        case (Some(sNext), _) if sNext.description == promotionRule.promoteDesc && tgtLoop.description == promotionRule.promoteDesc =>
          val hl03FieldOpt = tgtLoop.fields.collectFirst {
            case f: RefinedSingleFieldSpec if f.canonicalName == "HL03" => f
          }

          hl03FieldOpt match {
            case Some(f) if f.validValues.contains(promotionRule.toHL03) =>
              val inserted = cloneWithNewHL03(tgtLoop, promotionRule.toDesc, promotionRule.toHL03)
              val fixed = fixValidValues(tgtLoop, promotionRule.toHL03)
              ZIO.succeed(inserted.copy(nested = Some(fixed))) // ⬅️ wrap `tgtLoop` inside promoted
            case Some(_) =>
              ZIO.fail(DifferenceError(s"Target HL03 does not support '${promotionRule.toHL03}'"))
            case None =>
              ZIO.fail(DifferenceError("HL03 field not found in target"))
          }

        case _ =>
          ZIO.fail(DifferenceError("Invalid HL structure for promotion"))
      }
    }

    recurse(src, target)
  }


  private def fixValidValues(loop: RefinedLoopSpec, removeValue: String): RefinedLoopSpec = {
    val newFields = loop.fields.zipWithIndex.map {
      case (field: RefinedSingleFieldSpec, 2) =>
        val updatedValues = field.validValues.filterNot(_ == removeValue)
        field.copy(validValues = updatedValues)

      case (field, 2) =>
        // It's index 2, but not a RefinedSingleFieldSpec — leave unchanged
        field

      case (field, _) =>
        field // Any other index — leave unchanged
    }
    loop.copy(fields = newFields)
  }

  // Descending entire diff tree--prune away any entries that produce no target output:
  // * keep any src OPTIONAL or REQUIRED, target MISSING
  // * any other target.availability == MISSING
  // * src.availability == MISSING, target.availability == OPTIONAL
  def prune(diffs: List[SegmentDifference]): List[SegmentDifference] =
    def _pruneFields(fields: List[FieldDifference]): List[FieldDifference] =
      fields.flatMap ( f =>
        if f.availability._2 != MISSING && f.availability != (MISSING,OPTIONAL) then
          Some(f)
        else
          None
      )

    // Fix _shouldKeep here (for segments only) to handle optional/required src--don't prune! Let rule gen handle this.
    def _pruneOneLoop(l: LoopSegmentDifference): Option[LoopSegmentDifference] =
      l.availability match {
        case (OPTIONAL, MISSING) | (REQUIRED, MISSING) => Some(
          l.copy(
            fieldDiff = _pruneFields(l.fieldDiff),
            bodyDiff = prune(l.bodyDiff),
            nested = l.nested.flatMap(_pruneOneLoop)
          )
        )
        case (MISSING,MISSING) | (MISSING,OPTIONAL) => None
        case _ =>
          Some(
            l.copy(
              fieldDiff = _pruneFields(l.fieldDiff),
              bodyDiff = prune(l.bodyDiff),
              nested = l.nested.flatMap(_pruneOneLoop)
            )
          )
      }

    diffs.flatMap {
      case s: SingleSegmentDifference =>
        s.availability match {
          case (OPTIONAL, MISSING) | (REQUIRED, MISSING) =>
            Some(s.copy(fieldDiff = _pruneFields(s.fieldDiff)))
          case (MISSING, MISSING) | (MISSING, OPTIONAL) => None
          case _ =>
            Some(s.copy(fieldDiff = _pruneFields(s.fieldDiff)))
        }
      case l: LoopSegmentDifference =>
        _pruneOneLoop(l)
    }