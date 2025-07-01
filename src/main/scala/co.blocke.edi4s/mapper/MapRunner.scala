package co.blocke.edi4s
package mapper

import zio.*
import model.*
import scala.annotation.tailrec
import Availability.*

object MapRunner:

  val MAX_BREAK = 4000 // max number of iterations before we decide we're in an endless loop--this may fail for very large messages!


  private inline def resolveHLName( token: SegmentX12Token ): Option[String] =
    if token.name == "HL" then
      token.fields.find(_.name == "HL03").map(_.asInstanceOf[SimpleX12Token].value)
        .map(n => s"HL[$n]")
    else
      Some(token.name)

  private inline def fieldNum(f: String) = f.takeRight(2).toInt

  private def applyFieldAssignmentsZIO(
                                        segRule: SingleSegmentAssignment | LoopSegmentAssignment,
                                        data: Option[SegmentX12Token]
                                      ): ZIO[Any, MappingError, List[X12Token]] = {

    def resolveFieldValue(f: X12Token): Option[String] = f match {
      case fv: SimpleX12Token => Some(fv.value)
      case fv: EmptyX12Token => Some("")
      case _ => None
    }

    val dataMap = data.map(_.fields.map(f => f.name -> f).toMap).getOrElse(Map.empty[String, X12Token])

    val maxFields = segRule.fieldAssignments.lastOption match {
      case Some(m: MatchFieldAssignment) =>
        m.cases(m.cases.keySet.toList.head).lastOption.map(fa => fa.targetField.takeRight(2).toInt).getOrElse(0)
      case _ =>
        segRule.fieldAssignments.lastOption.map(fa => fa.targetField.takeRight(2).toInt).getOrElse(0)
    }

    def assignOneField(f: FieldAssignment, slots: List[String]): ZIO[Any, MappingError, List[String]] =
      f match {
        case fa: PlaceholderAssignment =>
          ZIO.succeed {
            val fnum = fieldNum(fa.targetField)
            slots.updated(fnum - 1, fa.dummyValue)
          }

        case fa: DirectAssignment =>
          val value = (dataMap.get(fa.targetField), fa.availability._1) match {
            case (None, OPTIONAL) => ZIO.succeed("")
            case (Some(d), _) => ZIO.fromOption(resolveFieldValue(d)).orElseFail(MappingError(s"Unresolvable field: ${fa.targetField}"))
            case _ => ZIO.fail(MappingError(s"Required field missing: ${fa.targetField}"))
          }

          value.map { v =>
            val fnum = fieldNum(fa.targetField)
            slots.updated(fnum - 1, v)
          }

        case fa: ConstantAssignment =>
          ZIO.succeed {
            val fnum = fieldNum(fa.targetField)
            slots.updated(fnum - 1, fa.value)
          }

        case fa: ProfileAssignment =>
          ZIO.succeed {
            val fnum = fieldNum(fa.targetField)
            slots.updated(fnum - 1, "<CTX>")
          }

        case fa: MatchFieldAssignment =>
          dataMap.get(fa.targetField) match {
            case Some(v: SimpleX12Token) =>
              val fnum = fieldNum(fa.targetField)
              val mt = if fa.availability._2 == OPTIONAL then "" else "ERROR"

              fa.cases.get(v.value) match {
                case Some(assignments) =>
                  ZIO.foldLeft(assignments)(slots) { (wipSlots, a) => assignOneField(a, wipSlots) }
                case None =>
                  ZIO.succeed(slots.updated(fnum - 1, mt))
              }

            case Some(_: EmptyX12Token) =>
              ZIO.succeed(slots)

            case Some(other) =>
              ZIO.fail(MappingError(s"Unsupported token type for ${fa.targetField}: $other"))

            case None =>
              ZIO.fail(MappingError(s"Match field not found in data: ${fa.targetField}"))
          }
      }

    val zioResult = ZIO.foldLeft(segRule.fieldAssignments)(Array.fill(maxFields)("").toList) { (wipSlots, fassign) =>
      assignOneField(fassign, wipSlots)
    }

    zioResult.map { slotsDone =>
      val segName = if segRule.canonicalName.startsWith("HL[") then "HL" else segRule.canonicalName
      slotsDone.zipWithIndex.map {
        case ("", i) => EmptyX12Token(segName + f"${i + 1}%02d")
        case (v, i) => SimpleX12Token(segName + f"${i + 1}%02d", v)
      }
    }
  }

  //----------------------------------------------------------------------------------

  private inline def classname(c: Any) =
//    println(">>> "+c.getClass.getName)
    c.getClass.getName.split('.').last

  private def applySegAssignmentZIO(
                                     rule: SegmentAssignment,
                                     uponData: Option[SegmentX12Token],
                                     ec: EC,
                                     trace: MappingTrace
                                   ): ZIO[Any, MappingTrace, (EC, MappingTrace, Boolean)] = {

    def withHandledError[A](z: ZIO[Any, MappingError, A]): ZIO[Any, MappingTrace, A] =
      z.mapError { case MappingError(msg) => trace + ErrorEvent(msg) }

    rule match {
      case a: NoOpSegmentAssignment =>
        ZIO.succeed((ec, trace + SegAssignEvent(a.canonicalName, classname(a)), false))

      case a: SingleSegmentAssignment if uponData.isDefined =>
        withHandledError(applyFieldAssignmentsZIO(a, uponData)).flatMap { tokens =>
          val updatedEC = ec + SegmentX12Token(a.canonicalName, tokens)
          val updatedTrace = trace + SegAssignEvent(a.canonicalName, classname(a))
          ZIO.succeed((updatedEC, updatedTrace, false))
        }

      case a: SingleSegmentAssignment =>
        if a.availability._2 == REQUIRED then
          withHandledError(applyFieldAssignmentsZIO(a, uponData)).flatMap { tokens =>
            val updatedEC = ec + SegmentX12Token(a.canonicalName, tokens)
            val updatedTrace = trace + SegAssignEvent(a.canonicalName + " (missing+required)", classname(a))
            ZIO.succeed((updatedEC, updatedTrace, false))
          }
        else
          ZIO.succeed((ec, trace + SegAssignEvent(a.canonicalName + " (optional+missing)", classname(a)), false))

      case a: LoopSegmentAssignment =>
        withHandledError(applyFieldAssignmentsZIO(a, uponData)).flatMap { tokens =>
          val baseSeg = SegmentX12Token(a.canonicalName.replaceAll("""\[\w+]\s*""", ""), tokens)
          val stage1 = ec + baseSeg

          if a.availability._1 == MISSING then
            ZIO.succeed((stage1, trace + SegAssignEvent(a.canonicalName + " (src missing)", classname(a)), true))
          else if a.body.isEmpty then
            ZIO.succeed((stage1.backspace, trace + SegAssignEvent(a.canonicalName, classname(a)), true))
          else
            ZIO.succeed((stage1.pushFrame(a.canonicalName, a.body), trace + LoopPushEvent(a.canonicalName, a.body.map(_.canonicalName)), true))
        }
    }
  }

  private def findHLLevel(level: String, loopRules: LoopSegmentAssignment): Option[LoopSegmentAssignment] =
    if loopRules.canonicalName == level then Some(loopRules)
    else loopRules.nested.flatMap(n => findHLLevel(level, n))

  // Returns (remaining_segs, EC)
  private def mapOneSegment(
                             segs: List[SegmentX12Token],
                             ec: EC,
                             trace: MappingTrace,
                             loopLatch: Boolean = false,
                             breakLimit: Int = 0
                           ): ZIO[Any, MappingTrace, (List[SegmentX12Token], EC, MappingTrace)] = {
    if breakLimit > MAX_BREAK then
      ZIO.fail(trace + ErrorEvent("Endless loop detected for segment " + segs.headOption.map(_.name)))
    else
      (segs, ec.next) match {
        case (Nil, None) =>
          ZIO.succeed((Nil, ec, trace + DoneEvent()))

        case (sH :: sT, None) =>
          val ecc = ec.popFrame.backspace
          ecc.frames.headOption match {
            case Some(topFrame) =>
              val newTrace = trace + EvalSegEvent(Some(sH.name), None, 1) +
                LoopPopEvent(topFrame.level, ecc.peek.map(_.canonicalName).getOrElse("unknown"))
              mapOneSegment(segs, ecc, newTrace, loopLatch = true, breakLimit + 1)
            case None =>
              ZIO.fail(trace + EvalSegEvent(Some(sH.name), None, 2) +
                ErrorEvent(s"End of rules detected yet there's more data at segment $breakLimit"))
          }

        case (Nil, Some(r: SingleSegmentAssignment)) if r.availability._1 == MISSING =>
          val newTrace = trace + EvalSegEvent(None, Some(r.canonicalName), 3) + SegMatchEvent(r.canonicalName, true, false)
          applySegAssignmentZIO(r, None, ec, newTrace).flatMap {
            case (ec2, newTrace2, isLoop) =>
              mapOneSegment(segs, ec2, newTrace2, isLoop, breakLimit + 1)
          }

        case (Nil, Some(r: LoopSegmentAssignment)) if r.availability._1 == MISSING =>
          val newTrace = trace + EvalSegEvent(None, Some(r.canonicalName), 4) + SegMatchEvent(r.canonicalName, true, false)
          applySegAssignmentZIO(r, None, ec, newTrace).flatMap {
            case (ec2, newTrace2, isLoop) =>
              mapOneSegment(segs, ec2, newTrace2, isLoop, breakLimit + 1)
          }

        case (Nil, Some(r)) =>
          ZIO.fail(trace + EvalSegEvent(None, Some(r.canonicalName), 5) +
            ErrorEvent(s"We've run out of data before we've run out of mapping rules at ${r.canonicalName} segment ${ec.frames.head.pc}."))

        case (sH :: sT, Some(r: LoopSegmentAssignment)) if sH.name == "HL" && r.canonicalName.startsWith("HL") =>
          resolveHLName(sH) match {
            case None =>
              ZIO.fail(trace + ErrorEvent(s"No HL03 field found on HL field on segment $breakLimit"))
            case Some(hl) =>
              val trace1 = trace + EvalSegEvent(Some(sH.name), Some(hl), 6)
              val newTrace = trace1 + SegMatchEvent(hl, false, false)
              findHLLevel(hl, r) match {
                case None =>
                  ZIO.fail(newTrace + ErrorEvent(s"No HL level defined for discriminator $hl on segment $breakLimit"))
                case Some(x) =>
                  applySegAssignmentZIO(x, Some(sH), ec, newTrace).flatMap {
                    case (ec2, newTrace2, isLoop) =>
                      mapOneSegment(sT, ec2, newTrace2, isLoop, breakLimit + 1)
                  }
              }
          }

        case (sH :: sT, Some(r)) if sH.name == r.canonicalName =>
          val newTrace = trace + EvalSegEvent(Some(sH.name), Some(r.canonicalName), 7) + SegMatchEvent(r.canonicalName, false, false)
          applySegAssignmentZIO(r, Some(sH), ec, newTrace).flatMap {
            case (ec2, newTrace2, isLoop) =>
              mapOneSegment(sT, ec2, newTrace2, isLoop, breakLimit + 1)
          }

        case (sH :: sT, Some(r: SegmentAssignment)) if r.availability == (OPTIONAL, REQUIRED) && !loopLatch =>
          val newTrace = trace + EvalSegEvent(Some(sH.name), Some(r.canonicalName), 8) + SegMatchEvent(r.canonicalName, false, true)
          r match {
            case sa: SingleSegmentAssignment if sa.orElseAssignments.isDefined =>
              applySegAssignmentZIO(sa.orElseAssignments.get, None, ec, newTrace).flatMap {
                case (ec2, newTrace2, isLoop) =>
                  mapOneSegment(segs, ec2, newTrace2, isLoop, breakLimit + 1)
              }
            case la: LoopSegmentAssignment if la.orElseAssignments.isDefined =>
              applySegAssignmentZIO(la.orElseAssignments.get, None, ec, newTrace).flatMap {
                case (ec2, newTrace2, isLoop) =>
                  mapOneSegment(segs, ec2, newTrace2, isLoop, breakLimit + 1)
              }
            case _ =>
              ZIO.fail(newTrace + ErrorEvent(s"Src optional, target required, but no 'orElse' assignment specified for when src is None on segment $breakLimit"))
          }

        case (sH :: sT, Some(r)) if !loopLatch =>
          val newTrace = trace + EvalSegEvent(Some(sH.name), Some(r.canonicalName), 9)
          applySegAssignmentZIO(r, None, ec, newTrace).flatMap {
            case (ec2, newTrace2, isLoop) =>
              mapOneSegment(segs, ec2, newTrace2, isLoop, breakLimit + 1)
          }

        case (sH :: sT, Some(r)) =>
          val newTrace = trace + EvalSegEvent(Some(sH.name), Some(r.canonicalName), 10)
          mapOneSegment(segs, ec, newTrace, loopLatch = false, breakLimit + 1)
      }
  }

  // Top-level call to run rules. If error occurs, we return a trace of what happened. Mapping is so complex that
  // we need to return some visibility into the process so we don't devolve into a forrest of printlns!
  def mapWithRules(isa: IsaSegment, rules: MappingSpec): ZIO[Any, MappingTrace, IsaSegment] =

    def transformBody(body: List[SegmentX12Token]): ZIO[Any, MappingTrace, EC] =
      val ec = EC().pushFrame("top",rules.rules)
      mapOneSegment(isa.groupSets.head.transactions.head.body, ec, MappingTrace()).map(_._2)

    def transformSt(st: StSegment): ZIO[Any, MappingTrace, StSegment] =
      for
        // Process ST
        // In real life, this is where you'd go to some cache and get the ruleset of the doc, eg 856
        _ <- ZIO.succeed {
          println("================")
          println("  EDI: " + st.transactionSetIdCode)
          println("================")
        }
        newBodyEC <- transformBody(st.body)
      yield st.copy(body = newBodyEC.accOut)

    def transformGs(gs: GsSegment): ZIO[Any, MappingTrace, GsSegment] =
      for
        newTransactions <- ZIO.foreach(gs.transactions)(transformSt)
      yield gs.copy(transactions = newTransactions)

    // Note: this code here is very prototype-y.  Does dumb and blind copy of ISA and GT segments.
    // Real code needs to parse and interpret these!

    println("==================================================================")
    for
      newGroupSets <- ZIO.foreach(isa.groupSets)(transformGs)
    yield isa.copy(groupSets = newGroupSets)
