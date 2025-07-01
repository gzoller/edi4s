package co.blocke.edi4s
package mapper

import zio.*
import model.*
import scala.annotation.tailrec
import Availability.*

object MapRunner:

  val MAX_BREAK = 200 // max number of iterations before we decide we're in an endless loop--this may fail for very large messages!


  private inline def resolveHLName( token: SegmentX12Token ): Option[String] =
    if token.name == "HL" then
      token.fields.find(_.name == "HL03").map(_.asInstanceOf[SimpleX12Token].value)
        .map(n => s"HL[$n]")
    else
      Some(token.name)

  private inline def fieldNum(f: String) = f.takeRight(2).toInt

  private def applyFieldAssignments(segRule: SingleSegmentAssignment | LoopSegmentAssignment, data: Option[SegmentX12Token]): List[X12Token] =
    def resolveFieldValue(f: X12Token): Option[String] = f match {
      case fv: SimpleX12Token => Some(fv.value)
      case fv: EmptyX12Token => Some("")
      case _ => None
    }

    val dataMap = data.map(_.fields.map(f => (f.name -> f)).toMap).getOrElse(Map.empty[String, X12Token])
  //    println(dataMap)

  //    val fieldAssigns = segRule.fieldAssignments
  //    val maxFields = maxTargetFieldNumber(fieldAssigns)  // take last 2 digits
    val maxFields = segRule.fieldAssignments.lastOption match {
      case Some(m: MatchFieldAssignment) =>  // for Match, we gotta dig deeper to discover the ordinality of the fields
        m.cases(m.cases.keySet.toList.head).lastOption.map(fa => fa.targetField.takeRight(2).toInt).getOrElse(0)
      case _ => segRule.fieldAssignments.lastOption.map(fa => fa.targetField.takeRight(2).toInt).getOrElse(0)
    }

    def assignOneField( f: FieldAssignment, slots: List[String] ): List[String] =
      f match {
        // This should never happen in production!
        case fa: PlaceholderAssignment =>
          val fnum = fieldNum(fa.targetField)
          slots.updated(fnum-1, fa.dummyValue)
        case fa: DirectAssignment =>
  //          println("Updating " + fa.targetField + " num " + fieldNum(fa.targetField))
          val v = (dataMap.get(fa.targetField), fa.availability._1) match { // data + src availability -- ignore optional
            case (None, OPTIONAL) => ""
            case (Some(d), _) => resolveFieldValue(d).getOrElse("")
            case _ => "ERROR"
          }
          val fnum = fieldNum(fa.targetField)
          slots.updated(fnum - 1, v)
        case fa: ConstantAssignment =>
          //          println("Updating " + fa.targetField + " num " + fieldNum(fa.targetField))
          val fnum = fieldNum(fa.targetField)
          slots.updated(fnum - 1, fa.value)
        case fa: ProfileAssignment =>
          //          println("Updating " + fa.targetField + " num " + fieldNum(fa.targetField))
          val fnum = fieldNum(fa.targetField)
          slots.updated(fnum - 1, "<CTX>")  // TODO: Wire up to the context + profile object
        case fa: MatchFieldAssignment =>
          dataMap.get(fa.targetField).map {
            case v: SimpleX12Token =>
              val fnum = fieldNum(fa.targetField)
              val mt = if fa.availability._2 == OPTIONAL then "" else "ERROR"
              fa.cases.get(v.value).map(assigns =>
                assigns.foldLeft(slots) { case (wipSlots, a) => assignOneField(a, wipSlots) }
              ).getOrElse(slots.updated(fnum - 1, mt))
            case _: EmptyX12Token =>
              slots // should never happen--makes no sense
            // TODO: Others... (eg repeated)
          }.get  // TODO: Use ZIO here to return error. fd.targetField was not in dataMap
      }

  //    println("Map Segment "+segRule.canonicalName+" fields: "+fieldAssigns.size)
    val slotsDone = segRule.fieldAssignments.foldLeft(Array.fill(maxFields)("").toList){ (wipSlots, fassign) => assignOneField(fassign, wipSlots) }
    val segName = if segRule.canonicalName.startsWith("HL[") then "HL" else segRule.canonicalName
    slotsDone.zipWithIndex.map{
      case ("", i) => EmptyX12Token( segName + f"${i+1}%02d" )
      case (v, i)  => SimpleX12Token( segName + f"${i+1}%02d", v )
    }

  //----------------------------------------------------------------------------------

  private inline def classname(c: Any) =
//    println(">>> "+c.getClass.getName)
    c.getClass.getName.split('.').last

  private def applySegAssignment(rule: SegmentAssignment, uponData: Option[SegmentX12Token], ec: EC, trace: MappingTrace): (EC,MappingTrace,Boolean) =
//    println( s"Assign segment ${uponData.map(_.name)} with assignment "+ rule.getClass.getName.split('.').last)
    //    println( "                 EC: "+ec.accOut.map(_.name).mkString(","))
    rule match {
      case a: NoOpSegmentAssignment =>
//        println(s"   --- No-Op (${a.canonicalName}) ---")
        (ec, trace + SegAssignEvent(a.canonicalName, classname(a)), false) // no action
      case a: SingleSegmentAssignment if uponData.isDefined =>
        (
          ec + SegmentX12Token(a.canonicalName, applyFieldAssignments(a, uponData)),
          trace + SegAssignEvent(a.canonicalName, classname(a)),
          false
        )
      case a: SingleSegmentAssignment =>
        if a.availability._2 == REQUIRED then // for MISSING/REQUIRED.  All others do nothing
          (
            ec + SegmentX12Token(a.canonicalName, applyFieldAssignments(a, uponData)),
            trace + SegAssignEvent(a.canonicalName + " (missing+required)", classname(a)),
            false // missing optional src and target is likewise optional (or it'd be OrElseFieldsSegmentAssignment!)
          )
        else
          (ec, trace + SegAssignEvent(a.canonicalName + " (optional+missing)", classname(a)), false) // missing optional src and target is likewise optional (or it'd be OrElseFieldsSegmentAssignment!)
      case a: LoopSegmentAssignment =>
        val stage1 = ec + SegmentX12Token(a.canonicalName.replaceAll("""\[\w+]\s*""", ""), applyFieldAssignments(a, uponData))
        if a.availability._1 == MISSING then
          (stage1, trace + SegAssignEvent(a.canonicalName + " (src missing)", classname(a)), true)
        else if a.body.isEmpty then
          (stage1.backspace, trace + SegAssignEvent(a.canonicalName, classname(a)), true)
        else
          //          println("     (push frame) "+a.canonicalName)
          (
            stage1.pushFrame(a.canonicalName, a.body),
            trace + LoopPushEvent(a.canonicalName, a.body.map(_.canonicalName)),
            true
          )
    }

  private def findHLLevel(level: String, loopRules: LoopSegmentAssignment): Option[LoopSegmentAssignment] =
    if loopRules.canonicalName == level then Some(loopRules)
    else loopRules.nested.flatMap(n => findHLLevel(level, n))

  // Returns (remaining_segs, EC)
  @tailrec
  private def mapOneSegment(
                             segs: List[SegmentX12Token],
                             ec: EC,
                             trace: MappingTrace,
                             loopLatch: Boolean = false, // set to true to ignore (once) a missing element (end of loop)
                             breakLimit: Int = 0 // protect us from infinite loops
                           ): ZIO[Any, MappingTrace, (List[SegmentX12Token], EC, MappingTrace)] =
//    if segs.nonEmpty then
//      println("Segment "+resolveHLName(segs.head) + " "+ec.peek.map(_.canonicalName))//ec.frames.head.pc)
    if breakLimit > MAX_BREAK then
      println("BOOM: \n"+trace.events.mkString("\n"))
      ZIO.fail( trace + ErrorEvent("Endless loop detected for segment "+segs.headOption.map(_.name)))
    else
      (segs, ec.next) match {
        case (Nil,None) =>  // Successful completion
          ZIO.succeed(Nil, ec, trace + DoneEvent()) // trace is ignored upon return because we succeeded

        case (sH::sT, None) =>  // May be an error or end of a loop... need to check
          //        println("    (pop frame)")
          val ecc = ec.popFrame.backspace
          ecc.frames.headOption match {
            case Some(topFrame) =>
              val newTrace = trace + EvalSegEvent(Some(sH.name), None, 1) + LoopPopEvent(topFrame.level, ecc.peek.map(_.canonicalName).getOrElse("unknown"))
              //        println("LoopPop: "+sH.name + " -> "+ecc.peek.map(_.canonicalName))
              mapOneSegment(segs, ecc, newTrace, true, breakLimit + 1)
            case None =>
              println("BOOM: \n"+trace)
              ZIO.fail( trace + EvalSegEvent(Some(sH.name), None, 2) + ErrorEvent(s"End of rules detected yet there's more data at segment $breakLimit"))
          }

        case (Nil, Some(r:SingleSegmentAssignment)) if r.availability._1 == MISSING => // Single: missing in src and end of data (not an error)
          val newTrace = trace + EvalSegEvent(None, Some(r.canonicalName), 3) + SegMatchEvent(r.canonicalName, true, false)
          //        println("(end of data) - Missing In Src"+" -> "+ec.peek.map(_.canonicalName))
          val (ec2, newTrace2, isLoop) = applySegAssignment(r, None, ec, newTrace)
          mapOneSegment(segs, ec2, newTrace2, isLoop, breakLimit+1)  // don't advance segs
        case (Nil, Some(r:LoopSegmentAssignment)) if r.availability._1 == MISSING => // Single: missing in src and end of data (not an error)
          val newTrace = trace + EvalSegEvent(None, Some(r.canonicalName), 4) + SegMatchEvent(r.canonicalName, true, false)
//          val nextTrace = trace + LoopSegmentTrace("(none)", r.getClass.getName.split('.').last, r.availability, Nil, None, Nil, false)
          //        println("(end of data) (loop) - Missing In Src"+" -> "+ec.peek.map(_.canonicalName))
          val (ec2, newTrace2, isLoop) = applySegAssignment(r, None, ec, newTrace)
          mapOneSegment(segs, ec2, newTrace2, isLoop, breakLimit+1)  // don't advance segs

        case (Nil, Some(r)) =>  // End of data but not missing in src, means unexpected early termination of input data
          ZIO.fail( trace + EvalSegEvent(None, Some(r.canonicalName), 5) + ErrorEvent(s"We've run out of data before we've run out of mapping rules at ${r.canonicalName} segment ${ec.frames.head.pc}."))

        //      case (sH::sT, Some(r:LoopSegmentAssignment)) if (sH.name == "HL" && (r.canonicalName == resolveHLName(sH))) =>
        case (sH::sT, Some(r:LoopSegmentAssignment)) if sH.name == "HL" && r.canonicalName.startsWith("HL") =>  // HL loop handling
          resolveHLName(sH) match {
            case None => ZIO.fail( trace + ErrorEvent(s"No HL03 field found on HL field on segment $breakLimit"))
            case Some(hl) =>
              val trace1 = trace + EvalSegEvent(Some(sH.name), Some(hl), 6)
              val newTrace = trace1 + SegMatchEvent(hl, false, false)
              // Now we need to make sure levels are ok, or do we need to go to nested level
              findHLLevel(hl, r) match {
                case None => ZIO.fail( newTrace + ErrorEvent(s"No HL level defined for discriminator $hl on segment $breakLimit"))
                case Some(x) =>
//                  println(s"    <HL loop found> $hl -> "+x.canonicalName)
                  val (ec2, newTrace2, isLoop) = applySegAssignment(x, Some(sH), ec, newTrace)
                  mapOneSegment(sT, ec2, newTrace2, isLoop, breakLimit+1)
              }
          }

        case (sH::sT, Some(r)) if (sH.name == r.canonicalName) => // Direct match
          val newTrace = trace + EvalSegEvent(Some(sH.name), Some(r.canonicalName), 7) + SegMatchEvent(r.canonicalName, false, false)
          //        println("Direct Match: "+sH.name + " :: "+r.canonicalName)
          val (ec2, newTrace2, isLoop) = applySegAssignment(r, Some(sH), ec, newTrace)
          mapOneSegment(sT, ec2, newTrace2, isLoop, breakLimit+1)

        case (sH::sT, Some(r:SegmentAssignment)) if r.availability == (OPTIONAL,REQUIRED) && !loopLatch => // GetOrElse assignment
          val newTrace = trace + EvalSegEvent(Some(sH.name), Some(r.canonicalName), 8) + SegMatchEvent(r.canonicalName, false, true)
          //        println("Mismatch - Missing In Src "+sH.name+ " -> "+ec.peek.map(_.canonicalName))
          r match {
            case sa: SingleSegmentAssignment if sa.orElseAssignments.isDefined =>
              val (ec2, newTrace2, isLoop) = applySegAssignment(sa.orElseAssignments.get, None, ec, newTrace)
              mapOneSegment(segs, ec2, newTrace2, isLoop, breakLimit+1)  // don't advance segs
            case la: LoopSegmentAssignment if la.orElseAssignments.isDefined =>
              val (ec2, newTrace2, isLoop) = applySegAssignment(la.orElseAssignments.get, None, ec, newTrace)
              mapOneSegment(segs, ec2, newTrace2, isLoop, breakLimit+1)  // don't advance segs
            case _ =>
              ZIO.fail( newTrace + ErrorEvent(s"Src optional, target required, but no 'orElse' assignment specified for when src is None on segment $breakLimit"))
          }

        case (sH::sT, Some(r)) if !loopLatch =>
          // no trace activity here
          //        println("Missing: "+sH.name + " :: "+r.canonicalName)
          val (ec2, newTrace2, isLoop) = applySegAssignment(r, None, ec, trace + EvalSegEvent(Some(sH.name), Some(r.canonicalName), 9))
          mapOneSegment(segs, ec2, newTrace2, isLoop, breakLimit+1)  // don't advance segs

        case (sH::sT, Some(r)) =>
          // no trace activity here
          //        println("Loop skip: "+sH.name + " :: "+r.canonicalName)
          //        println("     (peek): "+ec.peek.get.canonicalName)
          mapOneSegment(segs, ec, trace + EvalSegEvent(Some(sH.name), Some(r.canonicalName), 10), false, breakLimit+1)  // don't advance segs
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


/*

    Problems:

    2) Empty MAN fields in HL[P]
    3) Empty PID fields
    4) Empty N1 fields
    */