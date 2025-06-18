package co.blocke.edi4s
package mapper

import zio.*
import model.*
import scala.annotation.tailrec

object MapRunner:

  case class Frame( rules: List[SegmentAssignment], var pc: Int = 0 )

  case class EC( accOut: List[SegmentX12Token] = Nil, frames: List[Frame] = Nil ):

    def pushFrame(rules: List[SegmentAssignment] ): EC =
//      println("  EC PUSHED FRAME: \n"+rules.map(r => r.canonicalName+ " :: "+r.getClass.getName).mkString("    ","\n    ",""))
      this.copy( frames = Frame(rules) :: this.frames )

    def popFrame: EC =
      val popped = this.copy( frames = frames.drop(1) )
      println(" || popped next-rule "+popped.peek.map(_.canonicalName)) // skip the rule that caused the push we're now popping
      popped

    def next: Option[SegmentAssignment] =
      frames.headOption.flatMap{ f =>
        val result = f.rules.lift(f.pc)
        f.pc += 1
        result
      }

    def backspace: EC = this.copy(frames = frames match {
      case h::t => h.copy(pc = h.pc-1) +: t
      case Nil => Nil
    })

    def +( out: SegmentX12Token ): EC =
      println(" --> Mapped "+out.name)
      this.copy(accOut = this.accOut :+ out)

    def peek: Option[SegmentAssignment] = this.frames.headOption.flatMap(f => f.rules.lift(f.pc))

    def showFrames: String =
      "Frames:\n"+ {
        if frames == Nil then "(empty)"
        else
          frames.map(f => "   -------------<<\n" + f.rules.map(r => "   "+r.canonicalName + " -> " + r.getClass.getCanonicalName).mkString("\n")).mkString("\n")
      }


  /*
    Src Spec HL[P]:
       LIN
       SN1
       PO4
       PID

    Target Spec HL[P]:
       PO4
       REF
       MAN
       DTM
       N1

    Src Data:
       LIN
       SN1
       PID
       TD1

    Expected Output:
       PO4
       MAN (synthetic--not in src)

    Rules:
       LIN (no-op)
       SN1 (no-op)
       PO4 (assignment)
       PID (no-op)
       MAN (synthetic assignment)
       *** Missing TD1 *** <- Or maybe TD1 belongs to a popped frame?

    Problem: How to represent a rule/placeholder for when src does not contain a required field in target?
    If src contains a segment NOT used in target we get a NoOpAssignment placeholder

    Bad structure. Source data has TD1, which is not part of HL[Pack]. The data also has an N1 in HL[P] that isn't in the source spec.
      In HL[I] src has PO4, which isn't in spec and is optional in target (correctly, no rule is generated for it)


      EDI 997 is a functional ack.  Sent by receiver and may include errors. It says "I got this document." Doesn't
      guarantee it was successfully ingested.

    */

  private inline def resolveHLName( token: SegmentX12Token ): Option[String] =
    if token.name == "HL" then
      token.fields.find(_.name == "HL03").map(_.asInstanceOf[SimpleX12Token].value)
        .map(n => s"HL[$n]")
    else
      Some(token.name)

  private inline def fieldNum(f: String) = f.takeRight(2).toInt

  private def allTargetFields(assignments: List[FieldAssignment]): List[String] =
    assignments.flatMap {
      case e: EnumMatchFieldAssignment =>
        // Include the EnumMatch’s own targetField and recurse into all case field assignments
        val caseFields = e.cases.values.flatten.toList.flatMap(a => allTargetFields(List(a)))
        e.targetField :: caseFields

      case o: OrElseFieldAssignment =>
        // Include the targetField and recurse into the `assignment` field
        o.targetField :: allTargetFields(List(o.assignment))

      case g: GeneralFieldAssignment =>
        List(g.targetField)
    }

  private def maxTargetFieldNumber(assignments: List[FieldAssignment]): Int =
    allTargetFields(assignments).sorted.lastOption.map(fieldNum).getOrElse(0)

  private def applyFieldAssignments( segRule: FieldsSegmentAssignment | LoopSegmentAssignment, data: SegmentX12Token ): List[X12Token] =

    def resolveFieldValue(f: X12Token): Option[String] = f match {
      case fv: SimpleX12Token => Some(fv.value)
      case fv: EmptyX12Token => Some("")
      case _ => None
    }

    val dataMap = data.fields.map(f => (f.name -> f)).toMap
//    println(dataMap)

    def resolveValueKind( kind: ValueKind, v: String, targetField: String ): String =  // <<< Make this an option vs returning ERROR. May be ok if not found and field is optional!
      kind match {
        case ValueKind.Constant => v
        case ValueKind.Direct => dataMap.get(targetField).flatMap(resolveFieldValue).getOrElse("ERROR")
        case _ => "<CTX>"
      }

    val fieldAssigns = segRule match {
      case f: FieldsSegmentAssignment => f.fieldAssignments
      case f: LoopSegmentAssignment => f.fieldAssignments
    }
    val maxFields = maxTargetFieldNumber(fieldAssigns)  // take last 2 digits

    def assignOneField( f: FieldAssignment, slots: List[String] ): List[String] =
      f match {
        case fa: GeneralFieldAssignment =>
//          println("Updating " + fa.targetField + " num " + fieldNum(fa.targetField))
          val v = resolveValueKind(fa.valueKind, fa.value, fa.targetField)
          val fnum = fieldNum(fa.targetField)
          slots.updated(fnum - 1, v)
        case fa: OrElseFieldAssignment =>
          if dataMap.contains(fa.targetField) then
            assignOneField(fa.assignment, slots)
          else
            val v = resolveValueKind(fa.orElseValueKind, fa.orElseValue, fa.targetField)
            val fnum = fieldNum(fa.targetField)
            slots.updated(fnum - 1, v)
        case fa: EnumMatchFieldAssignment =>
          dataMap.get(fa.targetField).map {
            case v: SimpleX12Token =>
              val fnum = fieldNum(fa.targetField)
              fa.cases.get(v.value).map(assigns =>
                assigns.foldLeft(slots) { case (wipSlots, a) => assignOneField(a, wipSlots) }
              ).getOrElse(slots.updated(fnum - 1, "ERROR"))
            case _: EmptyX12Token =>
              slots // should never happen--makes no sense
            // TODO: Others... (eg repeated)
          }.get  // TODO: Use ZIO here to return error. fd.targetField was not in dataMap
      }
//      if f.targetField.startsWith("PID") then
//        println(s"   >> PID ${f.getClass.getName}: "+z)
//      z

//    println("Map Segment "+segRule.canonicalName+" fields: "+fieldAssigns.size)
    val slotsDone = fieldAssigns.foldLeft(Array.fill(maxFields)("").toList){ (wipSlots, fassign) => assignOneField(fassign, wipSlots) }
    val segName = if segRule.canonicalName.startsWith("HL[") then "HL" else segRule.canonicalName
    slotsDone.zipWithIndex.map{
      case ("", i) => EmptyX12Token( segName + f"${i+1}%02d" )
      case (v, i)  => SimpleX12Token( segName + f"${i+1}%02d", v )
    }


  @tailrec
  private def applySegAssignment(rule: SegmentAssignment, uponData: Option[SegmentX12Token], ec: EC, missingInSrc: Boolean = false ): EC =
//    println( "   Assign = "+rule.getClass.getName+ " upon "+uponData)
//    println( "                 EC: "+ec.accOut.map(_.name).mkString(","))
    rule match {
      case a: NoOpSegmentAssignment => ec // no action
      case a: FieldsSegmentAssignment if uponData.isDefined =>
        // TODO: Real field assignments here...dummy for now
        ec + SegmentX12Token(rule.canonicalName, applyFieldAssignments(a, uponData.get))
      case a: FieldsSegmentAssignment =>
        ec // missing optional src and target is likewise optional (or it'd be OrElseFieldsSegmentAssignment!)
      // TODO: What about LoopSeegmentAssignment w/no data?
      case a: LoopSegmentAssignment =>
        val fields = if uponData.isDefined then applyFieldAssignments(a, uponData.get) else Nil
        val stage1 = ec + SegmentX12Token(rule.canonicalName.replaceAll("""\[\w+]\s*""", ""), fields)
        if missingInSrc then
          stage1
        else
          println("     (push frame) "+a.canonicalName)
          stage1.pushFrame(a.body)
      case a: OrElseFieldsSegmentAssignment if uponData.isDefined =>
        applySegAssignment(a.someAssignment, uponData, ec)
      case a: OrElseFieldsSegmentAssignment =>
        applySegAssignment(a.noneAssignment, uponData, ec)
    }

  private def findHLLevel(level: String, loopRules: LoopSegmentAssignment): Option[LoopSegmentAssignment] =
    if loopRules.canonicalName == level then Some(loopRules)
    else loopRules.nested.flatMap(n => findHLLevel(level, n))

//  var xxx = 0

  // Returns (remaining_segs, EC)
  @tailrec
  private def mapOneSegment(
                             segs: List[SegmentX12Token],
                             ec: EC,
                             loopLatch: Boolean = false // set to true to ignore (once) a missing element (end of loop)
                           ): ZIO[Any, MappingError, (List[SegmentX12Token], EC)] =
//    xxx += 1
//    println(s"<< Iteration $xxx >>>")
    if segs.nonEmpty then
      println("Segment "+resolveHLName(segs.head) + " "+ec.peek.map(_.canonicalName))//ec.frames.head.pc)
//      if segs.head.name == "CTT" then
//        println("   >> CTT found. "+ec.showFrames)
//        xxx += 1
//    if xxx > 50 then ZIO.fail(MappingError("Boom"))
//    else
    (segs, ec.next) match {
      case (Nil,None) =>
//        println("All done...")
        ZIO.succeed(Nil, ec)  // successful completion
      case (sH::sT, None) =>
        // May be an error or end of a loop... need to check
        println("    (pop frame)")
        val ecc = ec.popFrame.backspace
//        println("LoopPop: "+sH.name + " -> "+ecc.peek.map(_.canonicalName))
//        println("EC: "+ec.popFrame.backspace.frames)
//        if xxx < 150 then
//          println("re-mapping...(no loop repeat)")
        mapOneSegment(segs, ecc, true)
//          mapOneSegment(segs, ec.popFrame.backspace)
//        else
//          println(s"What happened? $xxx "+sH)
//          ZIO.fail(MappingError(s"We've run out of mapping rules before we've run out of data at ${sH}."))

      case (Nil, Some(r:FieldsSegmentAssignment)) if r.missingInSrc =>
//        println("(end of data) - Missing In Src"+" -> "+ec.peek.map(_.canonicalName))
        mapOneSegment(segs, applySegAssignment(r, None, ec))  // don't advance segs
      case (Nil, Some(r:LoopSegmentAssignment)) if r.missingInSrc =>
//        println("(end of data) (loop) - Missing In Src"+" -> "+ec.peek.map(_.canonicalName))
        mapOneSegment(segs, applySegAssignment(r, None, ec))  // don't advance segs

      case (Nil, Some(r)) =>
        ZIO.fail(MappingError(s"We've run out of data before we've run out of mapping rules at ${r.canonicalName} segment ${ec.frames.head.pc}."))

//      case (sH::sT, Some(r:LoopSegmentAssignment)) if (sH.name == "HL" && (r.canonicalName == resolveHLName(sH))) =>
      case (sH::sT, Some(r:LoopSegmentAssignment)) if sH.name == "HL" && r.canonicalName.startsWith("HL") =>

        resolveHLName(sH) match {
          case None => ZIO.fail(MappingError("No HL03 field found on HL field."))
          case Some(hl) =>
            // Now we need to make sure levels are ok, or do we need to go to nested level
            findHLLevel(hl, r) match {
              case None => ZIO.fail(MappingError(s"No HL level defined for discriminator $hl"))
              case Some(x) =>
                println(s"    <HL loop found> $hl -> "+x.canonicalName)
                mapOneSegment(sT, applySegAssignment(x, Some(sH), ec))
            }
        }
//        println("Found HL: "+sH)
        // Gotta parse out the level from r (HL[S]->S) then compare it to HL03 in the data.
        // If not a match -- have logic to look ahead and "find" the right level.
//        sH.fields.find(_.name == "HL03").map(_.asInstanceOf[SimpleX12Token].value) match {
//          case None => ZIO.fail(MappingError("No HL03 field found on HL field."))
//          case Some(v) => findHLLevel(s"HL[$v]", r) match {
//            case None => ZIO.fail(MappingError(s"No HL level defined for discriminator $v"))
//            case Some(x) =>
//              println("    <HL loop found> "+r.canonicalName)
//              mapOneSegment(sT, applySegAssignment(x, Some(sH), ec))
//          }
//        }
      case (sH::sT, Some(r)) if (sH.name == r.canonicalName) =>
//        println("Direct Match: "+sH.name + " :: "+r.canonicalName)
        mapOneSegment(sT, applySegAssignment(r, Some(sH), ec))

      case (sH::sT, Some(r:FieldsSegmentAssignment)) if r.missingInSrc =>
//        println("Mismatch - Missing In Src "+sH.name+ " -> "+ec.peek.map(_.canonicalName))
        mapOneSegment(segs, applySegAssignment(r, None, ec, r.missingInSrc))  // don't advance segs
      case (sH::sT, Some(r:LoopSegmentAssignment)) if r.missingInSrc =>
//        println("Mismatch (loop) - Missing In Src "+sH.name+ " -> "+ec.peek.map(_.canonicalName) + " for rule "+r.canonicalName)
        mapOneSegment(segs, applySegAssignment(r, None, ec, r.missingInSrc))  // don't advance segs

      case (sH::sT, Some(r)) if !loopLatch =>
//        println("Missing: "+sH.name + " :: "+r.canonicalName)
        mapOneSegment(segs, applySegAssignment(r, None, ec))  // don't advance segs
      case (sH::sT, Some(r)) =>
//        println("Loop skip: "+sH.name + " :: "+r.canonicalName)
//        println("     (peek): "+ec.peek.get.canonicalName)
        mapOneSegment(segs, ec)  // don't advance segs
    }


  def mapWithRules(isa: IsaSegment, rules: MappingSpec): ZIO[Any, MappingError, IsaSegment] =


    def transformBody(body: List[SegmentX12Token]): ZIO[Any, MappingError, EC] =
      val ec = EC().pushFrame(rules.rules)
      mapOneSegment(isa.groupSets.head.transactions.head.body, ec).map(_._2)

    def transformSt(st: StSegment): ZIO[Any, MappingError, StSegment] =
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

    def transformGs(gs: GsSegment): ZIO[Any, MappingError, GsSegment] =
      for
        newTransactions <- ZIO.foreach(gs.transactions)(transformSt)
      yield gs.copy(transactions = newTransactions)

    // Note: this code here is very prototype-y.  Does dumb and blind copy of ISA and GT segments.
    // Real code needs to parse and interpret these!

    println("==================================================================")
    for
      newGroupSets <- ZIO.foreach(isa.groupSets)(transformGs)
    yield isa.copy(groupSets = newGroupSets)
