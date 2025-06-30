package co.blocke.edi4s
package mapper

case class MappingTrace(events: List[TraceEvent] = Nil):
  def +(te: TraceEvent): MappingTrace = this.copy(events = events :+ te)

  override def toString: String =
    val sb = new StringBuilder
    var indent = 0

    def pad: String = "  " * indent

    def segLine(s: String) = s"${pad}  >> $s"

    events.foreach {
      case EvalSegEvent(dataSeg, ruleSeg, loc) =>
        sb.append(s"${pad}Evaluating segment: data=${dataSeg.getOrElse("None")}, rule=${ruleSeg.getOrElse("None")}, loc=$loc\n")

      case SegMatchEvent(seg, isImmaculate, orElseUsed) =>
        val flags = List(
          if isImmaculate then Some("immaculate") else None,
          if orElseUsed then Some("orElse") else None
        ).flatten.mkString(", ")
        val suffix = if flags.nonEmpty then s" [$flags]" else ""
        sb.append(segLine(s"Matched segment: $seg$suffix\n"))

      case SegAssignEvent(seg, ruleClass) =>
        sb.append(segLine(s"Assigned segment: $seg using rule: $ruleClass\n"))

      case LoopPushEvent(toLevel, pushedBody) =>
        sb.append(s"${pad}Entering loop: $toLevel with body: ${pushedBody.mkString(", ")}\n")
        indent += 1

      case LoopPopEvent(level, curSeg) =>
        indent = (indent - 1).max(0)
        sb.append(s"${pad}Exiting loop: $level at segment: $curSeg\n")

      case ErrorEvent(msg) =>
        sb.append(s"${pad}❌ ERROR: $msg\n")

      case DoneEvent() =>
        sb.append(s"${pad}✅ Mapping complete\n")
    }

    sb.toString()

sealed trait TraceEvent

case class EvalSegEvent(dataSeg: Option[String], ruleSeg: Option[String], loc: Int) extends TraceEvent

// isImmaculate = true means no src data--we're generating target data from thin air
case class SegMatchEvent(seg: String, isImmaculate: Boolean, orElseUsed: Boolean) extends TraceEvent

case class SegAssignEvent(seg: String, ruleClass: String) extends TraceEvent

case class LoopPushEvent(toLevel: String, pushedBody: List[String]) extends TraceEvent
case class LoopPopEvent(level: String, curSeg: String) extends TraceEvent

case class ErrorEvent(msg: String) extends TraceEvent

case class DoneEvent() extends TraceEvent
