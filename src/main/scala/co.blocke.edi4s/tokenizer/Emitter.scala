package co.blocke.edi4s
package tokenizer

import model.*
//import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// This presumes that all mapping/assignment has been done and the ISA is fully ready to render!
// The TokenizerConfig can come from the parsing cycle, or you can specify target-specific values for emitting.

object Emitter:

  private val dateFormatter = DateTimeFormatter.ofPattern("yyMMdd")
  private val timeFormatter = DateTimeFormatter.ofPattern("HHmm")

  def emitTransaction( cfg: TokenizerConfig, isa: IsaSegment ): String =
    val sb = new StringBuilder()
    emitISA(cfg, sb, isa)
    isa.groupSets.foreach( gs =>
      emitGS(cfg, sb, gs)
      gs.transactions.foreach( tx =>
        emitSt(cfg, sb, tx)
        tx.body.foreach( bodySeg => emit(cfg, sb, bodySeg))// emitBody...
        emitSe(cfg, sb, tx)
      )
      emitGE(cfg, sb, gs)
    )
    emitIEA(cfg, sb, isa)
    sb.toString

  //    val now = LocalDateTime.now()
  //    val dateStr = now.format(dateFormatter) // e.g., "250511"
  //    val timeStr = now.format(timeFormatter) // e.g., "1932"

  private def emitISA(cfg: TokenizerConfig, sb: StringBuilder, isa: IsaSegment ): StringBuilder =
    sb.append(s"ISA${cfg.elementDelimiter}")
    sb.append(s"${isa.authorizationQualifier}${cfg.elementDelimiter}")
    sb.append(s"${isa.authorizationInfo}${cfg.elementDelimiter}")
    sb.append(s"${isa.securityQualifier}${cfg.elementDelimiter}")
    sb.append(s"${isa.securityInfo}${cfg.elementDelimiter}")
    sb.append(s"${isa.senderQualifier}${cfg.elementDelimiter}")
    sb.append(s"${isa.senderId}${cfg.elementDelimiter}")
    sb.append(s"${isa.receiverQualifier}${cfg.elementDelimiter}")
    sb.append(s"${isa.receiverId}${cfg.elementDelimiter}")
    sb.append(s"${isa.interchangeDate}${cfg.elementDelimiter}")
    sb.append(s"${isa.interchangeTime}${cfg.elementDelimiter}")
    sb.append(s"${isa.repetitionSeparator}${cfg.elementDelimiter}")
    sb.append(s"${isa.version}${cfg.elementDelimiter}")
    sb.append(s"${isa.controlNumber}${cfg.elementDelimiter}")
    sb.append(s"${isa.ackRequested}${cfg.elementDelimiter}")
    sb.append(s"${isa.usageIndicator}${cfg.elementDelimiter}")
    sb.append(s"${isa.componentSeparator}${cfg.segmentDelimiter}")
    sb

  private def emitGS(cfg: TokenizerConfig, sb: StringBuilder, gs: GsSegment ): StringBuilder =
    sb.append(s"GS${cfg.elementDelimiter}")
    sb.append(s"${gs.functionalIdCode}${cfg.elementDelimiter}")
    sb.append(s"${gs.applicationSenderCode}${cfg.elementDelimiter}")
    sb.append(s"${gs.applicationReceiverCode}${cfg.elementDelimiter}")
    sb.append(s"${gs.date}${cfg.elementDelimiter}")
    sb.append(s"${gs.time}${cfg.elementDelimiter}")
    sb.append(s"${gs.groupControlNumber}${cfg.elementDelimiter}")
    sb.append(s"${gs.responsibleAgencyCode}${cfg.elementDelimiter}")
    sb.append(s"${gs.versionReleaseIndustryCode}${cfg.segmentDelimiter}")
    sb

  private def emitGE(cfg: TokenizerConfig, sb: StringBuilder, gs: GsSegment): StringBuilder =
    sb.append(s"GE${cfg.elementDelimiter}${gs.transactions.length}${cfg.elementDelimiter}${gs.groupControlNumber}${cfg.segmentDelimiter}")
    sb

  private def emitIEA(cfg: TokenizerConfig, sb: StringBuilder, isa: IsaSegment): StringBuilder =
    sb.append(s"IEA${cfg.elementDelimiter}${isa.groupSets.length}${cfg.elementDelimiter}${isa.controlNumber}${cfg.segmentDelimiter}")
    sb

  private def emitSt(cfg: TokenizerConfig, sb: StringBuilder, st: StSegment): StringBuilder =
    sb.append(s"ST${cfg.elementDelimiter}${st.transactionSetIdCode}${cfg.elementDelimiter}${st.transactionSetControlNumber}${cfg.segmentDelimiter}")
    sb

  private def emitSe(cfg: TokenizerConfig, sb: StringBuilder, st: StSegment): StringBuilder =
    sb.append(s"SE${cfg.elementDelimiter}${st.body.length}${cfg.elementDelimiter}${st.transactionSetControlNumber}${cfg.segmentDelimiter}")
    sb

  private def emit(cfg: TokenizerConfig, sb: StringBuilder, bodySeg: SegmentX12Token): StringBuilder =
    sb.append(bodySeg.name + cfg.elementDelimiter)
    val maxFields = bodySeg.fields.length-1
    bodySeg.fields.zipWithIndex.foreach {
      case (s: SimpleX12Token,i) =>
        sb.append(s.value)
        if i == maxFields then sb.append(cfg.segmentDelimiter) else sb.append(cfg.elementDelimiter)
      case (s: EmptyX12Token,i) =>
        if i == maxFields then sb.append(cfg.segmentDelimiter) else sb.append(cfg.elementDelimiter)
      case (s: RepeatedX12Token,i) =>
        sb.append(s.value.mkString(cfg.repeatDelimiter.toString))
        if i == maxFields then sb.append(cfg.segmentDelimiter) else sb.append(cfg.elementDelimiter)
      case (s: CompositeX12Token,i) =>
        sb.append(s.value.mkString(cfg.componentDelimiter.toString))
        if i == maxFields then sb.append(cfg.segmentDelimiter) else sb.append(cfg.elementDelimiter)
    }
    sb