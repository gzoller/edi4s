package co.blocke.edi4s
package tokenizer

import zio.*
import model.*
import scala.annotation.tailrec

object X12Parser:

  def parse( doc: String, config: TokenizerConfig ): ZIO[Any, X12ParseError, (IsaSegment, TokenizerConfig)] =

    @tailrec
    def skipNewlines(i: Int): Int =
      if i < doc.length && (doc.charAt(i) == '\n' || doc.charAt(i) == '\r') then
        skipNewlines(i + 1)
      else
        i

    // pre-parse to determine delimiters
    val parseConfig = config.copy(
      segmentDelimiter = doc.charAt(105),    // ~
      elementDelimiter = doc.charAt(3),      // *
      componentDelimiter = doc.charAt(104)   // :
    )

    // parse ISA
    val fields = doc.substring(0, 105).split("\\"+parseConfig.elementDelimiter, -1)
    val endOfIsa = doc.indexOf('~')
    if fields.length != 17 || fields(0) != "ISA" then
      ZIO.fail(X12ParseError("Malformed ISA segment"))
    else if endOfIsa == -1 then
      ZIO.fail(X12ParseError("Missing segment delimiter (~) after ISA"))
    else if endOfIsa != 105 then
      ZIO.fail(X12ParseError("ISA segment is not the required 106 chars long"))
    else
      for {
        (segments, offset) <- X12Tokenizer.tokenize(doc, endOfIsa+1, parseConfig)
        groups <- parseGsSegments(segments)
        isa = IsaSegment(
          authorizationQualifier = fields(1),
          authorizationInfo      = fields(2),
          securityQualifier      = fields(3),
          securityInfo           = fields(4),
          senderQualifier        = fields(5),
          senderId               = fields(6),
          receiverQualifier      = fields(7),
          receiverId             = fields(8),
          interchangeDate        = fields(9),
          interchangeTime        = fields(10),
          repetitionSeparator    = fields(11).head,
          version                = fields(12),
          controlNumber          = fields(13),
          ackRequested           = fields(14).toInt,
          usageIndicator         = fields(15).head,
          componentSeparator     = fields(16).head,
          groups
        )

        // Parse and validate IEA
        _ <- segments.last match
          case SegmentX12Token("IEA", SimpleX12Token(_, _) :: SimpleX12Token(_, controlNumber) :: _) =>
            if controlNumber.trim != isa.controlNumber.trim then
              ZIO.fail(X12ParseError(s"ISA13 (${isa.controlNumber.trim}) does not match IEA02 ($controlNumber)"))
            else ZIO.unit
          case other =>
            ZIO.fail(X12ParseError(s"Expected well-formed IEA segment but got $other"))

      } yield (isa, parseConfig)


  private def parseGsSegments(segs: List[SegmentX12Token]): ZIO[Any, X12ParseError, List[GsSegment]] =
    def loop(remaining: List[SegmentX12Token], acc: List[GsSegment]): ZIO[Any, X12ParseError, (List[GsSegment], List[SegmentX12Token])] =
      remaining match
        case SegmentX12Token("GS", _) :: _ =>
          for {
            (gs, rest) <- parseGsSegment(remaining)
            result <- loop(rest, acc :+ gs)
          } yield result
        case _ =>
          ZIO.succeed((acc, remaining)) // Done parsing GS segments
    loop(segs, Nil).map(_._1)


  private def getValue(t: X12Token): ZIO[Any, X12ParseError, String] =
    t match {
      case SimpleX12Token(_, v) => ZIO.succeed(v)
      case x => ZIO.fail(X12ParseError(s"Expected a simple (single) X12 field but got something else: $x"))
    }

  private def parseGsSegment(segs: List[SegmentX12Token]): ZIO[Any, X12ParseError, (GsSegment, List[SegmentX12Token])] =
    segs match {
      case SegmentX12Token("GS", gsValue) :: rest =>
        // Find index of first SE segment
        val geIndex = rest.indexWhere(_.name == "GE")
        if geIndex == -1 then
          ZIO.fail(X12ParseError("No GE segment found after GS"))
        else
          val body = rest.take(geIndex) // tokens between GS and GE
          val ge = rest(geIndex) // the GE segment
          val tail = rest.drop(geIndex + 1) // everything after GE
          ge match {
            case SegmentX12Token("GE", seValue) =>
              for {
                functionalIdCode <- getValue(gsValue.head)
                applicationSenderCode <- getValue(gsValue(1))
                applicationReceiverCode <- getValue(gsValue(2))
                date <- getValue(gsValue(3))
                time <- getValue(gsValue(4))
                groupControlNumber <- getValue(gsValue(5))
                responsibleAgencyCode <- getValue(gsValue(6))
                versionReleaseIndustryCode <- getValue(gsValue(7))

                seGroupControlNumber <- getValue(seValue(1))
                _ <- if seGroupControlNumber == groupControlNumber then ZIO.unit
                else ZIO.fail(X12ParseError("GS/GE segments have different control numbers (GS06/GE02)"))

                gsBody <- parseStSegments(body)
                result = GsSegment(
                  functionalIdCode,
                  applicationSenderCode,
                  applicationReceiverCode,
                  date,
                  time,
                  groupControlNumber,
                  responsibleAgencyCode.charAt(0),
                  versionReleaseIndustryCode,
                  gsBody
                )
              } yield (result, tail)
            case _ => ZIO.fail(X12ParseError("Expected GE segment but received somethine else"))
          }
      case _ => ZIO.fail(X12ParseError("Expected GS segment but received somethine else"))
    }

  private def parseStSegments(segs: List[SegmentX12Token]): ZIO[Any, X12ParseError, List[StSegment]] =
    def loop(remaining: List[SegmentX12Token], acc: List[StSegment]): ZIO[Any, X12ParseError, (List[StSegment], List[SegmentX12Token])] =
      remaining match
        case SegmentX12Token("ST", _) :: _ =>
          for {
            (st, rest) <- parseStSegment(remaining)
            result <- loop(rest, acc :+ st)
          } yield result
        case _ =>
          ZIO.succeed((acc, remaining)) // Done parsing GS segments

    loop(segs, Nil).map(_._1)

  private def parseStSegment(segs: List[SegmentX12Token]): ZIO[Any, X12ParseError, (StSegment, List[SegmentX12Token])] =
    segs match {
      case SegmentX12Token("ST", stValue) :: rest =>
        // Find index of first SE segment
        val seIndex = rest.indexWhere(_.name == "SE")
        if seIndex == -1 then
          ZIO.fail(X12ParseError("No SE segment found after ST"))
        else
          val body = rest.take(seIndex) // tokens between ST and SE
          val se = rest(seIndex) // the SE segment
          val tail = rest.drop(seIndex + 1) // everything after SE
          se match {
            case SegmentX12Token("SE", seValue) =>
              for {
                transactionSetIdCode <- getValue(stValue.head)
                transactionSetControlNumber <- getValue(stValue(1))
                _ <- seValue.lift(1) match {
                  case Some(SimpleX12Token(_, seCtrlNumber)) =>
                    if seCtrlNumber == transactionSetControlNumber then ZIO.unit
                    else ZIO.fail(X12ParseError("ST/SE segments have different control numbers (ST02/SE02)"))
                  case Some(_) => ZIO.fail(X12ParseError("Expected SE02 (simple segment) but got something else"))
                  case None => ZIO.fail(X12ParseError("SE02 missing"))
                }
                result = StSegment(transactionSetIdCode, transactionSetControlNumber, body)
              } yield (result, tail)
            case _ => ZIO.fail(X12ParseError("SE segment not found")) // should never happen
          }
      case _ => ZIO.fail(X12ParseError(s"Expected ST-(body)-SE, but found something else"))
    }