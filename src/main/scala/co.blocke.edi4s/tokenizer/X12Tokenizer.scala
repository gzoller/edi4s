package co.blocke.edi4s
package tokenizer

import model.*
import zio.*
import scala.annotation.tailrec


enum FieldStatus:
  case PRESENT, EMPTY, END_OF_SEGMENT, COMPONENT, REPEATED


object X12Tokenizer:

  private def tokenizeOneElement(
                          doc: String,
                          offset: Int,
                          config: TokenizerConfig
                        ): ZIO[Any, X12ParseError, (String, Int, FieldStatus)] =

    // Skip whitespace characters (spaces, \r, \n, \t) before parsing
    @tailrec
    def skipNewlines(i: Int): Int =
      if i < doc.length && (doc.charAt(i) == '\n' || doc.charAt(i) == '\r') then
        skipNewlines(i + 1)
      else
        i

    @tailrec
    def loop(i: Int, sb: StringBuilder, isEscape: Boolean = false, isFirst: Boolean = false): ZIO[Any, X12ParseError, (String, Int, FieldStatus)] =
      if i >= doc.length then ZIO.fail(X12ParseError("Unexpected end of input"))
      else
        val ch = doc.charAt(i)
        if isEscape then
          sb.append(ch)
          loop(i + 1, sb, false)
        else ch match
          case c if c == config.escapeCharacter =>
            loop(i + 1, sb, true)
          case c if c == config.componentDelimiter =>
            ZIO.succeed((sb.toString, i + 1, FieldStatus.COMPONENT))
          case c if c == config.elementDelimiter && isFirst =>
            val empty = if i + 1 < doc.length && doc.charAt(i + 1) == config.elementDelimiter then "" else ""
            ZIO.succeed((empty, i + 1, FieldStatus.EMPTY))
          case c if c == config.elementDelimiter =>
            val status = if sb.isEmpty then FieldStatus.EMPTY else FieldStatus.PRESENT
            ZIO.succeed((sb.toString, i + 1, status))
          case c if c == config.segmentDelimiter =>
            ZIO.succeed((sb.toString, i + 1, FieldStatus.END_OF_SEGMENT))
          case c if c == config.repeatDelimiter =>
            ZIO.succeed((sb.toString, i+1, FieldStatus.REPEATED))
          case c =>
            sb.append(c)
            loop(i + 1, sb, false)

    loop(skipNewlines(offset), new StringBuilder(), false, true)

  private def tokenizeSegment(
    doc: String,
    offset: Int,
    config: TokenizerConfig
  ): ZIO[Any, X12ParseError, (SegmentX12Token, Int)] =

    def parseComposite(parentName: String, index: Int, offset: Int, firstElement: String): ZIO[Any, X12ParseError, (CompositeX12Token, Int, FieldStatus)] =
      def loop(i: Int, acc: List[X12Token], currentOffset: Int): ZIO[Any, X12ParseError, (List[X12Token], Int, FieldStatus)] =
        tokenizeOneElement(doc, currentOffset, config).flatMap { (chunk, nextOffset, status) =>
          val compName = f"$parentName${index}%02d${acc.length + 1}%02d"
          val token = if chunk.isEmpty then EmptyX12Token(compName) else SimpleX12Token(compName, chunk)
          status match
            case FieldStatus.COMPONENT =>
              loop(i + 1, acc :+ token, nextOffset)
            case _ =>
              ZIO.succeed((acc :+ token, nextOffset, status))
        }

      val compName = f"$parentName${index}%02d01"
      val token = if firstElement.isEmpty then EmptyX12Token(compName) else SimpleX12Token(compName, firstElement)
      loop(2, List(token), offset).map { (comps, nextOffset, status) =>
        (CompositeX12Token(f"$parentName${index}%02d", comps), nextOffset, status)
      }

    def parseRepeated(
                       parentName: String,
                       index: Int,
                       offset: Int,
                       firstChunk: String
                     ): ZIO[Any, X12ParseError, (RepeatedX12Token, Int, FieldStatus)] = {
      val fieldName = f"$parentName${index}%02d"
      def loop(
                values: List[String],
                currentOffset: Int
              ): ZIO[Any, X12ParseError, (List[String], Int, FieldStatus)] = {
        tokenizeOneElement(doc, currentOffset, config).flatMap {
          case (chunk, nextOffset, FieldStatus.REPEATED) =>
            loop(values :+ chunk, nextOffset)

          case (chunk, nextOffset, status) =>
            // Last value — stop
            ZIO.succeed((values :+ chunk, nextOffset, status))
        }
      }
      loop(List(firstChunk), offset).map { (allValues, nextOffset, trailingStatus) =>
        (RepeatedX12Token(fieldName, allValues), nextOffset, trailingStatus)
      }
    }

    def loop(
      index: Int,
      offset: Int,
      acc: List[X12Token],
      segmentName: String
    ): ZIO[Any, X12ParseError, (List[X12Token], Int)] =
      val fieldName = segmentName + f"$index%02d"
      tokenizeOneElement(doc, offset, config).flatMap {
        case (chunk, nextOffset, status) =>
          status match
            case FieldStatus.END_OF_SEGMENT =>
              val token = if chunk.isEmpty then EmptyX12Token(fieldName) else SimpleX12Token(fieldName, chunk)
              ZIO.succeed(acc :+ token, nextOffset)

            case FieldStatus.EMPTY =>
              loop(index + 1, nextOffset, acc :+ EmptyX12Token(fieldName), segmentName)

            case FieldStatus.PRESENT =>
              loop(index + 1, nextOffset, acc :+ SimpleX12Token(fieldName, chunk), segmentName)

            case FieldStatus.REPEATED =>
              parseRepeated(segmentName, index, nextOffset, chunk).flatMap {
                case (composite, next, FieldStatus.END_OF_SEGMENT) =>
                  ZIO.succeed(acc :+ composite, next)
                case (composite, next, _) =>
                  loop(index + 1, next, acc :+ composite, segmentName)
              }

            case FieldStatus.COMPONENT =>
              parseComposite(segmentName, index, nextOffset, chunk).flatMap {
                case (composite, next, FieldStatus.END_OF_SEGMENT) =>
                  ZIO.succeed(acc :+ composite, next)
                case (composite, next, _) =>
                  loop(index + 1, next, acc :+ composite, segmentName)
              }
      }

    for {
      (firstChunk, offsetAfter1, _) <- tokenizeOneElement(doc, offset, config)
      segmentName = firstChunk
      (tokens, finalOffset) <- loop(1, offsetAfter1, Nil, segmentName)
    } yield SegmentX12Token(segmentName, tokens) -> finalOffset

  // Depending on input, this may return a fully-formed/wrapped X12 doc (ie with ISA)
  // or just a single ST block
  def tokenize(
                doc: String,
                offset: Int,
                config: TokenizerConfig
              ): ZIO[Any, X12ParseError, (List[SegmentX12Token], Int)] =
    def loop(
              acc: List[SegmentX12Token],
              offset: Int
            ): ZIO[Any, X12ParseError, (List[SegmentX12Token], Int)] =
      for {
        (seg, nextOffset) <- tokenizeSegment(doc, offset, config)
        result <-
          if nextOffset >= doc.length then
            ZIO.succeed((acc :+ seg, nextOffset))
          else
            loop(acc :+ seg, nextOffset)
      } yield result

    loop(Nil, offset)

