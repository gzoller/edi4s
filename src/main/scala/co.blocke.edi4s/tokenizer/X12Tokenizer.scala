package co.blocke.edi4s
package tokenizer

import model.*
import zio.*
import scala.annotation.tailrec


enum FieldStatus:
  case PRESENT, EMPTY, END_OF_SEGMENT, COMPONENT


trait X12Tokenizer:

  def tokenizeOneElement(tc: TokenizerContext): ZIO[Any, Throwable, (String, TokenizerContext, FieldStatus)] = {

    inline def consumeWhitespace(ctx: TokenizerContext): TokenizerContext =
      var i = ctx.offset
      while i < ctx.doc.length && ctx.doc.charAt(i).isWhitespace do i += 1
      ctx.copy(offset = i)

    @tailrec
    def loop(
              currentOffset: Int,
              sb: StringBuilder,
              isEscape: Boolean,
              isFirst: Boolean = false,
            ): ZIO[Any, Throwable, (String, Int, FieldStatus)] =
      if (currentOffset >= tc.doc.length) {
        // End of document reached
        ZIO.fail(new UnexpectedEndOfData("Missing segment end token"))
      } else {
        tc.doc.charAt(currentOffset) match {
          case tc.config.escapeCharacter =>
            loop(currentOffset + 1, sb, true)
          case c if isEscape =>
            sb.append(c)
            loop(currentOffset + 1, sb, false)

          case tc.config.componentDelimiter =>
            ZIO.succeed((sb.toString, currentOffset + 1, FieldStatus.COMPONENT))

          case tc.config.elementDelimiter if isFirst =>
            if currentOffset+1 < tc.doc.length && tc.doc.charAt(currentOffset+1) == tc.config.elementDelimiter then
              ZIO.succeed(("", currentOffset+2, FieldStatus.EMPTY))
            else
              ZIO.succeed(("", currentOffset+1, FieldStatus.EMPTY))
          case tc.config.elementDelimiter =>
            if sb.isEmpty then
              ZIO.succeed((sb.toString, currentOffset + 1, FieldStatus.EMPTY))
            else
              ZIO.succeed((sb.toString, currentOffset + 1, FieldStatus.PRESENT))

          case tc.config.segmentDelimiter =>
            ZIO.succeed((sb.toString, currentOffset + 1, FieldStatus.END_OF_SEGMENT))

          case c =>
            sb.append(c)
            loop(currentOffset + 1, sb, false)
        }
      }

    for {
      // Start the recursive loop with the initial offset and an empty StringBuilder.
      result <- loop(tc.offset, new StringBuilder(), false, true)
      (value, newOffset, status) = result
    } yield (value, consumeWhitespace(tc.copy(offset = newOffset)), status)
  }


object SegmentTokenizer extends X12Tokenizer:

  // By the time this is called, DocumentParser has consumed the segment name and delimiter ('*'). We parse from there...
  def tokenize(tc: TokenizerContext): ZIO[Any, Throwable, (SegmentX12Token, TokenizerContext)] =
    inline def mkElement(chunk: String): X12Token =
      if chunk.isEmpty then
        EmptyX12Token("")
      else
        SimpleX12Token("", chunk)

    def loop(
              acc: List[X12Token],
              current: TokenizerContext,
              inComposite: Boolean = false,
              compositeAcc: List[X12Token] = Nil
            ): ZIO[Any, Throwable, (List[X12Token], TokenizerContext)] =
      for {
        (chunk, nextPC, status) <- tokenizeOneElement(current)
        result <- status match {
          case FieldStatus.END_OF_SEGMENT =>
            if inComposite then
              val composites = compositeAcc :+ mkElement(chunk)
              ZIO.succeed(acc :+ CompositeX12Token("",composites), nextPC)
            else
              ZIO.succeed(acc :+ mkElement(chunk), nextPC)
          case FieldStatus.EMPTY =>
            if inComposite then
              val composites = compositeAcc :+ EmptyX12Token("")
              loop(acc :+ CompositeX12Token("",composites), nextPC)
            else
              loop(acc :+ EmptyX12Token(""), nextPC)
          case FieldStatus.PRESENT =>
            if inComposite then
              val composites = compositeAcc :+ SimpleX12Token("", chunk)
              loop(acc :+ CompositeX12Token("",composites), nextPC)
            else
              loop(acc :+ SimpleX12Token("", chunk), nextPC)
          case FieldStatus.COMPONENT => loop(acc, nextPC, true, compositeAcc :+ mkElement(chunk))
        }
      } yield result
    loop(Nil, tc).flatMap{ (elements,nextPc) =>
      elements match {
        case (head:SimpleX12Token) :: tail => ZIO.succeed( (SegmentX12Token(head.value, tail), nextPc) )
        case _ => ZIO.fail(new Exception("boom: "+elements))
      }
    }


object X12Tokenizer extends X12Tokenizer:
  def tokenize(tc: TokenizerContext): ZIO[Any, Throwable, (X12TokenDocument, TokenizerContext)] =
    def loop(
              acc: List[SegmentX12Token],
              current: TokenizerContext
            ): ZIO[Any, Throwable, (List[SegmentX12Token], TokenizerContext)] =
      for {
        (seg, nextPc) <- SegmentTokenizer.tokenize(current)
        result <- if nextPc.offset >= nextPc.doc.length then
          ZIO.succeed( (acc :+ seg, nextPc) )
        else
          loop(acc :+ seg, nextPc)
      } yield result
    loop(Nil, tc).flatMap( (segments, nextPc) => ZIO.succeed( (X12TokenDocument(segments), nextPc)))
