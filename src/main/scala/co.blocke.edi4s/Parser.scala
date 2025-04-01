package co.blocke.edi4s

import zio.*
import scala.annotation.tailrec


enum FieldStatus:
  case PRESENT, EMPTY, END_OF_SEGMENT, END_OF_COMPONENT


trait Parser:

  def parseOneElement(pc: ParseContext): ZIO[Any, Throwable, (String, ParseContext, FieldStatus)] = {

    @tailrec
    def loop(
              currentOffset: Int,
              sb: StringBuilder,
              isEscape: Boolean,
              isFirst: Boolean = false,
            ): ZIO[Any, Throwable, (String, Int, FieldStatus)] =
      if (currentOffset >= pc.doc.length) {
        // End of document reached
        ZIO.fail(new UnexpectedEndOfData("Missing segment end token"))
      } else {
        pc.doc.charAt(currentOffset) match {
          case pc.config.escapeCharacter =>
            loop(currentOffset + 1, sb, true)
          case c if isEscape =>
            sb.append(c)
            loop(currentOffset + 1, sb, false)

          case pc.config.componentDelimiter =>
            ZIO.succeed((sb.toString, currentOffset + 1, FieldStatus.END_OF_COMPONENT))

          case pc.config.elementDelimiter if isFirst =>
            if currentOffset+1 < pc.doc.length && pc.doc.charAt(currentOffset+1) == pc.config.elementDelimiter then
              ZIO.succeed(("", currentOffset+2, FieldStatus.EMPTY))
            else
              ZIO.succeed(("", currentOffset+1, FieldStatus.EMPTY))
          case pc.config.elementDelimiter =>
            if sb.isEmpty then
              ZIO.succeed((sb.toString, currentOffset + 1, FieldStatus.EMPTY))
            else
              ZIO.succeed((sb.toString, currentOffset + 1, FieldStatus.PRESENT))

          case pc.config.segmentDelimiter =>
            ZIO.succeed((sb.toString, currentOffset + 1, FieldStatus.END_OF_SEGMENT))

          case c =>
            sb.append(c)
            loop(currentOffset + 1, sb, false)
        }
      }

    for {
      // Start the recursive loop with the initial offset and an empty StringBuilder.
      result <- loop(pc.offset, new StringBuilder(), false, true)
      (value, newOffset, status) = result
    } yield (value, pc.copy(offset = newOffset), status)
  }