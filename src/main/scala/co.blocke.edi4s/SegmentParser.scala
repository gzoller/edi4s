package co.blocke.edi4s

import model.*
import zio._
import scala.annotation.tailrec


// Trait for segment parsers
trait SegmentParser extends Parser:
  def parse(pc: ParseContext, segment: SegmentSpec): ZIO[Any, Throwable, (List[Element], ParseContext)]


/*
// Updated LoopingSegmentParser with ListElement
class LoopingSegmentParser(
                                 segmentName: String,    // The name of the loop segment (e.g., "N1")
                                 segmentParser: SegmentParser,  // Parser for the loop segment itself (e.g., "N1")
                                 bodySegmentParsers: List[SegmentParser] // Parsers for the body segments that follow (e.g., "N3", "N4")
                               ) extends SegmentParser {

  def parse(pc: ParseContext): ZIO[Any, Throwable, (List[Element], ParseContext)] = {
    var loopElements = List.empty[Element] // Accumulated result list for the loop

    // Start parsing the loop
    def parseLoop(parseContext: ParseContext): ZIO[Any, Throwable, (List[Element], ParseContext)] = {
      for {
        // Step 1: Parse the loop segment (e.g., N1)
        result <- segmentParser.parse(parseContext).mapError(e => new Exception(s"Error parsing segment $segmentName: ${e.getMessage}", e))

        // Step 2: Destructure the result to get loop segment fields and updated context
        (loopSegmentFields, updatedContext) = result

        // Step 3: Parse body segments (e.g., N3, N4) for this loop iteration
        bodySegmentFields <- bodySegmentParsers.foldLeft(ZIO.succeed(List.empty[Element])) { (acc, parser) =>
          for {
            parsedFields <- acc
            (fields, newContext) <- parser.parse(updatedContext)
          } yield parsedFields ++ fields // Accumulate body segment fields
        }

        // Combine loop segment and body segment fields into a single list
        combinedFields = loopSegmentFields ++ bodySegmentFields

        // Add this iteration's fields to the loop elements
        loopElementsUpdated = loopElements :+ ListElement(combinedFields)

        // Step 4: Check if there is another loop segment
        (nextToken, nextContext) = updatedContext.getNextToken // Get next token and context

        // If the next token is the same segment name, continue parsing the next iteration
        isEndOfLoop = nextToken != segmentName // Check if we're at the end of the loop

        // If end of loop, return the accumulated results along with the final context
        result <- if (isEndOfLoop) ZIO.succeed((loopElementsUpdated, updatedContext)) // Return both elements and updated context
        else parseLoop(nextContext) // Recursively continue parsing the next iteration
      } yield result
    }

    parseLoop(pc) // Start parsing with the initial context
  }
}
  */


class SimpleSegmentParser(segmentName: String) extends SegmentParser:

  // By the time this is called, DocumentParser has consumed the segment name and delimiter ('*'). We parse from there...
  def parse(pc: ParseContext, segment: SegmentSpec): ZIO[Any, Throwable, (List[Element], ParseContext)] =

    def requiredInRest(expected: List[SegmentFieldSpec]): List[String] =
      expected.collect {
        case spec if spec.presence == "required" => spec.name
      }

    def loop(expected: List[SegmentFieldSpec],
             acc: List[Element],
             current: ParseContext
            ): ZIO[Any, Throwable, (List[Element], ParseContext)] =
      expected match
        case Nil =>
          // All expected fields have been processed.
          ZIO.succeed((acc, current))
        case fieldSpec :: tail =>
          parseOneElement(current).flatMap { case (parsedValue, nextContext, status) =>
            fieldSpec.presence match
              case "required" =>
                status match {
                  case FieldStatus.EMPTY =>
                    ZIO.fail(new Exception(s"Required field '${fieldSpec.name}' is missing or empty"))
                  case FieldStatus.END_OF_SEGMENT =>
                    val element = Simple(parsedValue, fieldSpec.name)
                    val rest = requiredInRest(tail)
                    if rest.nonEmpty then
                      ZIO.fail(new Exception(s"Required field(s) '${rest.mkString(",")}' is missing or empty"))
                    else
                      ZIO.succeed((acc :+ element, nextContext))
                  case _ => // TODO: Handle composites
                    val element = Simple(parsedValue, fieldSpec.name)
                    loop(tail, acc :+ element, nextContext)
                }
              case "optional" =>
                status match {
                  case FieldStatus.EMPTY =>
                    val element = OptionalSimple(None, fieldSpec.name)
                    loop(tail, acc :+ element, nextContext)
                  case FieldStatus.END_OF_SEGMENT =>
                    val element = OptionalSimple(Some(parsedValue), fieldSpec.name)
                    val rest = requiredInRest(tail)
                    if rest.nonEmpty then
                      ZIO.fail(new Exception(s"Required field(s) '${rest.mkString(",")}' is missing or empty"))
                    else
                      ZIO.succeed((acc :+ element, nextContext))
                  case _ => // TODO: Handle composites
                    val element = OptionalSimple(Some(parsedValue), fieldSpec.name)
                    loop(tail, acc :+ element, nextContext)
                }
              case other =>
                ZIO.fail(new Exception(s"Unknown presence value '$other' for field '${fieldSpec.name}'"))
          }
    loop(segment.fields, Nil, pc)



/*

  Type Code	Name	Description
AN	Alphanumeric String	Letters, digits, and special characters (printable ASCII). Left-justified, space-filled if shorter than max length.
ID	Identifier	Code value from a predefined list. Typically used for qualifiers, e.g., "PO" for Purchase Order.
Nn	Numeric String (implied decimal)	Numeric digits only. Optional sign. Decimal is implied by the n (e.g., N2 means 2 decimal places).
R	Real Number	Decimal numbers with explicit decimal points. No separators or currency symbols.
DT	Date	CCYYMMDD (usually 8 digits). Sometimes YYMMDD in older versions.
TM	Time	HHMM, HHMMSS, or HHMMSSD. 24-hour format, no colons.
B	Binary	Binary data, often Base64 or hex-encoded. Rare in most business EDI documents.
TX	Text	Free-form text of variable length. Allows wider character ranges than AN.

*/