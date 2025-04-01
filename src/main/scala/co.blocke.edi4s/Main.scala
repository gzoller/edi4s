package co.blocke.edi4s

import zio._
import model._

import scala.annotation.tailrec

/*
Key takeaways:
	•	**: A field is missing, but it’s not the last one. There are other fields following it, so it’s essentially a “gap” where a value should have been but wasn’t provided.
	•	*~: A field is present but empty. It’s in the data, but there’s no value between the delimiters.
	•	~~: The last field in the segment is missing. This could happen if the last field in the segment is omitted entirely.
*/

object Main extends ZIOAppDefault {

  val sampleEDI1 = "ST*855*0001*0002*4010~"
  // All fields are present and populated.
  // Field 1: "855", Field 2: "0001", Field 3: "0002", Field 4: "4010"

  val sampleEDI2 = "ST*855*0001**4010~"
  // Field 1: "855", Field 2: "0001", Field 3: missing (**), Field 4: "4010"

  val sampleEDI3 = "ST*855****4010~"
  // Field 1: "855", Field 2: "0001", Fields 3 and 4 are missing (**), Field 5: "4010"

  val sampleEDI4 = "ST*855*0001~"
  // Field 1: "855", Field 2: "0001", Field 3 and 4 truncated (missing)

  val sampleEDI5 = "ST*855*a:b:c**x~" // composite

  val sampleEDI6 = "ST*855*Foo\\*Bar\\:Blah\\~*x~"  // escape

  val sampleEDI7 = "ZZ*855*0001**4010~"
  val sampleEDI8 = "ZZ*855*0001~"

  val sampleEDI9 = "ST**0001*0002*4010~"

  private val parseContext = ParseContext(
    doc = sampleEDI2,
    docSpec = DocumentSpec("foo","bar",List.empty[SegmentSpec]),
    offset = 0,
    config = ParseConfig()
  )

  private val segment = SegmentSpec("ST","required",List(
    SegmentFieldSpec("ST01","required"),
    SegmentFieldSpec("ST02","optional"),
    SegmentFieldSpec("ST03","optional"),
    SegmentFieldSpec("ST04","optional"),
  ))
  private val segment2 = SegmentSpec("ZZ","required",List(
    SegmentFieldSpec("ZZ01","required"),
    SegmentFieldSpec("ZZ02","optional"),
    SegmentFieldSpec("ZZ03","optional"),
    SegmentFieldSpec("ZZ04","required"),
  ))

  private val parser = SimpleSegmentParser("ST")


//  def loopParse(pc: ParseContext, count: Int = 0): ZIO[Any, Throwable, Unit] =
//    if (count >= 15)
//      Console.printLine(s"Iteration limit reached after $count iterations.")
//    else
//      for {
//        // Parse one element using the current ParseContext
////        _ <- ZIO.succeed(println("Before: "+pc.offset))
//        (result, nextPc) <- parser.parse(pc, segment)
////        _ <- ZIO.succeed(println("After: "+nextPc.offset))
//        // Print the iteration number, the parsed value, and its status
//        _ <- Console.printLine(s"    Result: $result")
//        // Check if we should stop: either because the status indicates END_OF_SEGMENT,
//        // or continue with the updated ParseContext and increment the count
//        _ <- if (status == FieldStatus.END_OF_SEGMENT ) ZIO.unit
//        else loopParse(nextPc, count + 1)
//      } yield ()

  // Start the loop
  def run = {
    val tests = List(
      (sampleEDI1, segment),
      (sampleEDI2, segment),
      (sampleEDI3, segment),
      (sampleEDI4, segment),
      (sampleEDI5, segment),
      (sampleEDI6, segment),
      (sampleEDI7, segment2),
      (sampleEDI8, segment2),
      (sampleEDI9, segment)
    )

    ZIO.foreach(tests){ (txt, seg) =>
      println(txt)
      val parseContext = ParseContext(doc = txt, docSpec = DocumentSpec("foo","bar",List.empty[SegmentSpec]), offset = 3, config = ParseConfig())
      parser.parse(parseContext, seg).flatMap((result, nextPc) => ZIO.succeed(println("   >>> "+result+"\n")))
    }
  }
}

/*

trait SegNode:
  val value: String | List[SegNode]
case class StringNode(value: String) extends SegNode
case class ListNode(value: List[SegNode]) extends SegNode:
  def +(node: SegNode): ListNode = ListNode( value + node.value )

// doc: The document to be parsed
// requiredFields: Map[ segment_name, list_of_required_fields]
// offset: where to start parsing next character
case class ParseContext(doc: String, requiredFields: Map[String, List[Int]], offset: Long)

trait SegmentParser:
  def parse( pc: ParseContext ): IO[(SegNode, ParseContext)] // where T is a nested list structure

case class DocumentParser(segmentParsers: List[SegmentParser]):
  def parse( pc: ParseContext ): IO[SegNode] =
    segmentParser.foldLeft( (List.empty[String], pc))( case ((acc, ctx), segParser) => segParser.parse(ctx) )


case class EdiDocument(name: String, version: String, requiredFields: Map[String, List[Int]]):
  val parser = ... // Build parser aggregation here!
  def parse( doc: String ) = parser.parse( ParseContext(doc, requiredFields, 0) )
*/