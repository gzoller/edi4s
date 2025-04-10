package co.blocke.edi4s

import zio._
import zio.nio.file.{Files, Path}
import model.*
import tokenizer.*
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

  val sampleEDI10 =
    """REF*IA*TF-12345~
      |REF*IV*SO-9876~
      |HL*1**S~
      |HL*2*1*I~
      |LIN**CH*US~
      |SN1**168*EA~
      |HL*3*1*I~
      |LIN**CH*CN~
      |SN1**99*EA~""".stripMargin
    //"ST*855*0001*0002*4010~\n  ST*855*a:b:c**x~"

  // Start the loop
  def run = {
    val tests = List(
      sampleEDI1,
      sampleEDI2,
      sampleEDI3,
      sampleEDI4,
      sampleEDI5,
      sampleEDI6,
      sampleEDI7,
      sampleEDI8,
      sampleEDI9,
      sampleEDI10
    )

    ZIO.foreach(tests){ txt =>
      println(txt)
      val tokenizerContext = TokenizerContext(doc = txt,offset = 0, config = TokenizerConfig())
      X12Tokenizer.tokenize(tokenizerContext).flatMap((result, nextPc) => ZIO.succeed(println("   >>> "+result+"\n")))
    }

    val filePath = Path("doc856_v5010.json")
    for {
      lines <- Files.readAllLines(filePath)
      edi <- CanonicalParser.readSpec(lines.mkString("\n"))
      pid = edi.components.schemas("PID")
//      def toRefined( edi: EdiObject, topLevel: String, document: String, version: String, partner: String ): ZIO[Any, CanonicalError, RefinedDocumentSpec]
      _ <- CanonicalParser.toRefined(edi, "TS856", "856", "5010", "Taylor Farms")
//      _ <- ZIO.succeed(println("RESULTS: \n"+ CanonicalParser.show(edi,"TS856"))) /*edi.components.schemas.collect {
//        case (x,y: EdiSchema) => x
//      }))
    } yield ()

  }
}