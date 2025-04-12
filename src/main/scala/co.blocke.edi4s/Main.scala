package co.blocke.edi4s

import zio._
import zio.nio.file.{Files, Path}
import model.*
import tokenizer.*

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

    import CanonicalParser.given


    /*
    val a = FieldDifference("ST01x","ST01", None, Some((true,false)), Some(("string","integer")),None,None,None,Some((None,Some("someref"))))
    val b = FieldDifference("Foo","ST0201", None, Some((true,false)), Some(("string","integer")),None,None,None,Some((None,Some("someref"))))
    val c = FieldDifference("Bar","ST0202", None, Some((true,false)), Some(("string","integer")),None,None,None,Some((None,Some("someref"))))
    val d = FieldDifference("Hi","ST0203", presence = Some((true,false)))
    val e = FieldDifference("Bye","ST0204", presence = Some((false,true)))
    val cc = CompositeFieldDifference("blah","ST02", List(b,c,d,e))
    val cells = a.render("ST",Nil) ++ cc.render("ST",Nil)
//      ++ List(List("ST.ST03","!present in source - missing in target"))
//      ++ List(List("ST.ST04", "!present in source - missing in target"))
    val t = Table(
      "EDI 856 Specification Differences",
      List("Difference","Source","Target"),
      List(35,40,40),
      cells,
      "color_text"
    )
    ZIO.succeed(println(t.toString))
     */
//    val filePath = Path("doc856_v5010.json")
//    refined <- CanonicalParser.toRefined(edi, "TS856", "856", "5010", "Taylor Farms")


    def loadCanonicalSpec(path: String, topLevel: String, document: String, version: String, partner: String): ZIO[Any, CanonicalError, RefinedDocumentSpec] =
      val filePath = Path(path)
      (for {
        lines <- Files.readAllLines(filePath)
        edi <- CanonicalParser.readSpec(lines.mkString("\n"))
        refined <- CanonicalParser.toRefined(edi, topLevel, document, version, partner)
      } yield refined)
        .mapError{
          case ioe: java.io.IOException => CanonicalError(ioe.getMessage)
        }

    def loadRefinedSpec(path: String): ZIO[Any, CanonicalError, RefinedDocumentSpec] =
      val filePath = Path(path)
      (for {
        lines <- Files.readAllLines(filePath)
        refined = sjRefinedSpec.fromJson(lines.mkString("\n"))
      } yield refined)
        .mapError {
          case ioe: java.io.IOException => CanonicalError(ioe.getMessage)
        }

    for {
      std <- loadRefinedSpec("x12_856_5010.json")
      tf <- loadRefinedSpec("tf_856_5010.json")
      diffs <- DifferenceEngine.computeDifference(std, tf)
      table = DifferenceEngine.differenceTable(diffs,"EDI 856 Specification Differences")
      _ <- ZIO.succeed(println(table.toString))
    } yield ()


  }
}