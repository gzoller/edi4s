package co.blocke.edi4s

import co.blocke.edi4s.CanonicalParser.show
import zio.*
import zio.nio.file.{Files, Path}
import model.*
import tokenizer.*
import table.*

import java.io.FileNotFoundException

/*
Key takeaways:
	•	**: A field is missing, but it’s not the last one. There are other fields following it, so it’s essentially a “gap” where a value should have been but wasn’t provided.
	•	*~: A field is present but empty. It’s in the data, but there’s no value between the delimiters.
	•	~~: The last field in the segment is missing. This could happen if the last field in the segment is omitted entirely.
*/

object Main extends ZIOAppDefault {

  import CanonicalParser.given


  def loadCanonicalSpec(path: String, topLevel: String, document: String, version: String, partner: String): ZIO[Any, CanonicalError, RefinedDocumentSpec] =
    val filePath = Path(path)
    for {
      lines <- Files.readAllLines(filePath).mapError{
        case ioe: Throwable => CanonicalError("Can't read file: "+ioe.getMessage)
      }
      edi <- CanonicalParser.readSpec(lines.mkString("\n"))
      refined <- CanonicalParser.toRefined(edi, topLevel, document, version, partner)
    } yield refined

  def loadRefinedSpec(path: String): ZIO[Any, CanonicalError, RefinedDocumentSpec] =
    val filePath = Path(path)
    for {
      lines <- Files.readAllLines(filePath).mapError{
        case ioe: Throwable => CanonicalError("Can't read file: "+ioe.getMessage)
      }
      refined = sjRefinedSpec.fromJson(lines.mkString("\n"))
    } yield refined

  def run = {
    for {
      /*
      foo <- loadCanonicalSpec("specs/canonical/doc856_v4030.json", "TS856", "850", "4030", "ANSI")
      _ <- ZIO.succeed(println(sjRefinedSpec.toJson(foo)))
       */

      /* Un-comment to do a comparison run
      */

      tf <- loadRefinedSpec("specs/tf_856_5010.json")
      std <- loadRefinedSpec("specs/pfg_856_5010.json")
      result <- DiffEngine.compareSpecs(tf, std)
      titles = List(
          Title(List(Cell("📦 EDI Segment Comparison Report"))),
          Title(List(Cell("Taylor Farms -to- Core-Mark")))
        )
      header = Header(List(
          Cell("Segment"),
          Cell("Source (Taylor Farms)"),
          Cell("Target (Core-Mark")
        ))
      rows = result.foldLeft(List.empty[BodyRow]){ case (acc,diff) => acc ++ diff.render() }
//      _ <- ZIO.succeed(pprint.log(result))
      table = Table(
          title = titles,
          columns = 3,
          columnWidthPct = List(60, 30, 30),
          tableWidth = 200,
          header,
          rows
        )
      _ <- ZIO.succeed(println(table.toHtml))


      /*
      _ <- ZIO.succeed{
        val foo = result.map(_ match {
          case s: LoopSegmentDifference =>
//            if s.bodyDiff.isDefined then
//              s.bodyDiff.get.map(d => s.canonicalName+"."+d.path+"*"+d.canonicalName)
            if s.nested.isDefined then
              s.nested.get.map(n => s.canonicalName+"."+n.path+"&"+n.canonicalName)
            else List(s.path.toString+"."+s.canonicalName)
          case s: SimpleSegmentDifference =>
            List(s.path.toString+"*"+s.canonicalName)
          case _ => List("unknown")
        })
        println(foo.flatten.mkString("\n"))
      }
      */

    } yield ()
  }
}
      /*
      _ <- ZIO.succeed {
        val titles = List(
          Title(List(Cell("📦 EDI Segment Comparison Report")))
        )

        val header = Header(List(
          Cell("Segment"),
          Cell("Source"),
          Cell("Target")
        ))

        val rows = List(
          SubHeader(List(
            Cell("Header Information", style = Some(Style.SECONDARY))
          )),
          Row(List(
            Cell("ST01", indent = 1),
            Cell("850.25", align = Some(Align.DECIMAL)),
            Cell("850")
          )),
          Row(List(
            Cell("ST02", indent = 1),
            Cell("0001.1", align = Some(Align.DECIMAL)),
            Cell("0002", style = Some(Style.ALERT)) // Highlight difference
          )),
          SubHeader(List(
            Cell("Footer"),
            Cell("Stuff here...", style = Some(Style.MUTED))
          )),
          Row(List(
            Cell("SE01"),
            Cell("25"),
            Cell("25.50", align = Some(Align.DECIMAL))
          )),
          Row(List(
            Cell("SE02"),
            Cell("0001.444", align = Some(Align.DECIMAL)),
            Cell("0001", align = Some(Align.CENTER))
          )),
          Row(List(
            Cell("SE03"),
            Cell("0009.444", align = Some(Align.DECIMAL)),
            Cell("stuff", style = Some(Style.WARN), align = Some(Align.RIGHT))
          )),

          // --- New Colspan Examples ---

          SubHeader(List(
            Cell("Full-width notice", style = Some(Style.ALERT))
          )),

          Row(List(
            Cell("Some info spans 2 cols", colspan = 2),
            Cell("Final Col")
          )),

          Row(List(
            Cell("This is a nested summary", indent = 1, style = Some(Style.MUTED), colspan = 3)
          )),

          Row(List(
            Cell("Left cell", colspan = 2),
            Cell("Middle") // Spans two columns
          ))
        )

        val table = Table(
          title = titles,
          columns = 3,
          columnWidthPct = List(40, 30, 30),
          tableWidth = 100,
          header,
          rows
        )

        println(table.toHtml)
      }
       */


  /*
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
*/

    /*
    def walkSpec(
                  segments: List[RefinedSegmentSpec | RefinedLoopSpec],
                  path: String = ""
                ): Unit = {
      segments.foreach {
        case seg: RefinedSegmentSpec =>
          println(s"[Segment] $path.${seg.name}")
          walkFields(seg.fields, s"$path.${seg.name}")

        case loop: RefinedLoopSpec =>
          println(s"[Loop] $path.${loop.name}\n  └─ body size: ${loop.body.size}, nested size: ${loop.nested.map(_.size).getOrElse(0)}")
          walkFields(loop.fields, s"$path.${loop.name}")
          walkSpec(loop.body, s"$path.${loop.name}.body")
          loop.nested.foreach(n => walkSpec(n, s"$path.${loop.name}.nested"))
      }
    }

    def walkFields(
                    fields: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                    path: String
                  ): Unit = {
      fields.foreach {
        case field: RefinedSingleFieldSpec =>
          println(s"[Field] $path.${field.name} (single)")

        case comp: RefinedCompositeFieldSpec =>
          println(s"[Field] $path.${comp.name} (composite with ${comp.components.size} components)")
          comp.components.foreach { subfield =>
            println(s"  └─ [Subfield] $path.${comp.name}.${subfield.name}")
          }
      }
    }
     */

    /*
    def identifyGaps(source: List[String], target: List[String]): (List[String], List[String]) = {
      val inSourceOnly = source.filterNot(target.contains)
      val inTargetOnly = target.filterNot(source.contains)
      (inSourceOnly, inTargetOnly)
    }

    def walkSegments(
                      path: String,
                      src: List[RefinedSegmentSpec | RefinedLoopSpec],
                      tgt: List[RefinedSegmentSpec | RefinedLoopSpec]
                    ): Unit = {
      val srcNames = src.map(nameOf)
      val tgtNames = tgt.map(nameOf)
      val (inSrcOnly, inTgtOnly) = identifyGaps(srcNames, tgtNames)

      inSrcOnly.foreach(name => println(s"[GAP] Segment '$name' is only in source at $path"))
      inTgtOnly.foreach(name => println(s"[GAP] Segment '$name' is only in target at $path"))

      srcNames.intersect(tgtNames).foreach { name =>
        val s = src.find(nameOf(_) == name).get
        val t = tgt.find(nameOf(_) == name).get
        val newPath = s"$path.$name"
        (s, t) match {
          case (ss: RefinedSegmentSpec, ts: RefinedSegmentSpec) =>
            walkFields(newPath, ss.fields, ts.fields)

          case (sl: RefinedLoopSpec, tl: RefinedLoopSpec) =>
            walkFields(newPath, sl.fields, tl.fields)
            walkSegments(s"$newPath.body", sl.body, tl.body)

            val slNested = sl.nested.getOrElse(Nil)
            val tlNested = tl.nested.getOrElse(Nil)
            val slNames = slNested.map(nameOf)
            val tlNames = tlNested.map(nameOf)
            val (nestedSrcOnly, nestedTgtOnly) = identifyGaps(slNames, tlNames)

            nestedSrcOnly.foreach(n => println(s"[GAP] Nested loop '$n' only in source at $newPath"))
            nestedTgtOnly.foreach(n => println(s"[GAP] Nested loop '$n' only in target at $newPath"))

            slNames.intersect(tlNames).foreach { nestedName =>
              val sloop = slNested.find(nameOf(_) == nestedName).get
              val tloop = tlNested.find(nameOf(_) == nestedName).get
              walkSegments(s"$newPath.nested.$nestedName", List(sloop), List(tloop))
            }

          case _ => // shouldn't happen in well-formed specs
        }
      }
    }

    def walkFields(
                    path: String,
                    src: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec],
                    tgt: List[RefinedSingleFieldSpec | RefinedCompositeFieldSpec]
                  ): Unit = {
      val srcNames = src.map(nameOfField)
      val tgtNames = tgt.map(nameOfField)
      val (inSrcOnly, inTgtOnly) = identifyGaps(srcNames, tgtNames)

      inSrcOnly.foreach(name => println(s"[GAP] Field '$name' is only in source at $path"))
      inTgtOnly.foreach(name => println(s"[GAP] Field '$name' is only in target at $path"))

      srcNames.intersect(tgtNames).foreach { name =>
        val s = src.find(n => nameOfField(n) == name).get
        val t = tgt.find(n => nameOfField(n) == name).get
        val newPath = s"$path.$name"

        (s, t) match {
          case (sc: RefinedCompositeFieldSpec, tc: RefinedCompositeFieldSpec) =>
            walkFields(newPath, sc.components, tc.components)
          case _ => // skip singles
        }
      }
    }

    def nameOf(x: RefinedSegmentSpec | RefinedLoopSpec): String = x match {
      case s: RefinedSegmentSpec => s.name
      case l: RefinedLoopSpec => l.name
    }

    def nameOfField(x: RefinedSingleFieldSpec | RefinedCompositeFieldSpec): String = x match {
      case s: RefinedSingleFieldSpec => s.name
      case c: RefinedCompositeFieldSpec => c.name
    }
    */
