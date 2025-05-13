package co.blocke.edi4s

import zio.*
import zio.nio.file.{Files, Path}
import table.*
import model.*
import tokenizer.*
import java.io.File
import scala.io.Source

import java.io.FileNotFoundException


object Main extends ZIOAppDefault {

  import CanonicalParser.given

  // TODO: Make this ZIO to handle file read problems
  private def readFileToString(file: File): String =
    val source = Source.fromFile(file)
    try source.mkString
    finally source.close()

  private def readRefined(path: String): ZIO[Any, CanonicalError, RefinedDocumentSpec] =
    val filePath = Path(path)
    for {
      lines <- Files.readAllLines(filePath).mapError {
        case ioe: Throwable => CanonicalError("Can't read file: " + ioe.getMessage)
      }
      refined = sjRefinedSpec.fromJson(lines.mkString("\n"))
    } yield refined

  private def diffReport(
                  srcPartner: String,
                  targetPartner: String,
                  src: RefinedDocumentSpec,
                  edi: RefinedDocumentSpec,
                  target: RefinedDocumentSpec,
                  filterUnused: Boolean = true
                ): Table =
    val result = DiffEngine.compareSpecs(src, edi, target)
    val titles = List(
      Title(List(Cell("📦 EDI Segment Comparison Report"))),
      Title(List(Cell(s"$srcPartner -to- $targetPartner")))
    )
    val header = Header(List(
      Cell(s"Source ($srcPartner)"),
      Cell("Difference"),
      Cell(s"Target ($targetPartner)"),
      Cell("Difference")
    ))
    val rawRows = result.foldLeft(List.empty[BodyRow]) { case (acc, diff) => acc ++ diff.render() }
    val rows = if filterUnused then
      rawRows.filter { row =>
        // keep rows that have at least one cell *not* muted
        row.cells.exists(cell => !cell.style.contains(Style.MUTED))
      }
    else rawRows
    Table(
      title = titles,
      columns = 4,
      columnWidthPct = List(35, 15, 35, 15),
      tableWidth = 200,
      header,
      rows
    )

  def run: ZIO[ZIOAppArgs & Scope, CanonicalError | X12ParseError, Unit] = {

    for {

      _ <- ZIO.succeed("Starting!")
      std <- readRefined("specs/x12_856_5010.json")
      src <- readRefined("specs/tf_856_5010.json")
      pfg <- readRefined("specs/pfg_856_5010.json")
      tj <- readRefined("specs/tj_856_4030.json")
      cm <- readRefined("specs/cm_856_5010.json")

      table1 = diffReport("Taylor Farms", "Core-Mark", src, std, cm, true)
      //_ <- ZIO.succeed(println(table1.toString))

      doc = readFileToString(new File("specs/raw_x12/sample_856.x12"))
      (isa,cfg) <- X12Parser.parse(doc, TokenizerConfig())

      sb = Emitter.emitTransaction(cfg, isa)
      _ <- ZIO.succeed(println(sb.split("~").mkString("\n").toString))

    } yield ()
  }
}