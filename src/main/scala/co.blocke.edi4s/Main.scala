package co.blocke.edi4s

import co.blocke.edi4s.diff.DiffEngine
import zio.*
import zio.nio.file.{Files, Path}
import table.*
import model.*
import parser.*
import diff.*

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



  def run: ZIO[ZIOAppArgs & Scope, CanonicalError | X12ParseError | Throwable, Unit] = {

    for {
      _ <- ZIO.succeed("Starting!")
//      _ <- Locator.go

// >> DiffEngine + Table
      std <- readRefined("specs/x12_856_5010.json")
      src <- readRefined("specs/tf_856_5010.json")
//      pfg <- readRefined("specs/pfg_856_5010.json")
      tj <- readRefined("specs/tj_856_4030.json")
//      cm <- readRefined("specs/cm_856_5010.json")

      table1 = DiffReport.asTable("Taylor Farms", "Trader Joes", src, std, tj, true)
      _ <- ZIO.succeed(println(table1.toString))

// >> Emitting X12
//      doc = readFileToString(new File("specs/raw_x12/sample_856.x12"))
//      (isa,cfg) <- X12Parser.parse(doc, TokenizerConfig())
//
//      sb = Emitter.emitTransaction(cfg, isa)
//      _ <- ZIO.succeed(println(sb.split("~").mkString("\n").toString))

    } yield ()
  }
}