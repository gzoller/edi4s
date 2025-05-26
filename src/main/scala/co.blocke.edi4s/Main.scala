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


  private def readEnumFields: ZIO[Any, CanonicalError, Map[String, List[String | EnumeratedDependency]]] =
    ZIO.scoped {
      ZIO.acquireRelease(ZIO.attempt(Source.fromResource("enumerated-fields.json")))(src =>
        ZIO.succeedBlocking(src.close())
      ).flatMap { source =>
        ZIO.attemptBlocking(source.mkString)
      }.flatMap { jsonStr =>
        ZIO.attempt(sjEnumFields.fromJson(jsonStr))
      }.mapError(th => CanonicalError(s"Failed to load enum fields: ${th.getMessage}"))
    }


  def run: ZIO[ZIOAppArgs & Scope, CanonicalError | X12ParseError | DifferenceError | Throwable, Unit] = {

    for {
      _ <- ZIO.succeed("Starting!")
      enums <- readEnumFields
//      _ <- ZIO.succeed(println(enums))
//      _ <- Locator.go

// >> DiffEngine + Table
      std <- readRefined("specs/x12_856_5010.json")
      src <- readRefined("specs/tf_856_5010.json")
//      pfg <- readRefined("specs/pfg_856_5010.json")
      tj <- readRefined("specs/tj_856_4030.json")
//      cm <- readRefined("specs/cm_856_5010.json")

//      table1 = DiffReport.asTable("Taylor Farms", "Trader Joes", src, std, tj, true)
//      _ <- ZIO.succeed(println(table1.toString))

      // Test
//      hlSrc = DiffUtil.getHLLevels( src.segments.find(_.name == "HL").get.asInstanceOf[RefinedLoopSpec] )
//      hlTarget = DiffUtil.getHLLevels( tj.segments.find(_.name == "HL").get.asInstanceOf[RefinedLoopSpec] )
//      _ <- ZIO.succeed(println("Src   : "+hlSrc.mkString(",")))
//      _ <- ZIO.succeed(println("Target: "+hlTarget.mkString(",")))
//      z = List(("Shipment","S"),("Order","O"),("Tare","T"),("Pack","P"),("Item","I"))
//      _ <- ZIO.succeed(println("Target: "+z.mkString(",")))
//      hlRule <- DiffUtil.analyzeHLStructures(hlSrc, z)
//      _ <- ZIO.succeed(println("HL Rule: "+hlRule))
      // --- end test

      diffResult <- DiffEngine.compareSpecs(src, std, tj)
      table = DiffReport.asTable("Taylor Farms", "Trader Joe's", diffResult, true)
      _ <- ZIO.succeed(println(table.toString))
//      rules = mapper.RuleGenerator.generate(diffResult, enums)
//      _ <- ZIO.succeed(println("RULES: \n"+sjAssignment.toJson(MappingSpec(rules))))


// >> Emitting X12
//      doc = readFileToString(new File("specs/raw_x12/sample_856.x12"))
//      (isa,cfg) <- X12Parser.parse(doc, TokenizerConfig())
//
//      sb = Emitter.emitTransaction(cfg, isa)
//      _ <- ZIO.succeed(println(sb.split("~").mkString("\n").toString))

    } yield ()
  }
}