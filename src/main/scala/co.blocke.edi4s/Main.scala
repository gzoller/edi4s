package co.blocke.edi4s

import co.blocke.edi4s.diff.DiffEngine
import zio.*
import zio.nio.file.{Files, Path}
import table.*
import model.*
import parser.*
import diff.*
import mapper.*

import co.blocke.scalajack.*
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


  private def readJson[T](path: String)(using sj: ScalaJack[T]): ZIO[Any, CanonicalError, T] =
    val filePath = Path(path)
    for {
      lines <- Files.readAllLines(filePath).mapError {
        (ioe: Throwable) => CanonicalError("Can't read file: " + ioe.getMessage)
      }
      json = sj.fromJson(lines.mkString("\n"))
    } yield json


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


  def run: ZIO[ZIOAppArgs & Scope, CanonicalError | X12ParseError | DifferenceError | MappingError | Throwable, Unit] = {

    for {
      _ <- ZIO.succeed("Starting!")
      enums <- readEnumFields
//      _ <- ZIO.succeed(println(enums))
//      _ <- Locator.go

// >> DiffEngine + Table

      // Read json specs
      std <- readRefined("specs/x12_856_5010.json")
      src <- readRefined("specs/tf_856_5010.json")
//      pfg <- readRefined("specs/pfg_856_5010.json")
      tj <- readRefined("specs/tj_856_4030.json")
//      cm <- readRefined("specs/cm_856_5010.json")

      // Demo 1 -- Compute differences in X12 specs and display as table
      diffs <- DiffEngine.compareSpecs(src, std, tj)
      table = DiffReport.asTable("Taylor Farms", "Trader Joe's", diffs, false)
      _ <- ZIO.succeed(println(table.toString))

      rules = MappingSpec(mapper.RuleGenerator.generate(diffs, enums))
      _ <- ZIO.succeed(println("RULES: \n"+ sjAssignment.toJson(rules)))


// >> Emitting X12
      //      doc = readFileToString(new File("test/OUT_ASN_856_TJ.x12"))
      //      doc = readFileToString(new File("specs/raw_x12/sample_856.x12"))
      //      sb = Emitter.emitTransaction(cfg, isa)
      //      _ <- ZIO.succeed(println(sb.split("~").mkString("\n").toString))

      doc = readFileToString(new File("test/foo.x12"))
      (isa,cfg) <- X12Parser.parse(doc, TokenizerConfig())
//      rules <- readJson[MappingSpec]("test/rules.json")
      mapped <- MapRunner.mapWithRules(isa, rules)
      sb2 = Emitter.emitTransaction(cfg, mapped)
      _ <- ZIO.succeed(println(sb2.toString
        .split("~", -1)        // -1 to preserve empty segments between ~~ (like HL*...~~)
        .mkString("~\n")))

    } yield ()


    /*  Parse XML and emit X12
    import tf.*
    given sjXMLObject: ScalaJackXML[Invoice810] = ScalaJack.sjXmlCodecOf[Invoice810]
    given sjObject: ScalaJack[Invoice810] = ScalaJack.sjCodecOf[Invoice810]

    for {
      _ <- ZIO.succeed(println("Starting..."))
      doc = readFileToString(new File("test/PFG SO-2318909 810 Invoice.xml"))
      xml = sjXMLObject.fromXml(doc)
      _ <- ZIO.succeed(println(xml))
    } yield ()
     */
  }
}