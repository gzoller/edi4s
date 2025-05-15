package co.blocke.edi4s
package parser

import model.*
import mapper.*
import parser.*

import zio.*
import co.blocke.scalajack.*
import java.io.File
import scala.io.Source

object Locator:

  given sjRefinedLoop: ScalaJack[RefinedSingleOrLoopSegmentSpec] = ScalaJack.sjCodecOf[RefinedSingleOrLoopSegmentSpec]

  private def readFileToString(file: File): String =
    val source = Source.fromFile(file)
    try source.mkString
    finally source.close()

  def go: ZIO[Any, Throwable | X12ParseError, Unit] =
    for {
      _ <- ZIO.succeed(println("Starting test..."))
      refSpecFile = readFileToString(new File("test/updated_refined_loop_spec.json"))
      refSpec = sjRefinedLoop.fromJson(refSpecFile)

      x12File = readFileToString(new File("test/sample.x12"))
      (tokens,_) <- X12Tokenizer.tokenize(x12File, 0, TokenizerConfig())

      (hl,rest) = X12ops.extractHLRange(tokens, refSpec.asInstanceOf[RefinedLoopSpec])
      roots = X12ops.extractHLTree(hl)

      flat = X12ops.flattenHLTree(roots.roots,Set("P"))
      _ <- ZIO.succeed{
        println("--- Flattened Record ---")
        flat.foreach { seg =>
          val fields = seg.fields.collect { case s: SimpleX12Token => s.value }.mkString(", ")
          println(s"${seg.name}: $fields")
        }
      }

      sb = new StringBuilder()
      _ = flat.foreach( bodySeg => Emitter.emit(TokenizerConfig(), sb, bodySeg))

//      _ <- ZIO.succeed(println("-----------\n"+flat.mkString("\n\n")))
      _ <- ZIO.succeed(println("-----------\n"+sb.toString.split("~").mkString("~\n")))
    } yield ()

  /*

    1. Direct assignment S->T
    2. AssignOrElse S(opt) -> T  If S is None and T is required then assign "orElse" value
    3. AssignFromContext C(key) -> T
    4. OptToOpt -> S(opt) -> T(opt)
    5. Expression (TBD) - formulaic combinations of elements into a value to be assigned


    >> Entire segment carries over:
    AA -> AA
      {
        "from": "AA"
        "to": "AA"
      }

    >> Entire segment with orElse: (optional segment)
    AA.orElse(01="foo", 02="bar", 05="blather") -> AA
      {
        "from": ["AA","orElse",[["01","=","foo"],["02","=","bar"],["05","=","blather"]]
        "to": "AA"
      }

    >> Assign certain fields (any unlisted are empty)
    AA[01, 02, 05] -> AA
      {
        "from": {
                   "seg":"AA",
                   "fields":["01","02","05"]
                }
        "to": "AA"
      }

    >> Assign certain fields (one with orElse)
    AA[01, 02.orElse("foo"), 05] -> AA
      {
        "from": {
                   "seg":"AA",
                   "fields":["01",["02","orElse","bar"],"05"]
                }
        "to": "AA"
      }

    >> Assign from context (ctx);
    AA[01, 02.ctx(foo), 03] -> AA
      {
        "from": {
                   "seg":"AA",
                   "fields":["01",["02","ctx","foo"],"03"]
                }
        "to": "AA"
      }

    >> Static assignment:
    AA[01, 02="foo", 03] -> AA
      {
        "from": {
                   "seg":"AA",
                   "fields":["01",["02","=","foo"],"03"]
                }
        "to": "AA"
      }

    >> opt-to-opt and req-to-opt shows as simple assignment...no special syntax
    >> req-to-opt is forbidden without .orElse
  */