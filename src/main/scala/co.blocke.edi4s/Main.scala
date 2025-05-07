package co.blocke.edi4s

import zio.*
import zio.nio.file.{Files, Path}
import co.blocke.edi4j.table.*
import co.blocke.edi4j.model4j.refined.*
import java.util.List

import java.io.FileNotFoundException


object Main extends ZIOAppDefault {


  def run: ZIO[ZIOAppArgs & Scope, CanonicalError, Unit] = {

    for {

      _ <- ZIO.succeed("Starting!")
      std = co.blocke.edi4j.FileReader.readRefined("specs/x12_856_5010.json")
      src = co.blocke.edi4j.FileReader.readRefined("specs/tf_856_5010.json")
      pfg = co.blocke.edi4j.FileReader.readRefined("specs/pfg_856_5010.json")
      tj = co.blocke.edi4j.FileReader.readRefined("specs/tj_856_4030.json")
      cm = co.blocke.edi4j.FileReader.readRefined("specs/cm_856_5010.json")
      //      _ <- ZIO.succeed(println(co.blocke.edi4j.FileReader.convertSpec("specs/canonical/doc856_v5010.json")))

      table1 = co.blocke.edi4j.DiffReport.genReport("Taylor Farms", "Core-Mark", src, std, cm, true)
      _ <- ZIO.succeed(println(table1.toString))

      //      _ <- ZIO.succeed(println(co.blocke.edi4j.FileReader.readRefined("specs/x12_856_5010.json")))

    } yield ()
  }
}