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


  def run: ZIO[ZIOAppArgs & Scope, CanonicalError, Unit] = {

    for {

      _ <- ZIO.succeed(println(co.blocke.edi4j.FileReader.convertSpec("specs/canonical/doc856_v5010.json")))

    } yield ()
  }
}
