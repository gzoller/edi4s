package co.blocke.edi4s

import zio._
import co.blocke.edi4s.model._

object EDIEngine {
  def parseEdi(edi: String, docType: String, version: String): IO[String, EDIDocument] =
    (docType, version) match {
      case ("855", "4030") => ZIO.attempt(EDI855_4030.parse(edi)).mapError(_.getMessage)
      case ("855", "5010") => ZIO.attempt(EDI855_5010.parse(edi)).mapError(_.getMessage)
      case _ => ZIO.fail(s"Unsupported docType/version: $docType/$version")
    }

  def generateEdi(doc: EDIDocument, docType: String, version: String): IO[String, String] =
    (docType, version, doc) match {
      case ("855", "4030", d: EDI855) => ZIO.attempt(EDI855_4030.generate(d)).mapError(_.getMessage)
      case ("855", "5010", d: EDI855) => ZIO.attempt(EDI855_5010.generate(d)).mapError(_.getMessage)
      case _ => ZIO.fail(s"Unsupported docType/version or invalid document class")
    }
}
