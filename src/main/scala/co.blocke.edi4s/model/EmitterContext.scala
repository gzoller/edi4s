package co.blocke.edi4s
package model

enum EdiVersion(val isaCode: String, val gsCode: String):
  case X12_4010 extends EdiVersion("00401", "004010")
  case X12_4030 extends EdiVersion("00403", "004030")
  case X12_5010 extends EdiVersion("00501", "005010")

  override def toString: String = isaCode


val EdiTransaction: Map[Int,String] = Map(
  850 -> "PO",
  810 -> "IN",
  856 -> "SH",
  820 -> "RA",
  997 -> "FA",
  862 -> "CN",
  832 -> "SC",
  889 -> "CO",
  990 -> "GC",
  846 -> "IB",
  304 -> "SI"
)


case class EmitterContext(
                           senderId: String,
                           receiverId: String,
                           repetitionSeparator: Char,
                           interchangeControlVersion: EdiVersion,
                           interchangeControlNumber: Long,
                           ackRequired: Int = 0,
                           usage: Char = 'T', // 'P' for production
                           tokenizerConfig: TokenizerConfig
                         )
