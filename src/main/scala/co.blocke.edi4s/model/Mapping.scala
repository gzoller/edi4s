package co.blocke.edi4s
package model

import co.blocke.scalajack.*


enum Op(val value: String):
  case Eq extends Op("=")
  case OrElse extends Op("orElse")
  case Ctx extends Op("ctx")

enum EqOnly(val value: String):
  case Eq extends EqOnly("=")

type opExpr = (String, Op, String)  // (field operation value)
type topOpExpr = (String, EqOnly, List[opExpr])

case class SpecificFields( seg: String, fields: List[String | opExpr] )

case class Mapping(from: String | SpecificFields | topOpExpr, to: String)

object Stuff:
  val sj = ScalaJack.sjCodecOf[Mapping]